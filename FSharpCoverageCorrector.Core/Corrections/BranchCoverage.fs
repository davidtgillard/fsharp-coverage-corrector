/// <summary>
///  Provides a function to correct the branch coverage of an F# package.
/// </summary>
[<AutoOpen>]
module FSharpCoverageCorrector.Core.Corrections.BranchCoverage

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpCoverageCorrector.Core
  
/// The scope of a binding. 
type private BindingScope =
  {
   /// The Node corresponding to the binding.
   Node: AstNode
   /// The binding of the node
   Binding: SynBinding
   /// the cyclomatic complexity of the binding scope.
   Complexity: int
  }
  
/// Contains an AstNodeRuleParams with the number of branches within it.
type NodeBranches =
  {
    /// The AstNodeRuleParams.
    Node: AstNodeRuleParams
    /// The number of branches within the node.
    NumBranches: int
  }
      
/// Determines the number of cases in a match clause.
let private countCasesInMatchClause (clause: SynMatchClause) =
  // recursive function to count the number of cases in a pattern.
  let rec countCases (pat: SynPat) (count: int) =
      let mutable localSoFar = count + 1
      match pat with
      | SynPat.Or (lhs, _, _) ->
          countCases lhs localSoFar
      | _ -> localSoFar
  // apply countCases to the given clause.
  match clause with 
  | SynMatchClause(pat, _, _, _, _) -> countCases pat 0

/// Returns the number of boolean operators in an expression.
/// Returns the number of boolean operators in an expression.
/// If expression is Match, MatchLambda, or MatchBang, the 'when' expressions of the match clauses are examined for boolean operators, if applicable.
let private countBooleanOperators expression =
  /// Boolean operator functions.
  let boolFunctions = Set.ofList ["op_BooleanOr"; "op_BooleanAnd"]
  let rec countOperators count = function
  | SynExpr.App(_, _, expr, SynExpr.Ident(ident), _)
  | SynExpr.App(_, _, SynExpr.Ident(ident), expr, _) ->
    if Set.contains ident.idText boolFunctions then
      countOperators (count + 1) expr
    else
      countOperators count expr
  | SynExpr.App(_, _, expr, expr2, _) ->
    let left = countOperators 0 expr
    let right = countOperators 0 expr2
    count + left + right
  | SynExpr.Paren(expr, _, _, _) ->
    countOperators count expr
  | SynExpr.Ident ident ->
    if Set.contains ident.idText boolFunctions then
      count + 1
    else
      count
  // in match and match-like expressions, consider the when expressions of any clauses
  | SynExpr.MatchBang(_, _, clauses, _)
  | SynExpr.MatchLambda(_, _, clauses, _, _)
  | SynExpr.Match(_, _, clauses, _) ->
    clauses |> List.sumBy (fun c -> 
                                    match c with
                                    | SynMatchClause(_, whenExprOpt, _, _, _) ->
                                      match whenExprOpt with
                                        | Some whenExpr ->
                                          countOperators 0 whenExpr
                                        | None -> 0)
             
  | _ -> count
  // kick off the calculation
  countOperators 0 expression

// determine if the node is a duplicate of a node in the AST containing ExtraSyntaxInfo (e.g. lambda arg being a duplicate of the lambda itself)
let private isMetadata (args: AstNodeRuleParams) =
  let parentIndex = args.SyntaxArray[args.NodeIndex].ParentIndex
  if parentIndex = args.NodeIndex then
    false
   else
    Object.ReferenceEquals(args.AstNode, args.SyntaxArray[parentIndex].Actual)

/// Calculates the number of branches of an AstNodeRuleParams.
let calculateBranches (args: AstNodeRuleParams) =
  let node = args.AstNode
  if not (isMetadata args) then
    // if not metadata, match the node against an expression which increments the complexity
    match node with
    | AstNode.Expression expression ->
      match expression with
      | SynExpr.While(_, condition, _, _) ->
        (countBooleanOperators condition) * 2 // include the number of boolean operators in the while condition
      | SynExpr.IfThenElse(condition, _, _, _, _, _, _) ->
        (1 + countBooleanOperators condition) * 2 // include the number of boolean operators in the condition
      | SynExpr.MatchBang(_, _, clauses, _)
      | SynExpr.MatchLambda(_, _, clauses, _, _)
      | SynExpr.Match(_, _, clauses, _) ->
          let numCases = clauses |> List.sumBy countCasesInMatchClause // determine the number of cases in the match expression 
          numCases + (countBooleanOperators expression) * 2 // include the number of boolean operators in any when expressions, if applicable
      | SynExpr.TryWith(_, _, cases, _, _, _, _) ->
          let numCases = cases |> List.sumBy countCasesInMatchClause
          numCases + (countBooleanOperators expression) * 2 // include the number of boolean operations in any case, if applicable
      | _ -> 0
    | _ -> 0
  else
    0 
   
/// Builds a lookup table of line numbers to NodeBranches
let private buildLineLookupToNumBranches (lineLookup: Map<int, AstNodeRuleParams list>) =
  lineLookup
  |> Map.map (fun _ ruleParamsList ->
                let updatedParams = ruleParamsList
                                    |> List.map (fun anrp -> { Node = anrp; NumBranches = calculateBranches anrp })
                                    |> List.filter (fun x -> x.NumBranches > 0)
                updatedParams)
  
/// Builds a lookup table of source files to maps of line lookups to branches
let private buildBranchCoverageLineLookup (l: SourceFileLineLookup list) =
  l
  |> List.map (fun sfll -> sfll.File, buildLineLookupToNumBranches sfll.LineLookup)
  |> Map.ofList

/// Returns true if the node is a type of expression that can have cases (Match, MatchBang, MatchLambda, TryWith)
let private hasCases node =
   match node with
   | AstNode.Expression expression ->
     match expression with
     | SynExpr.MatchBang _ | SynExpr.MatchLambda _ | SynExpr.Match _ | SynExpr.TryWith _ -> true
     | _ -> false
   | _ -> false

/// Resolves the number of branches in a line to match the correct number of branches.
let private resolveBranches (line: Line) (nodeBranches: NodeBranches list) =
  let correctedNumBranches = nodeBranches |> List.sumBy (fun n -> n.NumBranches) // get the number of conditions met
  // if there are no conditions, nothing to do
  if line.Conditions.Length = 0 then
    line
  // If there is only one nodeBranch and it of the type of expression that has cases (Match, MatchBang, MatchLambda, TryWith), we have to handle the case where an odd number of cases exist, which cannot be met with Jump statements
  else if nodeBranches.Length = 1 && hasCases nodeBranches[0].Node.AstNode then
    let numCases = nodeBranches[0].NumBranches // the number of cases is the number of branches
    let numCasesCovered = line.ConditionCoverage.NumCovered // the number of cases covered is the number of covered conditions
    // create a fake switch condition, using the ID from the first line condition
    let switchCondition : LineCondition = { Number =  line.Conditions[0].Number 
                                            Coverage = ConditionCoverage.Switch { NumConditions = numCases; NumCovered = min numCases numCasesCovered } }
    { line with Conditions = [switchCondition] }
  // otherwise, if the corrected number of branches is less than the number of conditions currently included in the line
  else if correctedNumBranches < line.ConditionCoverage.NumConditions then
    let mutable numConditionsCounter = 0 
    // todo: this should be a bit more intelligent about fitting the selection of conditions to the expected coverage.
    // add all the jump coverages first
    // first, sort by numCovered and condition
    let conditions = line.Conditions
                     |> List.sortBy (fun c -> let icc = c :> IConditionCoverage
                                              icc.NumCovered, icc.NumConditions)
                     |> List.takeWhile (fun c -> let icc = c :> IConditionCoverage
                                                 if numConditionsCounter < correctedNumBranches then
                                                   numConditionsCounter <- numConditionsCounter + icc.NumConditions
                                                   true
                                                 else
                                                   false)
    let overshot = numConditionsCounter - correctedNumBranches // the number of extra branches that aren't valid
    let updatedConditions = if overshot > 0 then
                              // get the last condition in the list, since the takeWhile above ensures that it is the last one that pushes the number of branches over
                              let last = List.last conditions
                              let numConditionsPriorToLast = conditions[0..conditions.Length-2] |> List.sumBy (fun c -> (c.Coverage :> IConditionCoverage).NumConditions)
                              let remainingConditions = correctedNumBranches - numConditionsPriorToLast

                              match last.Coverage with
                              | Jump _-> conditions // can't split the two branches of a jump condition - we can't reduce the number of conditions any further
                              | Switch sc -> // if the last condition is a switch condition, we assume we can reduce the number of conditions arbitrarily. Otherwise, use the jump condition.
                                let newSC = { NumConditions = remainingConditions; NumCovered = (min sc.NumCovered remainingConditions) }
                                let newLast = { last with Coverage=Switch newSC }
                                conditions[0..conditions.Length-2]@[newLast]
                            else
                              conditions
    { line with Conditions = updatedConditions }
  else
    line

/// Given a lookup table of line numbers to NodeBranches, return a new method with the corrected branch coverage
let private correctMethod (lookup: Map<int, NodeBranches list>) (method: Method) =
  let lines = method.Lines
              |> List.map (fun l ->
                            match Map.tryFind l.Number lookup with
                            | Some nodeBranches -> resolveBranches l nodeBranches
                            | None -> l)
  { method with Lines = lines }
  
/// Given a lookup table of line numbers to NodeBranches, return a new class with the corrected branch coverage
let private correctClass (lookup: Map<int, NodeBranches list>) (origClass: Class) (sourceFile: SourceFile) =
  let updatedMethods = origClass.Methods
                       |> List.map (correctMethod lookup)
  Class.Create(origClass.Name, sourceFile, updatedMethods, origClass.NonMethodLines)

// todo: use an error type instead of a list of classes for the errors about uncorrected classes
/// <summary>
/// Given a lookup table of line numbers to NodeBranches, correct the branch coverage of the method
/// </summary>
/// <param name="package">The package to be corrected.</param>
/// <returns>The corrected package, and a list of classes which could not be corrected due to missing source file information.</returns>
let correctBranchCoverage (package: Package) =
  let lineLookup = buildBranchCoverageLineLookup (buildLineLookup package)
  let classesWithSourceFiles, classesWithoutSourceFiles = package.Classes |> List.partition (fun cls -> cls.SourceFile.IsSome)
  let updatedClasses = classesWithSourceFiles
                       |> List.map (fun cls ->
                                        let sourceFile = cls.SourceFile.Value
                                      // we know the class has a source file at this point
                                        let perFileLookup = match Map.tryFind sourceFile lineLookup with
                                                            | Some found -> found
                                                            | None -> failwith $"source file {cls.SourceFile} not found within branch coverage line lookup table"
                                        correctClass perFileLookup cls sourceFile)
  let errors =
    classesWithoutSourceFiles
    |> List.map (fun c -> $"No source file {c.Filename} loaded for {c.Name}")
  { package with Classes = updatedClasses @ classesWithoutSourceFiles }, errors

  