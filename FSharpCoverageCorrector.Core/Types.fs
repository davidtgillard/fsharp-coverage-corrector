/// Contains basic types.
[<AutoOpen>]
module FSharpCoverageCorrector.Core.Types

open System
open System.Collections.Generic
open System.Linq
open FSharpLint.Framework.Rules

/// A common interface for a number of conditions.
type IConditionCoverage =
  /// The number of conditions.
  abstract NumConditions: int
  /// The number of conditions that are covered.
  abstract NumCovered: int

/// A general-purpose condition coverage.
type GenericConditionCoverage =
  {
    /// The number of conditions.
    NumConditions: int
    /// The number of conditions that are covered.
    NumCovered: int
  }
  
  interface IConditionCoverage with
    member this.NumConditions = this.NumConditions
    member this.NumCovered = this.NumCovered
  
/// <summary>Determines the coverage ratio of an <c>IConditionCoverage</c>.</summary>
/// <param name="coverage">The <c>IConditionCoverage</c> for which to report the coverage ratio.</param>
/// <returns>The coverage ratio. If the number of conditions is 0, then a ratio of 1.0 is reported.</returns>
let coverageRatio (coverage: IConditionCoverage) =
  if coverage.NumConditions = 0 then
    1.0
  else 
    float coverage.NumCovered / float coverage.NumConditions

/// <summary>Determines the coverage ratio of an <c>IConditionCoverage</c> as a percentage.</summary>
/// <param name="coverage">The <c>IConditionCoverage</c> for which to report the coverage ratio.</param>
/// <returns>The coverage, as a percentage. If the number of conditions is 0, then a ratio of 1.0 is reported.</returns>
let coverageAsPercentage (coverage: IConditionCoverage) =
  coverageRatio coverage * 100.0


/// Returns a condition coverage which holds the sum of all conditions and sum of of all covered conditions
let private overallConditionCoverage (conditions: IEnumerable<IConditionCoverage>) =
  Seq.fold (fun acc (cond: IConditionCoverage) -> {NumConditions= acc.NumConditions + cond.NumConditions; NumCovered = acc.NumCovered + cond.NumCovered })
    {NumConditions=0; NumCovered=0}
    conditions

/// Coverage for a jump condition.
type JumpCoverage =
  /// Neither branch is covered.
  | Zero
  /// One branch is covered.
  | One
  /// Both branches are covered.
  | Both
  
  /// <summary>Given a percentage, returns the corresponding <c>JumpCoverage</c> case.</summary>
  /// <param name="percentage">The percentage value, which must be one of {0.0; 50.0; 100.0}.</param>
  /// <returns>The corresponding <c>JumpCoverage</c> case.</returns>
  /// <exception cref="Exception">Thrown if <c>percentage</c> is not one of the acceptable values.</exception>
  static member FromPercentage(percentage: float) =
    if percentage = 0.0 then
      JumpCoverage.Zero
    else if percentage = 50.0 then
      JumpCoverage.One
    else if percentage = 100.0 then
      JumpCoverage.Both
    else
      raise (ArgumentOutOfRangeException(nameof percentage, $"Unexpected coverage percentage {percentage}: must be 0, 0.5, or 1.0"))
  
  interface IConditionCoverage with
    member this.NumConditions = 2
    member this.NumCovered = match this with
                             | Zero -> 0
                             | One -> 1
                             | Both -> 2
                
/// Coverage for a switch condition.        
type SwitchCoverage = {
    /// The number (ID) of the condition.
    Number: int
    /// The coverage of the condition.
    Coverage: GenericConditionCoverage }

/// The contents of the source file.
type SourceFile =
  {
    /// the full name of the source file.
    Filename: string
    /// the relative path of the source file within the project folder.
    RelativePath: string
    /// The lines of the source file.
    Lines: string[]
  }

/// Various types of condition coverage.
type ConditionCoverage =
  /// Condition coverage for a jump instruction.
  | Jump of JumpCoverage
  /// Condition coverage for a switch instruction.
  | Switch of GenericConditionCoverage
  
  interface IConditionCoverage with
    member this.NumConditions = match this with
                                | Jump j -> (j :> IConditionCoverage).NumConditions
                                | Switch s -> s.NumConditions
    member this.NumCovered = match this with
                             | Jump j -> (j :> IConditionCoverage).NumCovered
                             | Switch s -> s.NumCovered

/// Conditional coverage for a line of code.
type LineCondition =
  {
    /// The number (ID) of the coverage associated with the line.
    Number: int
    /// The coverage information.
    Coverage: ConditionCoverage
  }
  
  interface IConditionCoverage with
    member this.NumConditions = (this.Coverage :> IConditionCoverage).NumConditions
    member this.NumCovered = (this.Coverage :> IConditionCoverage).NumCovered
  
/// A line of code with associated coverage metrics.
type Line =
  {
    /// The number of the line of code.
    Number: int
    /// The number of times the line of code was visited.
    Hits: int
    /// The conditions of the line.
    Conditions: LineCondition list
    /// The contents of the line, if available.
    Contents: string option
  }
  
  /// The condition coverage of the line.
  member this.ConditionCoverage =
    overallConditionCoverage (this.Conditions.Cast<IConditionCoverage>())
       
/// Lines of source code and metadata for a method.                                         
type Method =
  {
    /// The name of the method.
    Name: string
    /// The signature of the method.
    Signature: string
    /// The lines of source code for the method.
    Lines: Line list
  }
  
  /// The line coverage of the method.
  member this.LineCoverage =
    let numLinesWithHits = this.Lines |> List.filter (fun l -> l.Hits > 0) |> List.length
    float(numLinesWithHits) / float(this.Lines.Length)
  
  /// The condition coverage of the method.
  member this.ConditionCoverage =
    let coveragePerLines = this.Lines |> List.map (fun l -> l.ConditionCoverage)
    overallConditionCoverage (coveragePerLines.Cast<IConditionCoverage>())
  
  /// The condition coverage ratio of the method.
  member this.ConditionCoverageRatio =
    // if the line coverage is 0 and there is at least one line, the condition coverage ratio is 0
    if this.LineCoverage = 0.0 && this.Lines.Length > 0 then
      0.0           
    else
      coverageRatio (this.ConditionCoverage :> IConditionCoverage)
      
  /// The cyclomatic complexity of the method.
  member this.CyclomaticComplexity =
    (this.Lines |> List.sumBy (fun l -> l.Conditions.Length)) + 1
    

type private SourceFileData =
  | Contents of SourceFile
  | NoContents of Filename: string
  
  member this.Filename =
    match this with
    | Contents sf -> sf.Filename
    | NoContents filename -> filename
  
/// lines of source code and metadata for a class.
type Class = private {
    /// The name of the class.
    name_ : string
    /// The source file of the class, if available.
    sourceFileData_: SourceFileData
    /// The methods of the class.
    methods_: Method list
    /// All lines of code within the class, including those belong to methods
    lines_: Line list
    /// lines that don't belong to any method
    nonMethodLines_: Line list
  } with

  /// The name of the file.
  member this.Filename = this.sourceFileData_.Filename
  /// The name of the class.
  member this.Name = this.name_
  /// The source file of the class.
  member this.SourceFile =
    match this.sourceFileData_ with
    | Contents data -> Some data
    | NoContents _ -> None
  /// The methods of the class.
  member this.Methods = this.methods_
  /// All lines of code within the class, including those belong to methods.
  member this.Lines = this.lines_
  
  /// <summary>
  /// Creates a class.
  /// </summary>
  /// <remarks>
  /// <c>nonMethodLines</c> will be compared against the lines of <c>methods</c> with lines belonging to methods removed.
  /// Where there is overlap in lines provided in <c>methods</c> and <c>nonMethodLines</c>, lines provided in <c>methods</c>
  /// will take precedence.
  /// </remarks>
  /// <param name="name">The name of the class.</param>
  /// <param name="sourceFile">The source file.</param>
  /// <param name="methods">The methods of the class.</param>
  /// <param name="nonMethodLines">The non-method lines of the class.</param>
  static member Create(name, sourceFile, methods, nonMethodLines) =
    let methodLines = methods
                      |> List.map (fun m -> m.Lines)
                      |> List.concat |> List.distinct
    let reduced = nonMethodLines
                  |> List.filter (fun l -> not (List.contains l methodLines))
    let allLines = methodLines @ reduced |> List.sortBy (fun l -> l.Number)
    {name_ = name; sourceFileData_ = SourceFileData.Contents sourceFile; methods_ = methods; lines_ = allLines; nonMethodLines_ = reduced}
    
  /// <summary>
  /// Creates a class.
  /// </summary>
  /// <remarks>
  /// <c>nonMethodLines</c> will be compared against the lines of <c>methods</c> with lines belonging to methods removed.
  /// Where there is overlap in lines provided in <c>methods</c> and <c>nonMethodLines</c>, lines provided in <c>methods</c>
  /// will take precedence.
  /// </remarks>
  /// <param name="name">The name of the class.</param>
  /// <param name="filename">The filename of the source file to which the class belongs.</param>
  /// <param name="methods">The methods of the class.</param>
  /// <param name="nonMethodLines">The non-method lines of the class.</param>
  static member Create(name, filename, methods, nonMethodLines) =
    let methodLines = methods
                      |> List.map (fun m -> m.Lines)
                      |> List.concat |> List.distinct
    let reduced = nonMethodLines
                  |> List.filter (fun l -> not (List.contains l methodLines))
    let allLines = methodLines @ reduced |> List.sortBy (fun l -> l.Number)
    {name_ = name; sourceFileData_ = SourceFileData.NoContents filename; methods_ = methods; lines_ = allLines; nonMethodLines_ = reduced}
  
  /// The lines of code within the class that don't belong to any methods.
  member this.NonMethodLines = this.nonMethodLines_
  
  /// The line coverage of the class.
  member this.LineCoverage =
    if this.Lines.Length = 0 then
      1.0
    else 
      let numHit = this.Lines |> List.filter (fun l -> l.Hits > 0) |> List.length
      (float numHit) / (float this.Lines.Length)
  
  /// The condition coverage of the class.
  member this.ConditionCoverage =
    overallConditionCoverage (this.Lines |> List.map (fun l -> l.ConditionCoverage))
    
  /// The cyclomatic complexity of the class.
  member this.CyclomaticComplexity =
    let cc = this.ConditionCoverage.NumConditions + this.Methods.Length
    min cc 1 // complexity must be at least 1

/// The lines of source code and metadata for a package.
type Package =
  {
    /// The name of the package.
    Name: string
    /// The classes within the package.
    Classes: Class list
    /// The rule params associated with this package.
    RuleParams: Map<SourceFile, AstNodeRuleParams list>
  }
  
  /// The line coverage of the package.
  member this.LineCoverage =
    let numLines = this.Classes |> List.sumBy (fun c -> c.Lines.Length)
    if numLines = 0 then
      1.0
    else 
      let numCovered = this.Classes |> List.sumBy (fun c -> (List.filter (fun l -> l.Hits > 0) c.Lines) |> List.length)
      (float numCovered) / (float numLines)
  
  /// The condition coverage of the package.
  member this.ConditionCoverage =
    overallConditionCoverage (this.Classes |> List.map (fun c -> c.ConditionCoverage))
  
  /// The cyclomatic complexity of the package.
  member this.CyclomaticComplexity =
    this.Classes |> List.sumBy (fun c -> c.CyclomaticComplexity)