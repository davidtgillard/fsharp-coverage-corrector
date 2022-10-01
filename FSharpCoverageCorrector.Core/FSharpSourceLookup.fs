namespace FSharpCoverageCorrector.Core

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

/// Provides functions and types for cross-referencing lines of code with AST nodes.
[<AutoOpen>]
module internal FSharpSourceLookup =
  
  /// Provides a lookup table from line numbers to AST nodes for a source file.
  type SourceFileLineLookup =
    {
      /// The source file.
      File: SourceFile
      /// The lookup table from line numbers to AST nodes.
      LineLookup: Map<int, AstNodeRuleParams list>
    }
    
  /// Returns the ranges associated with a given node.
  let private getRanges node =
    match node with
    | Expression synExpr -> [synExpr.Range]
    | Pattern synPat -> [synPat.Range]
    | SimplePattern synSimplePat -> match synSimplePat with
                                    | SynSimplePat.Attrib (_, _, range) -> [range]
                                    | SynSimplePat.Id (_, _, _, _, _, range) -> [range]
                                    | SynSimplePat.Typed (_, _, range) -> [range]
    | SimplePatterns synSimplePats -> match synSimplePats with
                                      | SynSimplePats.Typed (_, _, range) -> [range]
                                      | SynSimplePats.SimplePats (_, range) -> [range]
    | ModuleOrNamespace synModuleOrNamespace -> [synModuleOrNamespace.Range]
    | ModuleDeclaration synModuleDecl -> [synModuleDecl.Range]
    | Binding synBinding -> [synBinding.RangeOfBindingWithRhs]
    | TypeDefinition synTypeDefn -> [synTypeDefn.Range]
    | MemberDefinition synMemberDefn -> [synMemberDefn.Range]
    | ComponentInfo synComponentInfo -> [synComponentInfo.Range]
    | ExceptionRepresentation synExceptionDefnRepr -> [synExceptionDefnRepr.Range]
    | UnionCase synUnionCase -> [synUnionCase.Range]
    | EnumCase synEnumCase -> [synEnumCase.Range]
    | TypeRepresentation synTypeDefnRepr -> [synTypeDefnRepr.Range]
    | TypeSimpleRepresentation synTypeDefnSimpleRepr -> [synTypeDefnSimpleRepr.Range]
    | Type synType -> [synType.Range]
    | Field synField -> match synField with
                        | SynField.SynField (_, _, _, _, _, _, _, range) -> [range]
    | Match synMatchClause -> [synMatchClause.Range]
    | ConstructorArguments synArgPats -> match synArgPats with
                                         | SynArgPats.Pats pats -> pats |> List.map (fun p -> p.Range) 
                                         | SynArgPats.NamePatPairs (_, range) -> [range]
    | TypeParameter synTypar -> [synTypar.Range]
    | InterfaceImplementation synInterfaceImpl -> match synInterfaceImpl with
                                                  | SynInterfaceImpl.SynInterfaceImpl (_, _, range) -> [range]
    | Identifier (_, range) -> [range]
    | File parsedInput -> [parsedInput.Range]
    | LambdaBody synExpr -> [synExpr.Range]
    | LambdaArg synSimplePats -> match synSimplePats with
                                 | SynSimplePats.Typed (_, _, range) -> [range]
                                 | SynSimplePats.SimplePats (_, range) -> [range]
    | Else synExpr -> [synExpr.Range]
     
  /// Builds a lookup table from line numbers to AST nodes.
  let private buildLineLookupForFile (nodes: AstNodeRuleParams list) =
    nodes 
    |> List.map (fun anrp ->
                  getRanges(anrp.AstNode)
                  |> List.map (fun rng -> rng.StartLine, anrp))
    |> List.concat
    |> List.groupBy fst
    |> List.map (fun (startLine, paramsList) -> startLine, paramsList |> List.map snd)
    |> Map.ofList                 
  
  /// builds the source file line lookups for a provided package.  
  let buildLineLookup (package: Package) =
    package.RuleParams
    |> Map.toList
    |> List.map (fun (k, nodes) -> {File = k
                                    LineLookup = (buildLineLookupForFile nodes) })

