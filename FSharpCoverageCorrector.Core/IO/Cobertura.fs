/// Provides functions for serializing and deserializing cobertura XML files to and from FSharpCoverage data structures.
module FSharpCoverageCorrector.IO.Cobertura

open System.Linq
open System
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq
open FSharpCoverageCorrector.Core
open FSharpLint.Framework.ParseFile
open FSharpLint.Framework.Rules
open Microsoft.FSharp.Collections

/// Contents loaded from a Cobertura file
type CoberturaContents =
  {
    /// The sources read from a cobertura file
    Sources: string list
    /// The packages read from a cobertura file
    Packages: Package list
    /// A lookup table of class names to the source from which they came
    ClassesToSources: Map<string, string>
  }

/// Formats a float.
let inline private formatFloat (flt: float) =
  Math.Round(flt, 4)
  
/// Merge two maps.
let private mergeMaps m1 m2 =
  Map.fold (fun s k v -> Map.add k v s) m1 m2

/// Parses a line condition element
let private parseConditions (node: XmlNode) (overallConditionCoverage : GenericConditionCoverage) =
  let parseJumpCondition (node: XmlNode) =
    let attributes = node.Attributes
    let percentageVal = Convert.ToDouble(attributes["coverage"].Value.Replace("%", String.Empty))
    { Number = Convert.ToInt32 attributes["number"].Value
      Coverage = ConditionCoverage.Jump(JumpCoverage.FromPercentage(percentageVal)) }
 
  let children = node.ChildNodes.Cast<XmlNode>()
  let jumpConditionNodes, switchConditionNodes = children |> Seq.toList |> List.partition (fun cNode -> cNode.Attributes["type"].Value="jump")
  let jumpConditions = jumpConditionNodes |> List.map parseJumpCondition
  // ensure that all the conditions that aren't jump are actually of type switch
  for sc in switchConditionNodes do
    if sc.Attributes["type"].Value <> "switch" then
      failwithf $"""Unknown condition type %s{sc.Attributes["type"].Value}"""
  // the total number of jumps
  let jumpTotal = if jumpConditions.Length > 0 then
                    jumpConditions
                    |> List.map (fun jc -> (jc :> IConditionCoverage))
                    |> List.reduce (fun c1 c2 -> { NumConditions = c1.NumConditions + c2.NumConditions
                                                   NumCovered = c1.NumCovered + c2.NumCovered })
                  else
                    { NumConditions = 0; NumCovered = 0 }
  // The remaining (non-jump) conditions
  let sumRemainingConditions = { NumConditions = overallConditionCoverage.NumConditions - jumpTotal.NumConditions
                                 NumCovered = overallConditionCoverage.NumCovered - jumpTotal.NumCovered }
  // the switch condition
  let switchConditions = if switchConditionNodes.Length > 0 then
                           let attributes = switchConditionNodes.Head.Attributes
                           [ { Number = Convert.ToInt32 attributes["number"].Value
                               Coverage = ConditionCoverage.Switch sumRemainingConditions } ]
                         else
                           []
  (jumpConditions @ switchConditions) |> List.sortBy (fun c -> c.Number)

/// parses a line element
let private parseLine (fileContents: SourceFile) (node: XmlNode) =
  let attributes = node.Attributes
  let number = attributes["number"].Value |> Convert.ToInt32
  if fileContents.Lines.Length < number then
    failwith $"file {fileContents} expected to have at least $number lines, but only has {fileContents.Lines.Length} lines"
  let contents = fileContents.Lines[number-1]
  let conditions = match node.SelectSingleNode("conditions") with
                   | null -> []
                   | n -> let ccStr = attributes["condition-coverage"].Value
                          let split = ccStr.Replace(" ", String.Empty).Replace("(", String.Empty).Replace(")", String.Empty).Split([|'%'; '/'|])
                          // if the string has no content, set the condition coverage to 0. Otherwise, set it to the parsed values
                          let lineCC = if String.IsNullOrWhiteSpace contents then // note that the line should not be null
                                         { NumCovered=0; NumConditions=0 }
                                        else 
                                         { NumCovered = Convert.ToInt32 split[1]
                                           NumConditions = Convert.ToInt32 split[2] }
                          parseConditions n lineCC
  { Number=number
    Hits=attributes["hits"].Value |> Convert.ToInt32
    Conditions = conditions
    Contents = contents
  }

/// parses a method element
let private parseMethod fileContents (node: XmlNode) =
  let attributes = node.Attributes
  let lines = node.SelectSingleNode("lines").ChildNodes.Cast<XmlNode>()
              |> Seq.map (parseLine fileContents)
              |> Seq.toList
  { Name=attributes["name"].Value
    Signature=attributes["signature"].Value
    Lines=lines
  }
  
/// parses a class element and associates it with the appropriate source file and AST information.
/// Returns a class and the name of the Cobertura source in the path of while the source file in which the class is defined belongs.
let private parseClass (sources: string list) (sourceFiles: SourceFile list) (node: XmlNode) =
  let attributes = node.Attributes
  let className = attributes["name"].Value
  let classFilename = attributes["filename"].Value
  let sourceFileCandidates = sources
                             |> List.map (fun src ->
                                            let candidatePath = if src.Length = 0 || src[src.Length-1]<>'/' then
                                                                  src + "/" + classFilename
                                                                else src + classFilename
                                            sourceFiles
                                            |> List.filter (fun sf -> sf.Filename = candidatePath)
                                            |> List.map (fun sf -> sf, src)
                                          )
                             |> List.concat
  if List.isEmpty sourceFileCandidates then
    failwith $"unable to find loaded source file corresponding to {classFilename} for class {className}"
  else if List.length sourceFileCandidates > 1 then
    failwith $"ambiguous choice for source files from which {classFilename} could originate: {String.Join(';', sourceFileCandidates)}"
  // else
  let sourceFile, source = sourceFileCandidates.Head // sourceFileCandidates guaranteed to have a single element at this point
  let methods = node.SelectSingleNode("methods").ChildNodes.Cast<XmlNode>()
              |> Seq.map (parseMethod sourceFile)
              |> Seq.toList
  let lines = node.SelectSingleNode("lines").ChildNodes.Cast<XmlNode>()
              |> Seq.map (parseLine sourceFile)
              |> Seq.toList
  // return the class and the source
  let c = Class.Create(attributes["name"].Value, sourceFile, methods, lines)
  c, source
  
/// parses a package element and associates it with the appropriate source file and AST information.
let private parsePackage (sources : string list) (sourceFiles: SourceFile list) (ruleParams: Map<SourceFile, AstNodeRuleParams list>) (node: XmlNode) = 
  let classesAndSources = node.SelectSingleNode("classes").ChildNodes.Cast<XmlNode>()
                          |> Seq.map (parseClass sources sourceFiles)
                          |> Seq.toList
  let classes = classesAndSources |> List.map fst
  let classFilenames = classes |> List.map (fun c -> c.SourceFile.Filename) |> Set
  let relevantRuleParams = ruleParams
                           |> Map.filter (fun f _ -> classFilenames.Contains(f.Filename))
  let classToSourceLookup = classesAndSources
                            |> List.map (fun (c, s) -> c.Name, s)
                            |> Map
  let package = { Name=node.Attributes["name"].Value; Classes=classes; RuleParams = relevantRuleParams }
  package, classToSourceLookup

/// Reads F# packages from a list of source files.
/// Returns a list of packages and a map from class names to their sources (sources as loaded from the Cobertura XML, not filenames).
let private readPackages (sources: string list) (sourceFiles: SourceFile list) (ruleParams: Map<SourceFile, AstNodeRuleParams list>) (doc: XmlDocument) =
  let de = doc.DocumentElement
  // read the packages
  de.SelectSingleNode("packages").ChildNodes.Cast<XmlNode>()
  |> Seq.map (parsePackage sources sourceFiles ruleParams)
  |> Seq.fold (fun (pkgList, classesToSourcesLookup) (pkg, m) ->
                pkg::pkgList, mergeMaps classesToSourcesLookup m
              )
              (List.empty, Map.empty)
  
/// Reads the sources element from a cobertura XML document.
let private readSources (doc: XmlDocument) =
  doc.DocumentElement.SelectSingleNode("sources").ChildNodes.Cast<XmlNode>()
  |> Seq.map (fun node -> node.InnerText)
  |> Seq.toList

/// <summary>
/// Reads the code coverage information from packages contained within an XML cobertura file
/// and associates them with the AST nodes loaded from the source files.
/// </summary>
/// <param name="filesInfo">Parse information loaded from F# source files.</param>
/// <param name="ruleParams">A map of source files to <c>AstNodeRuleParams</c> belong in those files.</param>
/// <param name="filename">The XML cobertura file.</param>
/// <returns>A <c>Result</c> type containing either a <c>CoberturaContents</c>, if successful, or an exception if a failure occurred.</returns>
let readCoberturaFileContents (filesInfo: FileParseInfo list) (ruleParams: Map<SourceFile, AstNodeRuleParams list>) (filename: string) =
  let sourceFiles = filesInfo |> List.map (fun fpi -> { Filename = fpi.File
                                                        RelativePath = fpi.File
                                                        Lines = fpi.Text.Split([|"\r\n"; "\n"; "\r"|], StringSplitOptions.None) })
  try
    let doc = XmlDocument()
    doc.Load(filename)
    let sources = readSources doc
    let packages, classesToSourcesLookup = readPackages sources sourceFiles ruleParams doc
    {
      Sources = sources
      Packages = packages
      ClassesToSources = classesToSourcesLookup
    } |> Ok
  with ex -> Error ex
  
/// Serializes a LineCondition to a Cobertura Condition element.
let private serializeCondition (lc: LineCondition) =
  let cType = match lc.Coverage with
              | Jump _ -> "jump"
              | Switch _ -> "switch"
  XElement ("condition",
             XAttribute("number", lc.Number),
             XAttribute("type", cType),
             XAttribute("coverage", coverageAsPercentage (lc.Coverage :> IConditionCoverage) |> formatFloat))
  
/// Serializes a Line to a Cobertura Line element.
let private serializeLine (line: Line) =
  // function to get the condition coverage as a string containing the percentage
  let conditionCovStr (cc: IConditionCoverage) =
    $"{coverageAsPercentage cc |> formatFloat}%% ({cc.NumCovered}/{cc.NumConditions})"
  
  let branch = line.Conditions.Length > 0
  let elem = XElement ("line",
               XAttribute("number", line.Number),
               XAttribute("hits", line.Hits),
               XAttribute("branch", branch))
  if branch then
    elem.Add(XAttribute("condition-coverage", conditionCovStr (line.ConditionCoverage :> IConditionCoverage)))
    elem.Add(XElement("conditions", line.Conditions |> List.map serializeCondition |> List.toArray))
  elem

/// Serializes a Method to a Cobertura Method element.
let private serializeMethod (m: Method) =
  let elem = XElement("method",
    XAttribute("name", m.Name),
    XAttribute("signature", m.Signature),
    XAttribute("line-rate", m.LineCoverage |> formatFloat),
    XAttribute("branch-rate", m.ConditionCoverageRatio |> formatFloat),
    XAttribute("complexity", m.CyclomaticComplexity))
  if m.Lines.Length > 0 then
    elem.Add(XElement("lines", m.Lines |> List.map serializeLine |> List.toArray))
  elem

/// Serializes a Class to Cobertura Class element.
let private serializeClass (source: string) (c: Class) =
  let regex = Regex(Regex.Escape(source))
  let filename = regex.Replace(c.SourceFile.Filename, "", 1)
  let elem = XElement("class",
    XAttribute("name", c.Name),
    XAttribute("filename", filename),
    XAttribute("line-rate", c.LineCoverage |> formatFloat),
    XAttribute("branch-rate", (coverageRatio c.ConditionCoverage |> formatFloat)),
    XAttribute("complexity", c.CyclomaticComplexity))
  if c.Methods.Length > 0 then
    elem.Add(XElement("methods", c.Methods |> List.map serializeMethod |> List.toArray))
  if c.Lines.Length > 0 then
    elem.Add(XElement("lines", c.Lines |> List.map serializeLine |> List.toArray))
  elem
  
/// Serializes a Source to Cobertura source element.
let private serializeSource (source: string) =
  let elem = XElement("source")
  elem.SetValue source
  elem
    
/// Serializes a Package to Cobertura Package element.
let private serializePackage (p: Package) (classesToSources: Map<string, string>) =
  let elem = XElement("package",
    XAttribute("name", p.Name),
    XAttribute("line-rate", p.LineCoverage |> formatFloat),
    XAttribute("branch-rate", coverageRatio p.ConditionCoverage |> formatFloat),
    XAttribute("complexity", p.CyclomaticComplexity))
  if p.Classes.Length > 0 then
    elem.Add(XElement("classes", p.Classes |> List.map (fun c -> serializeClass classesToSources[c.Name] c) |> List.toArray))
  elem

/// <summary>
/// Generates a Cobertura 'coverage' XML element containing coverage information for all the provided packages. 
/// </summary>
/// <param name="contents">The contents of the cobertura file to be included in the returned element.</param>
/// <returns>a Cobertura 'coverage' XML element.</returns>
let generateXml (contents: CoberturaContents) =
  let packages = contents.Packages
  let linesValid = packages |> List.sumBy (fun p -> List.sumBy (fun (c: Class) -> c.Lines.Length) p.Classes)
  let linesCovered = packages |> List.sumBy (fun p -> List.sumBy (fun (c: Class) -> int(c.LineCoverage * float c.Lines.Length)) p.Classes)
  let lineCoverage = (float linesCovered) / (float linesValid)
  let branchesCovered = packages |> List.sumBy (fun p -> (p.ConditionCoverage :> IConditionCoverage).NumCovered)
  let branchesValid = packages |> List.sumBy (fun p -> (p.ConditionCoverage :> IConditionCoverage).NumConditions)
  let branchCoverage = (float branchesCovered) / (float branchesValid)
  let coverageElem = XElement("coverage",
    XAttribute("line-rate", lineCoverage |> formatFloat),
    XAttribute("branch-rate", branchCoverage |> formatFloat),
    XAttribute("version", 1.9),
    XAttribute("timestamp", DateTimeOffset.UtcNow.ToUnixTimeSeconds()),
    XAttribute("lines-covered", linesCovered),
    XAttribute("lines-valid", linesValid),
    XAttribute("branches-covered", branchesCovered),
    XAttribute("branches-valid", branchesValid))
  let sourcesElem = XElement "sources"
  for source in contents.Sources do
    sourcesElem.Add(serializeSource source)
  coverageElem.Add sourcesElem
  let packagesElem = XElement "packages"
  for pkg in packages do
    packagesElem.Add(serializePackage pkg contents.ClassesToSources)
  coverageElem.Add packagesElem
  coverageElem
      