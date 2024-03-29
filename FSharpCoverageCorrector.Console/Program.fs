open System
open System.Collections.Generic
open Argu
open FSharpLint.Framework.Ast
open FSharpCoverageCorrector.Core
open FSharpCoverageCorrector.Core.Corrections
open FSharpCoverageCorrector.IO.Cobertura
open FSharpLint.Framework.ParseFile
open Microsoft.FSharp.Core
  
type CliArguments =
  | [<ExactlyOnce>] Coverage_File of coverage_file: string
  | [<Mandatory>] Project_File of project_file: string
  | [<ExactlyOnce>] Output_File of output_file: string
  
  with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Coverage_File _ -> "The input coverage file, in Cobertura XML format."
            | Project_File _ -> "An project (.fsproj) file."
            | Output_File _ -> "The output file, in XML format."

[<Literal>]
let programName = "fsharp-coverage-corrector"
  
let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
let parser = ArgumentParser.Create<CliArguments>(programName=programName, errorHandler = errorHandler)

let printErrorAndExit message =
  eprintfn $"Error: %s{parser.PrintUsage(message)}"
  exit 1
      
let parseResult = parser.Parse(inputs = Environment.GetCommandLineArgs()[1..])

let toolsPath = Ionide.ProjInfo.Init.init()

// attempt to load the projects
let projectLoadResults = match loadProjectFiles (parseResult.GetResults Project_File) toolsPath with
                         | Ok parsedFileInfo -> parsedFileInfo
                         | Error errors ->
                             // build up an error message, listing all the errors
                             let errorMessages = List<string>()
                             errorMessages.Add $"Errors occurred loading project information:"
                             for err in errors do               
                                match err with
                                | ProjectLoadError.ProjectInfoError e -> errorMessages.Add $"\t{e}"
                                | ProjectLoadError.ParseFileError failureList ->
                                    let errMsg = failureList |> List.fold
                                                                    (fun str failure ->
                                                                      str + Environment.NewLine +
                                                                      match failure with
                                                                      | ParseFileFailure.AbortedTypeCheck -> "\t\t- Aborted type check"
                                                                      | ParseFileFailure.FailedToParseFile parseFailures ->
                                                                        parseFailures
                                                                        |> Array.map (fun f -> $"\t\t- Failed to parse file ${f.FileName}: ${f.Message}")
                                                                        |> String.concat Environment.NewLine
                                                                   )
                                                                   "\t- Error parsing source files"
                                    errorMessages.Add errMsg
                             // print the final error
                             printErrorAndExit (String.Join('\n', errorMessages.ToArray()))
                           
let anrps = loadAstNodeRuleParamsFromProject projectLoadResults

match readCoberturaFileContents projectLoadResults anrps (parseResult.GetResult Coverage_File) with
| Error ex ->
  let errMsg = $"Error reading data from input file: {ex.Message}{Environment.NewLine}{ex.ToString()}{Environment.NewLine}"
  printErrorAndExit errMsg
| Ok coberturaFileContents ->
  // apply the corrections
  let correctionPairs =
    coberturaFileContents.Packages
    |> List.map correctAutogeneratedClasses
    |> List.map (fun (pkg, errors) ->
      let pkg, newErrors = correctBranchCoverage pkg
      pkg, errors @ newErrors)
  let correctedPackages = correctionPairs |> List.map fst
  let correctionErrors =
    correctionPairs
    |> List.map snd
    |> List.concat
    |> List.distinct
  
  for ce in correctionErrors do
    eprintfn $"Warning: {ce}"
  
  let xml = generateXml {coberturaFileContents with Packages = correctedPackages}
  try
    xml.Save(parseResult.GetResult Output_File)
  with
  | ex -> let errMsg = $"Error writing data to {Output_File}: {ex.Message}{Environment.NewLine}{ex.ToString()}{Environment.NewLine}"
          printErrorAndExit errMsg

// finally, if successful, return ok exit code
Environment.ExitCode <- 0