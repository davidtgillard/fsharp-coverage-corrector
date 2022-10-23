namespace FSharpCoverageCorrector.Core

open System
open FSharp.Compiler.CodeAnalysis
open System.IO
open FSharpLint.Framework
open FSharpLint.Framework.Ast
open FSharpLint.Framework.ParseFile
open FSharpLint.Framework.Rules
open Ionide.ProjInfo.Types
open FSharpCoverageCorrector.Core.Utils

/// Data structures and functions for loading F# project information, including AST.
[<AutoOpen>]
module ProjectInfo =
  
  /// An error loading an FSharp project.
  type ProjectLoadError =
    /// A error occurred loading project information.
    | ProjectInfoError of string
    /// Errors occurred parsing the project.
    | ParseFileError of ParseFileFailure list
  
  /// loads project info. returns a tuple (loadedOptions, loadErrors) where loadedOptions contains loaded project options, and loadErrors contains any load errors
  let private getProjectInfo (projectFilePaths: string list) (toolsPath: ToolsPath) =
    // set up the loader
    let loader = Ionide.ProjInfo.WorkspaceLoader.Create toolsPath
    let notifications = ResizeArray<_>()
    loader.Notifications.Add notifications.Add
    // load the projects
    let options = loader.LoadProjects projectFilePaths
    let loadedOptions = options
                        |> Seq.map (fun proj -> Ionide.ProjInfo.FCS.mapToFSharpProjectOptions proj options)
                        |> Seq.toList
    let loadErrors =
      seq {
        for projState in notifications do
          match projState with
          | WorkspaceProjectState.Failed (projectFile, _) -> yield (ProjectInfoError $"An error occurred attempting to load {projectFile}")
          | _ -> ()
      } |> Seq.toList
    loadedOptions, loadErrors
    
    
  /// returns either a failed file, or none
  let private getFailedFiles = function
    | Failed failure -> Some failure
    | _ -> None
  
  /// returns either a parsed file, or none
  let private getParsedFiles = function
    | Success file -> Some file
    | _ -> None
  
  /// parses the files in a project and returns them in a result, or a list of parse failures
  let private parseFilesInProject files projectOptions checker =
    // get the parsed files
    let parsedFiles = files
                      |> List.map (fun file -> parseFile file checker (Some projectOptions))
    // get the failed files
    let failedFiles = parsedFiles |> List.choose getFailedFiles
    // if there aren't any failed, return Ok
    if List.isEmpty failedFiles then
      parsedFiles |> List.choose getParsedFiles |> Ok
    else
      Error failedFiles
      
  /// <summary>
  ///     Attempts to load all the files related to a project.
  /// </summary>
  /// <param name="projectFilePaths">The path to the project file (.fsproj).</param>
  /// <param name="toolsPath">The path to the Ionide tools.</param>
  /// <returns>A list of <c>FileParseInfo</c>s, if successful; otherwise a <c>ProjectLoadError</c>.</returns>
  let loadProjectFiles projectFilePaths toolsPath =
    let projectFilePaths = projectFilePaths
                           |> List.map Path.GetFullPath // convert to absolute paths
                           |> List.distinct // ensure that no project is attempted to be loaded twice
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    // try to retrieve the project info and parse the files
    let loadedProjectOptions, loadErrors = getProjectInfo projectFilePaths toolsPath
    if List.isEmpty loadErrors then
      let success, errors = loadedProjectOptions 
                            |> List.map (fun projectOptions -> match parseFilesInProject (Array.toList projectOptions.SourceFiles) projectOptions checker with
                                                               | Ok parseFileInfoList -> Ok parseFileInfoList
                                                               | Error parseFileFailures -> Error (ParseFileError parseFileFailures))
                            |> unwrapAndPartitionResults
      if List.isEmpty errors then
        success |> List.concat |> Ok
      else
        Error errors
    else
      Error loadErrors
    
  /// <summary>
  ///     Given a list of parsed files, creates a map of <c>SourceFile</c>s to a list of <c>AstNodeRuleParams</c> loaded from the file.
  /// </summary>
  /// <param name="fileParseInfoList">A list of parsed files.</param>
  /// <returns>A map of <c>SourceFile</c>s to <c>AstNodeRuleParams</c>, which references each source-file with the nodes loaded from it.</returns>
  let loadAstNodeRuleParamsFromProject fileParseInfoList =
    fileParseInfoList
    |> List.map (fun fpi ->
      // collect the lines in the file
      let lines = String.toLines fpi.Text |> Array.map (fun (line, _, _) -> line)
      // get the AST 
      let syntaxArray = AbstractSyntaxArray.astToArray fpi.Ast
      let file = { Filename = fpi.File
                   RelativePath = fpi.File
                   Lines = fpi.Text.Split([|"\r\n"; "\n"; "\r"|], StringSplitOptions.None) }
      // populate the nodes structures
      let nodes = syntaxArray 
                  |> Array.mapi (fun i astNode -> (i, astNode))
                  |> Array.map (fun (i, astNode) ->
                     let getParents (depth:int) = AbstractSyntaxArray.getBreadcrumbs depth syntaxArray i
                     { AstNode = astNode.Actual
                       NodeHashcode = astNode.Hashcode
                       NodeIndex =  i
                       SyntaxArray = syntaxArray
                       GetParents = getParents
                       FilePath = fpi.File
                       FileContent = fpi.Text
                       Lines = lines
                       CheckInfo = fpi.TypeCheckResults
                       GlobalConfig = GlobalRuleConfig.Default })
                 |> Array.toList
      // return tuple of files and nodes
      file, nodes)
    |> Map.ofList