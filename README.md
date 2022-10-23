# FSharp Coverage Corrector

## Description

Presently, generating code coverage for F# source code has a few problems: namely, the common tools for generating coverage reports such as [coverlet](https://github.com/coverlet-coverage/coverlet) operate at the IL level, and are tuned for C#. The resulting coverage metrics on F# source can be incorrect or inappropriate for certain cases. 

This project provides a command-line tool for taking as input the output of a coverage file in cobertura format, providing some 'corrections' to it, and outputting the corrected test coverage file.

## Installing and Using the `fsharp-coverage-corrector` Tool

To install fsharp coverage collector as a command line tool, run:

```
dotnet tool install --global fsharp-coverage-corrector.console --version 0.1.1-alpha
```

To run the tool, the command must be provided with: 
1. The path to an input coverage file in cobertura format, specified with `--coverage-file`.
2. The path to the .fsproj file specified with `--project-file`, which provides the referenced source code against which corrections will be made. Multiple `--project-file` options may be included.
3. A path for an output file, to be created. 

Example of usage:

```
fsharp-coverage-corrector --coverage-file coverage-cobertura.xml --project-file MyFSharpProject.fsproj --output-file corrected-coverage-cobertura.xml
```

## Current State, Maintenance

This project is presently in a pre-release (alpha) state, developed and maintained at the author's whim.
