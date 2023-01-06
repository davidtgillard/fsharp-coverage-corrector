#!/usr/bin/env bash


DOTNET_VERSION=6.0
FSCC_CONSOLE_PATH="./FSharpCoverageCorrector.Console/bin/Debug/net5.0/FSharpCoverageCorrector.Console"
TEST_ASSEMBLY_PATH="./CoverageExample/Example.Tests/bin/Debug/net${DOTNET_VERSION}/Example.Tests.dll"
PROJECT_FILE="./CoverageExample/Example/Example.fsproj"

UNCORRECTED_COVERAGE_REPORT_PATH_PREFIX="/tmp/uncorrected-fsharp-coverage-corrector-example"
UNCORRECTED_COBERTURA_FILE="${UNCORRECTED_COVERAGE_REPORT_PATH_PREFIX}.cobertura.xml"
UNCORRECTED_REPORT_DIR="$UNCORRECTED_COVERAGE_REPORT_PATH_PREFIX"

CORRECTED_COVERAGE_REPORT_PATH_PREFIX="/tmp/corrected-fsharp-coverage-corrector-example"
CORRECTED_COBERTURA_FILE="${CORRECTED_COVERAGE_REPORT_PATH_PREFIX}.cobertura.xml"
CORRECTED_REPORT_DIR="$CORRECTED_COVERAGE_REPORT_PATH_PREFIX"

dotnet coverlet "$TEST_ASSEMBLY_PATH" \
	--target "dotnet" \
	--targetargs "test $TEST_ASSEMBLY_PATH" \
	--output "$UNCORRECTED_COVERAGE_REPORT_PATH_PREFIX" \
	--format "cobertura" \
	--verbosity "detailed" \
	--exclude-by-attribute "Obsolete,GeneratedCode,CompilerGenerated" \
&& \
reportgenerator "-reports:$UNCORRECTED_COBERTURA_FILE" "-targetdir:$UNCORRECTED_REPORT_DIR" \ 


eval "$FSCC_CONSOLE_PATH" \
	--coverage-file "$UNCORRECTED_COBERTURA_FILE" \
	--project-file "$PROJECT_FILE" \
	--output-file "$CORRECTED_COBERTURA_FILE" \
&& \
reportgenerator "-reports:$CORRECTED_COBERTURA_FILE" "-targetdir:$CORRECTED_REPORT_DIR"
