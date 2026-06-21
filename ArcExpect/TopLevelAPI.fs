namespace ARCExpect

open AnyBadge.NET
open Fable.Pyxpecto.Model
open ARCExpect.Helper
open AVPRIndex

type Setup =
    
    static member Metadata(
        frontmatter: string,
        programmingLanguage: FrontmatterLanguage
    ) =
        ValidationPackageMetadata.extractFromString programmingLanguage frontmatter

    static member ValidationPackage(
        metadata: ValidationPackageMetadata,
        ?CriticalValidationCases: TestCase list,
        ?NonCriticalValidationCases: TestCase list
    ) =
        ARCValidationPackage.create(
            metadata = metadata,
            ?CriticalValidationCasesList = CriticalValidationCases,
            ?NonCriticalValidationCasesList = NonCriticalValidationCases
        )

    static member ValidationPackage(
        name: string,
        summary: string,
        description: string,
        majorVersion: int,
        minorVersion: int,
        patchVersion: int,
        programmingLanguage: string,
        ?Publish: bool,
        ?Authors: Author array,
        ?Tags: OntologyAnnotation array,
        ?ReleaseNotes: string,
        ?CriticalValidationCases: TestCase list,
        ?NonCriticalValidationCases: TestCase list,
        ?CQCHookEndpoint: string
    ) =
        Setup.ValidationPackage(
            metadata = ValidationPackageMetadata.create(
                name = name,
                summary = summary,
                description = description,
                majorVersion = majorVersion,
                minorVersion = minorVersion,
                patchVersion = patchVersion,
                programmingLanguage = programmingLanguage,
                ?Publish = Publish,
                ?Authors = Authors,
                ?Tags = Tags,
                ?ReleaseNotes = ReleaseNotes,
                ?CQCHookEndpoint = CQCHookEndpoint
            ),
            ?CriticalValidationCases = CriticalValidationCases,
            ?NonCriticalValidationCases = NonCriticalValidationCases
        )

open System.Collections.Generic
type Execute =

// ------------------ New API with ARCValidationPackage, metadata support and custom summaries ------------------
    
    static member ValidationAsync (
        ?Payload: Dictionary<string, obj>
    ) =
        fun (arcValidationPackage: ARCValidationPackage) ->
            async {
                let! criticalResults = PyxpectoRunner.runTestsWithResultsAsync arcValidationPackage.CriticalValidationCases
                let! nonCriticalResults = PyxpectoRunner.runTestsWithResultsAsync arcValidationPackage.NonCriticalValidationCases

                return
                    ValidationSummary.ofExpectoTestRunSummaries(
                        criticalSummary = criticalResults,
                        nonCriticalSummary = nonCriticalResults,
                        package = ValidationPackageSummary.create(arcValidationPackage.Metadata),
                        ?Payload = Payload
                    )
            }

    static member Validation (
        ?Payload: Dictionary<string, obj>
    ) =
        fun (arcValidationPackage: ARCValidationPackage) ->
            arcValidationPackage
            |> Execute.ValidationAsync(?Payload = Payload)
            |> Async.RunSynchronously

    static member SummaryCreation(
        path: string
    ) =  
        fun (validationSummary: ValidationSummary) -> 
            Serialization.writeJson path validationSummary 

    static member JUnitReportCreation(
        path: string
    ) =
        fun (validationSummary: ValidationSummary) -> 
            Serialization.writeJUnitXml path validationSummary

    static member BadgeCreation(
        path: string,
        labelText: string,
        ?ValueSuffix: string,
        ?Thresholds: Map<string, string>,
        ?DefaultColor: string
    ) =
        fun (validationSummary: ValidationSummary) -> 

            validationSummary
            |> BadgeCreation.ofValidationSummary(
                labelText,
                ?ValueSuffix = ValueSuffix,
                ?Thresholds = Thresholds,
                ?DefaultColor = DefaultColor
            )
            |> fun b -> b.WriteBadge(path)

    static member ValidationPipeline(
        basePath: string,
        ?BadgeLabelText: string,
        ?ValueSuffix: string,
        ?Thresholds: Map<string, string>,
        ?DefaultColor: string,
        ?Payload: Dictionary<string, obj>
    ) =
        fun (arcValidationPackage: ARCValidationPackage) ->

            let labelText = defaultArg BadgeLabelText $"{arcValidationPackage.Metadata.Name}@{ValidationPackageMetadata.getSemanticVersionString arcValidationPackage.Metadata}"

            let foldername = $"{arcValidationPackage.Metadata.Name}@{ValidationPackageMetadata.getSemanticVersionString arcValidationPackage.Metadata}"

            let resultFolder = 
                Path.combine ".arc-validate-results" foldername
                |> Path.combine basePath 
            let summaryPath = Path.combine resultFolder "validation_summary.json"
            let badgePath = Path.combine resultFolder "badge.svg"
            let jUnitPath = Path.combine resultFolder "validation_report.xml"

            Directory.ensure resultFolder |> ignore

            let results = 
                arcValidationPackage
                |> Execute.Validation(?Payload = Payload)

            results |> Execute.SummaryCreation(summaryPath)
            results |> Execute.JUnitReportCreation(jUnitPath)
            results
            |> Execute.BadgeCreation(
                badgePath, 
                labelText, 
                ?ValueSuffix = ValueSuffix, 
                ?Thresholds = Thresholds, 
                ?DefaultColor = DefaultColor
            )
