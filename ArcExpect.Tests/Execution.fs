module Execution.Tests

open Fable.Pyxpecto
open ARCExpect

let private validationPackage =
    Setup.ValidationPackage(
        name = "execution-tests",
        summary = "Tests for executing validation packages",
        description = "Exercises execution and badge creation.",
        majorVersion = 1,
        minorVersion = 0,
        patchVersion = 0,
        programmingLanguage = "fsharp",
        CriticalValidationCases = [
            testCase "critical pass" <| fun () -> Expect.isTrue true "critical test passes"
        ],
        NonCriticalValidationCases = [
            testCase "non-critical failure" <| fun () -> Expect.equal 1 2 "non-critical test fails"
        ]
    )

let private criticalFailurePackage =
    Setup.ValidationPackage(
        name = "critical-failure-tests",
        summary = "Tests for critical failure badges",
        description = "Exercises the critical failure badge branch.",
        majorVersion = 1,
        minorVersion = 0,
        patchVersion = 0,
        programmingLanguage = "fsharp",
        CriticalValidationCases = [
            testCase "critical failure" <| fun () -> Expect.equal 1 2 "critical test fails"
        ]
    )

let private allPassingPackage =
    Setup.ValidationPackage(
        name = "all-passing-tests",
        summary = "Tests for green validation badges",
        description = "Exercises the all-passing badge branch.",
        majorVersion = 1,
        minorVersion = 0,
        patchVersion = 0,
        programmingLanguage = "fsharp",
        CriticalValidationCases = [
            testCase "critical pass" <| fun () -> Expect.isTrue true "critical test passes"
        ],
        NonCriticalValidationCases = [
            testCase "non-critical pass" <| fun () -> Expect.isTrue true "non-critical test passes"
        ]
    )

let private validationSummaryAsync () =
    validationPackage |> Execute.ValidationAsync()

let private criticalFailureSummaryAsync () =
    criticalFailurePackage |> Execute.ValidationAsync()

let private allPassingSummaryAsync () =
    allPassingPackage |> Execute.ValidationAsync()

let private isGreenBadge (svg: string) =
    let normalized = svg.ToLowerInvariant()
    normalized.Contains "#4c1" || normalized.Contains "#97ca00"

let private validationPipelineBasePath =
#if FABLE_COMPILER_PYTHON
    "TestResults/py"
#else
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "TestResults")
    |> System.IO.Path.GetFullPath
#endif

let private validationPipelineResultFolder =
    validationPackage
    |> Execute.ValidationPipeline(validationPipelineBasePath)

    let resultsRoot = ARCExpect.Helper.Path.combine validationPipelineBasePath ".arc-validate-results"
    ARCExpect.Helper.Path.combine resultsRoot "execution-tests@1.0.0"

let private normalizeLineEndings (text: string) =
    text.Replace("\r\n", "\n")

let private expectedValidationSummary = """{
    "Critical": {
        "HasFailures": false,
        "Total": 1,
        "Passed": 1,
        "Failed": 0,
        "Errored": 0
    },
    "NonCritical": {
        "HasFailures": true,
        "Total": 1,
        "Passed": 0,
        "Failed": 1,
        "Errored": 0
    },
    "ValidationPackage": {
        "Name": "execution-tests",
        "Version": "1.0.0",
        "Summary": "Tests for executing validation packages",
        "Description": "Exercises execution and badge creation."
    }
}"""

let private expectedValidationReport =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?><testsuites tests=\"2\" failures=\"1\" errors=\"0\"><testsuite name=\"execution-tests\" tests=\"2\" failures=\"1\" errors=\"0\" skipped=\"0\"><testcase name=\"Critical - critical pass\"/><testcase name=\"NonCritical - non-critical failure\"><failure message=\"non-critical test fails.\n\u001b[32mexpected\u001b[0m: \u001b[32m2\u001b[0m\n\u001b[31m  actual\u001b[0m: \u001b[31m1\u001b[0m\"/></testcase></testsuite></testsuites>"

let execution = testList "execution" [
    testCaseAsync "Validation runs critical and non-critical validation cases" <| async {
        let! summary = validationSummaryAsync ()

        Expect.equal summary.ValidationPackage.Name "execution-tests" "package metadata is retained"
        Expect.equal summary.Critical.Total 1 "one critical test was run"
        Expect.equal summary.Critical.Passed 1 "critical test passed"
        Expect.equal summary.NonCritical.Total 1 "one non-critical test was run"
        Expect.equal summary.NonCritical.Failed 1 "non-critical failure is recorded"
        Expect.isFalse summary.Critical.HasFailures "critical suite has no failures"
        Expect.isTrue summary.NonCritical.HasFailures "non-critical suite has a failure"
    }

    testCaseAsync "Validation retains individual test outcomes" <| async {
        let! summary = validationSummaryAsync ()
        let criticalEntries = summary.Critical.OriginalRunSummary.Value.Entries
        let nonCriticalEntries = summary.NonCritical.OriginalRunSummary.Value.Entries

        Expect.equal criticalEntries.Length 1 "critical result contains one entry"
        Expect.equal nonCriticalEntries.Length 1 "non-critical result contains one entry"
        Expect.equal criticalEntries.Head.Outcome Passed "critical entry passed"

        match nonCriticalEntries.Head.Outcome with
        | Failed message -> Expect.isTrue (message.Contains "non-critical test fails") "failure message is retained"
        | outcome -> failwithf "Expected a failed non-critical test, got %A" outcome
    }
]

let badgeCreation = testList "badge creation" [
    testCaseAsync "creates a result badge with the passed-to-total value" <| async {
        let! summary = validationSummaryAsync ()
        let badge =
            summary.Critical.OriginalRunSummary.Value
            |> BadgeCreation.ofTestResults "critical tests"

        Expect.isTrue (badge.BadgeSvgText.Contains "critical tests") "badge includes its label"
        Expect.isTrue (badge.BadgeSvgText.Contains "1/1") "badge includes passed and total tests"
        Expect.isTrue (isGreenBadge badge.BadgeSvgText) "all-passing test results produce a green badge"
    }

    testCaseAsync "creates a validation badge that includes non-critical results" <| async {
        let! summary = validationSummaryAsync ()
        let badge = summary |> BadgeCreation.ofValidationSummary "ARC validation"

        Expect.isTrue (badge.BadgeSvgText.Contains "ARC validation") "badge includes its label"
        Expect.isTrue (badge.BadgeSvgText.Contains "1/2") "badge includes all passed and total tests"
    }

    testCaseAsync "creates a green validation badge when all tests pass" <| async {
        let! summary = allPassingSummaryAsync ()
        let badge = summary |> BadgeCreation.ofValidationSummary "ARC validation"

        Expect.isTrue (badge.BadgeSvgText.Contains "2/2") "badge includes all passed and total tests"
        Expect.isTrue (isGreenBadge badge.BadgeSvgText) "all-passing validation produces a green badge"
    }

    testCaseAsync "creates a critical-error badge when critical tests fail" <| async {
        let! summary = criticalFailureSummaryAsync ()
        let badge = summary |> BadgeCreation.ofValidationSummary "ARC validation"

        Expect.isTrue (badge.BadgeSvgText.Contains "ARC validation") "badge includes its label"
        Expect.isTrue (badge.BadgeSvgText.Contains "1 Critical Errors") "badge reports the critical error count"
    }

    testCase "ValidationPipeline creates a complete validation result folder" <| fun () ->
        let summaryPath = ARCExpect.Helper.Path.combine validationPipelineResultFolder "validation_summary.json"
        let reportPath = ARCExpect.Helper.Path.combine validationPipelineResultFolder "validation_report.xml"
        let badgePath = ARCExpect.Helper.Path.combine validationPipelineResultFolder "badge.svg"

        Expect.isTrue (ARCExpect.Helper.Directory.exists validationPipelineResultFolder) "result folder is created below .arc-validate-results"

        let summary = ARCExpect.Helper.File.readAllText summaryPath
        Expect.equal (normalizeLineEndings summary) expectedValidationSummary "summary exactly matches the expected validation result"

        let report = ARCExpect.Helper.File.readAllText reportPath
        Expect.equal (normalizeLineEndings report) expectedValidationReport "report exactly matches the expected JUnit XML"

        let badge = ARCExpect.Helper.File.readAllText badgePath
        Expect.isTrue (badge.Contains "<svg") "badge is an SVG"
        Expect.isTrue (badge.Contains "execution-tests@1.0.0") "badge uses the default package-version label"
        Expect.isTrue (badge.Contains "1/2") "badge represents total passed tests"

    testCaseAsync "writes a badge SVG through Execute.BadgeCreation" <| async {
        let path = ARCExpect.Helper.Path.combine validationPipelineBasePath "execute-badge-creation.svg"

        ARCExpect.Helper.Directory.ensure validationPipelineBasePath

        let! summary = validationSummaryAsync ()

        summary
        |> Execute.BadgeCreation(path, "ARC validation")

        Expect.isTrue (ARCExpect.Helper.File.readAllText(path).Contains "ARC validation") "written SVG includes the label"
    }
]
