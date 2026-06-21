namespace ARCExpect

type BadgeCreation =


    static member ofTestResults(
        labelText: string,
        ?ValueSuffix: string,
        ?Thresholds: Map<string, string>,
        ?DefaultColor: string
    ) =
        
        fun (testResults: TestRunResults) ->

            let max = testResults.Passed.Length + testResults.Failed.Length

            let thresholds = 
                Thresholds
                |> Option.defaultValue (Map([
                    string 0, BadgeColor.RED
                    string (max/2), BadgeColor.ORANGE_2
                    string max, BadgeColor.GREEN
                ]))

            let valueSuffix = 
                ValueSuffix
                |> Option.defaultValue $"/{max}"

            Badge(
                label = labelText,
                defaultColor = (DefaultColor |> Option.defaultValue (BadgeColor.ORANGE_2) ),
                thresholds = thresholds,
                value = testResults.Passed.Length,
                valueSuffix = valueSuffix
            )


    static member ofValidationSummary(
        labelText: string,
        ?ValueSuffix: string,
        ?Thresholds: Map<string, string>,
        ?DefaultColor: string
    ) =
        
        fun (validationSummary: ValidationSummary) ->

            let total = validationSummary.Critical.Total + validationSummary.NonCritical.Total

            let totalPassed = validationSummary.Critical.Passed + validationSummary.NonCritical.Passed

            let criticalFailedOrErrored = validationSummary.Critical.Failed + validationSummary.Critical.Errored

            if validationSummary.Critical.HasFailures then

                Badge(
                    label = labelText,
                    defaultColor = BadgeColor.RED,
                    value = criticalFailedOrErrored,
                    valueSuffix = $" Critical Errors"
                )

            else

                let thresholds = 
                    Thresholds
                    |> Option.defaultValue (Map([
                        string 0, BadgeColor.RED
                        string (total/2), BadgeColor.ORANGE_2
                        string total, BadgeColor.GREEN
                    ]))

                let valueSuffix = 
                    ValueSuffix
                    |> Option.defaultValue $"/{total}"

                Badge(
                    label = labelText,
                    defaultColor = (DefaultColor |> Option.defaultValue (BadgeColor.ORANGE_2) ),
                    thresholds = thresholds,
                    value = totalPassed,
                    valueSuffix = valueSuffix
                )