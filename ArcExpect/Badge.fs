namespace ARCExpect

open System
open System.Collections.Generic

module BadgeColor = 

    let RED = "red"
    let ORANGE_2 = "orange"
    let GREEN = "green"

type BadgeStyle =
    | Default
    | GitLabScoped

#if FABLE_COMPILER_PYTHON

open Fable.Core

[<AllowNullLiteral>]
type PyBadge =
    abstract badgeSvgText: string

    [<NamedParams(fromIndex = 1)>]
    abstract writeBadge:
        filePath: string *
        ?overwrite: bool ->
            unit

[<Erase>]
type AnyBadgePy =
    [<Import("Badge", "anybadge")>]
    [<NamedParams(fromIndex = 2)>]
    static member inline badge
        (
            label: string,
            value: obj,
            ?fontName: string,
            ?fontSize: int,
            ?numPaddingChars: int,
            ?numLabelPaddingChars: float,
            ?numValuePaddingChars: float,
            ?template: string,
            ?style: string,
            ?valuePrefix: string,
            ?valueSuffix: string,
            ?thresholds: Dictionary<string, string>,
            ?defaultColor: string,
            ?useMaxWhenValueExceeds: bool,
            ?textColor: string,
            ?semver: bool,
            ?escapeLabel: bool,
            ?escapeValue: bool
        ) : PyBadge =
        nativeOnly

module Impl =

    let styleToString =
        function
        | BadgeStyle.Default -> "default"
        | BadgeStyle.GitLabScoped -> "gitlab-scoped"

    let thresholdsToDict (thresholds: Map<string, string> option) =
        thresholds
        |> Option.map (fun thresholds ->
            let d = Dictionary<string, string>()

            for KeyValue(k, v) in thresholds do
                d.Add(k, v)

            d
        )

type Badge
    (
        label: string,
        value: obj,
        ?fontName: string,
        ?fontSize: int,
        ?numPaddingChars: int,
        ?numLabelPaddingChars: float,
        ?numValuePaddingChars: float,
        ?template: string,
        ?style: BadgeStyle,
        ?valuePrefix: string,
        ?valueSuffix: string,
        ?thresholds: Map<string, string>,
        ?defaultColor: string,
        ?useMaxWhenValueExceeds: bool,
        ?valueFormat: obj -> string,
        ?textColor: string,
        ?semver: bool,
        ?escapeLabel: bool,
        ?escapeValue: bool
    ) =

    let pyValue =
        match valueFormat with
        | Some format -> box (format value)
        | None -> value

    let pyStyle = style |> Option.map Impl.styleToString
    let pyThresholds = Impl.thresholdsToDict thresholds

    let inner =
        AnyBadgePy.badge(
            label,
            pyValue,
            ?fontName = fontName,
            ?fontSize = fontSize,
            ?numPaddingChars = numPaddingChars,
            ?numLabelPaddingChars = numLabelPaddingChars,
            ?numValuePaddingChars = numValuePaddingChars,
            ?template = template,
            ?style = pyStyle,
            ?valuePrefix = valuePrefix,
            ?valueSuffix = valueSuffix,
            ?thresholds = pyThresholds,
            ?defaultColor = defaultColor,
            ?useMaxWhenValueExceeds = useMaxWhenValueExceeds,
            ?textColor = textColor,
            ?semver = semver,
            ?escapeLabel = escapeLabel,
            ?escapeValue = escapeValue
        )

    member _.BadgeSvgText =
        inner.badgeSvgText

    member _.WriteBadge(filePath: string, ?overwrite: bool) =
        inner.writeBadge(filePath, ?overwrite = overwrite)

#else

open System.Globalization
open System.IO
open System.Net

module private Impl =

    let styleToDotNet =
        function
        | BadgeStyle.Default -> AnyBadge.NET.TemplateStyle.Default
        | BadgeStyle.GitLabScoped -> AnyBadge.NET.TemplateStyle.GitLabScoped

    let valueToInvariantString (value: obj) =
        match value with
        | null -> ""
        | :? IFormattable as formattable ->
            formattable.ToString(null, CultureInfo.InvariantCulture)
        | _ ->
            string value

    let escapeXmlText (shouldEscape: bool option) (text: string) =
        if defaultArg shouldEscape true then
            WebUtility.HtmlEncode(text)
        else
            text

type Badge
    (
        label: string,
        value: obj,
        ?fontName: string,
        ?fontSize: int,
        ?numPaddingChars: int,
        ?numLabelPaddingChars: float,
        ?numValuePaddingChars: float,
        ?template: string,
        ?style: BadgeStyle,
        ?valuePrefix: string,
        ?valueSuffix: string,
        ?thresholds: Map<string, string>,
        ?defaultColor: string,
        ?useMaxWhenValueExceeds: bool,
        ?valueFormat: obj -> string,
        ?textColor: string,
        ?semver: bool,
        ?escapeLabel: bool,
        ?escapeValue: bool
    ) =

    let dotnetStyle =
        style |> Option.map Impl.styleToDotNet

    let renderedValue =
        match valueFormat with
        | Some format -> format value
        | None -> Impl.valueToInvariantString value

    let label =
        Impl.escapeXmlText escapeLabel label

    let renderedValue =
        Impl.escapeXmlText escapeValue renderedValue

    let inner =
        AnyBadge.NET.Badge(
            label = label,
            value = renderedValue,
            ?FontName = fontName,
            ?FontSize = fontSize,
            ?NumPaddingChars = numPaddingChars,
            ?NumLabelPaddingChars = numLabelPaddingChars,
            ?NumValuePaddingChars = numValuePaddingChars,
            ?Template = template,
            ?Style = dotnetStyle,
            ?ValuePrefix = valuePrefix,
            ?ValueSuffix = valueSuffix,
            ?Thresholds = thresholds,
            ?DefaultColor = defaultColor,
            ?UseMaxWhenValueExceeds = useMaxWhenValueExceeds,
            ?TextColor = textColor,
            ?Semver = semver
        )

    member _.BadgeSvgText =
        inner.BadgeSvgText

    member _.WriteBadge(filePath: string, ?overwrite: bool) =
        let filePath =
            if filePath.EndsWith(".svg", StringComparison.OrdinalIgnoreCase) then
                filePath
            else
                filePath + ".svg"

        let overwrite = defaultArg overwrite false

        if File.Exists(filePath) && not overwrite then
            invalidOp $"File already exists: {filePath}"

        inner.WriteBadge(filePath)

#endif