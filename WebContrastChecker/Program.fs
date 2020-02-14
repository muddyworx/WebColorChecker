open System

type HexString = HexString of string

type RgbChannel = RgbChannel of byte

type NormalizedChannel = NormalizedChannel of double

type ChannelGamma = ChannelGamma of double

type RelativeLuminance = RelativeLuminance of double

type ContrastRatio = ContrastRatio of double

type RgbColor = {
    red : RgbChannel
    green : RgbChannel
    blue : RgbChannel
    }

type NormalizedRgb = {
    red : NormalizedChannel
    green : NormalizedChannel
    blue : NormalizedChannel
    }

type RgbGamma = {
    red : ChannelGamma
    green : ChannelGamma
    blue : ChannelGamma
    }

type NormalizeRgb = RgbColor -> NormalizedRgb

type FindChannelGamma = NormalizedChannel -> ChannelGamma

type FindRgbGamma = NormalizedRgb-> RgbGamma

type FindRelativeLuminance = RgbGamma -> RelativeLuminance

type FindRgbRelativeLuminance = RgbColor -> RelativeLuminance

type HexStringToRgb = HexString -> RgbColor

type FindContrast = RgbColor -> RgbColor -> ContrastRatio

let NormalizeRgb (rgbColor:RgbColor) = 
    let {
        RgbColor.red = (RgbChannel redChannel)
        RgbColor.green = (RgbChannel greenChannel)
        RgbColor.blue = (RgbChannel blueChannel)
        } = rgbColor
    let normalizedRgb = {
        NormalizedRgb.red = NormalizedChannel (double redChannel/255.0)
        NormalizedRgb.green = NormalizedChannel (double greenChannel/255.0)
        NormalizedRgb.blue = NormalizedChannel (double blueChannel/255.0)
        }
    normalizedRgb

let FindChannelGamma (NormalizedChannel n) = 
    match n <= 0.03928 with 
    | true -> ChannelGamma (n/12.92)
    | false -> ChannelGamma (((n + 0.055)/1.055)**2.4)

let FindRgbGamma (normalizedRgb:NormalizedRgb) = 
    let rgbGamma = {
        RgbGamma.red = FindChannelGamma normalizedRgb.red
        RgbGamma.green = FindChannelGamma normalizedRgb.green
        RgbGamma.blue = FindChannelGamma normalizedRgb.blue
        }
    rgbGamma

let FindRelativeLuminance (colorGamma:RgbGamma) = 
    let {
        RgbGamma.red = (ChannelGamma redGamma)
        RgbGamma.green = (ChannelGamma greenGamma)
        RgbGamma.blue = (ChannelGamma blueGamma)} = colorGamma
    RelativeLuminance ((0.2126 * redGamma) + (0.7152 * greenGamma) + (0.0722 * blueGamma))

let FindRgbRelativeLuminance (rgbColor) = 
    let relativeLuminance =  
        rgbColor 
        |> NormalizeRgb
        |> FindRgbGamma
        |> FindRelativeLuminance
    relativeLuminance

// TODO: this function could throw exceptions.
let HexStringToRgb (HexString hexColor) = 
    let hexToByte (s:string) = 
        System.Convert.ToByte(s, 16)

    let rgbColor = {
        RgbColor.red = RgbChannel (hexToByte (hexColor.[0..1]))
        RgbColor.green = RgbChannel (hexToByte (hexColor.[2..3]))
        RgbColor.blue = RgbChannel (hexToByte (hexColor.[4..5]))
        }
    rgbColor

let FindContrast (c1:RgbColor) (c2:RgbColor) = 
    let (RelativeLuminance l1) = FindRgbRelativeLuminance c1
    let (RelativeLuminance l2) = FindRgbRelativeLuminance c2
    let lmin = min l1 l2
    let lmax = max l1 l2
    ContrastRatio ((lmax + 0.05)/(lmin + 0.05))

[<EntryPoint>]
let main argv = 
    match argv.Length with 
    | 0 -> 
        printfn "Calculates contrast ratio or relative luminance.\n" 
        printfn "Takes either one or two hex strings as arguments (without #.)\n"
        0
    | 1 ->
        let (RelativeLuminance rL) = 
            HexString argv.[0]
            |> HexStringToRgb
            |> FindRgbRelativeLuminance
        printfn "Relative luminance: %f" rL
        0
    | 2 -> 
        let (ContrastRatio contrastRatio) = 
            FindContrast (HexStringToRgb (HexString argv.[0])) (HexStringToRgb (HexString argv.[1]))  
        printfn "Contrast ratio: %f" contrastRatio
        0
    | _ -> 
        printfn "Wrong number of arguments. One or two required."
        1