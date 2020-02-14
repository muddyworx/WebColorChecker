open System

type RgbColor = {
    red : byte
    green : byte 
    blue : byte
    }

type NormalizedRgb = {
    red : double
    green : double
    blue : double
    }

type RgbGamma = {
    red : double
    green : double
    blue : double
    }

type NormalizeRgb = RgbColor -> NormalizedRgb

type FindChannelGamma = double -> double

type FindRgbGamma = NormalizedRgb-> RgbGamma

type FindGammaLuminance = RgbGamma -> double

type FindRelativeLuminance = RgbColor -> double

type HexStringToRgb = string -> RgbColor

type FindContrast = RgbColor -> RgbColor -> double

let NormalizeRgb (rgbColor:RgbColor) = 
    let normalizedRgb = {
        red = (double rgbColor.red)/255.0
        green = (double rgbColor.green)/255.0 
        blue = (double rgbColor.blue)/255.0
        }
    normalizedRgb

let FindChannelGamma (normalizedChannel) = 
    match normalizedChannel <= 0.03928 with 
    | true -> normalizedChannel/12.92
    | false -> ((normalizedChannel + 0.055)/1.055)**2.4

let FindRgbGamma (normalizedRgb) = 
    let rgbGamma = {
        red = FindChannelGamma normalizedRgb.red
        green = FindChannelGamma normalizedRgb.green
        blue = FindChannelGamma normalizedRgb.blue
        }
    rgbGamma

let FindGammaLuminance (colorGamma) = 
    (0.2126 * colorGamma.red) + (0.7152 * colorGamma.green) + (0.0722 * colorGamma.blue)

let FindRelativeLuminance (rgbColor) = 
    let relativeLuminance =  
        rgbColor 
        |> NormalizeRgb
        |> FindRgbGamma
        |> FindGammaLuminance
    relativeLuminance

// TODO: this function could throw exceptions.
let HexStringToRgb (hexColor:string) = 
    let hexToByte (s:string) = 
        System.Convert.ToByte(s, 16)

    let rgbColor = {
        RgbColor.red = hexToByte (hexColor.[0..1])
        RgbColor.green = hexToByte (hexColor.[2..3])
        RgbColor.blue = hexToByte (hexColor.[4..5])
        }
    rgbColor

let FindContrast (c1:RgbColor) (c2:RgbColor) = 
    let l1 = FindRelativeLuminance c1
    let l2 = FindRelativeLuminance c2
    let lmin = min l1 l2
    let lmax = max l1 l2
    (lmax + 0.05)/(lmin + 0.05) 

[<EntryPoint>]
let main argv = 
    match argv.Length with 
    | 0 -> 
        printfn "Calculates contrast ratio or relative luminance.\n" 
        printfn "Takes either one or two hex strings as arguments (without #.)\n"
        0
    | 1 ->
        let rL = 
            argv.[0]
            |> HexStringToRgb
            |> FindRelativeLuminance
        printfn "Relative luminance: %f" rL
        0
    | 2 -> 
        let contrastRatio = FindContrast (HexStringToRgb argv.[0]) (HexStringToRgb argv.[1])  
        printfn "Contrast ratio: %f" contrastRatio
        0
    | _ -> 
        printfn "Wrong number of arguments. One or two required."
        1