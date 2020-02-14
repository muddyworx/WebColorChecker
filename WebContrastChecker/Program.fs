open System

type RgbColor = {
    red : byte
    green : byte 
    blue : byte
    }

type SRgbColor = {
    red : double
    green : double
    blue : double
    }

type ColorFactor = {
    red : double
    green : double
    blue : double
    }

type rgbToSRgb = RgbColor -> SRgbColor

type findColorFactor = double -> double

type sRgbToColorFactor = SRgbColor-> ColorFactor

type colorFactorToRelativeLuminance = ColorFactor -> double

type calculateRelativeLuminance = RgbColor -> double

type hexStringToRgb = string -> RgbColor

type calculateContrast = RgbColor -> RgbColor -> double

let rgbToSRgb (rgbColor:RgbColor) = 
    let sRgb = {
        red = (double rgbColor.red)/255.0
        green = (double rgbColor.green)/255.0 
        blue = (double rgbColor.blue)/255.0
        }
    sRgb

let findColorFactor (colorRatio) = 
    match colorRatio <= 0.03928 with 
    | true -> colorRatio/12.92
    | false -> ((colorRatio + 0.055)/1.055)**2.4

let sRgbToColorFactor (sRgbColor) = 
    let colorFactor = {
        red = findColorFactor sRgbColor.red
        green = findColorFactor sRgbColor.green
        blue = findColorFactor sRgbColor.blue
    }
    colorFactor

let colorFactorToRelativeLuminance (colorFactor) = 
    (0.2126 * colorFactor.red) + (0.7152 * colorFactor.green) + (0.0722 * colorFactor.blue)

let calculateRelativeLuminance (rgbColor) = 
    let relativeLuminance =  
        rgbColor 
        |> rgbToSRgb
        |> sRgbToColorFactor
        |> colorFactorToRelativeLuminance
    relativeLuminance

let hexStringToRgb (hexColor:string) = 
    let hexToByte (s:string) = 
        System.Convert.ToByte(s, 16)

    let rgbColor = {
        RgbColor.red = hexToByte (hexColor.[0..1])
        RgbColor.green = hexToByte (hexColor.[2..3])
        RgbColor.blue = hexToByte (hexColor.[4..5])
        }
    rgbColor

let calculateContrast (firstColor:RgbColor) (secondColor:RgbColor) = 
    let firstLuminance = calculateRelativeLuminance firstColor
    let secondLuminance = calculateRelativeLuminance secondColor
    let lowestLuminance = min firstLuminance secondLuminance
    let highestLuminance = max firstLuminance secondLuminance
    (highestLuminance + 0.05)/(lowestLuminance + 0.05) 

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
            |> hexStringToRgb
            |> calculateRelativeLuminance
        printfn "Relative luminance: %f" rL
        0
    | 2 -> 
        let contrastRatio = calculateContrast (hexStringToRgb argv.[0]) (hexStringToRgb argv.[1])  
        printfn "Contrast ratio: %f" contrastRatio
        0
    | _ -> 
        printfn "Wrong number of arguments. One or two required."
        1