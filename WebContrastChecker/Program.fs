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
    if colorRatio <= 0.03928 then (colorRatio/12.92) 
    else (((colorRatio + 0.055)/1.055)**2.4)

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
    if argv.Length <> 2 then
        printfn "Invalid number of arguments. 2 required"
        1
    else 
        let firstColor = hexStringToRgb argv.[0]
        let secondColor = hexStringToRgb argv.[1]
        let contrastRatio = calculateContrast firstColor secondColor
        printfn "Contrast ratio: %f" contrastRatio
        0