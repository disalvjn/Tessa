namespace Tessa.App
open Tessa.View
open Tessa.View.Types
open Tessa.Solve.Shapes
open Tessa.Eval
open Tessa.Eval.Types
open Tessa.Lex
open Tessa.Parse
open Tessa.Util
open Tessa.Language

module App = 
    module V = View
    module V = ViewTypes
    module E = Eval 
    module E = EvalTypes
    module S = SolveShapes
    module Lex = Lex 
    module Parse = Parse
    module L = Language
    open Util

    open Browser.Dom
    open Fable.Core.JsInterop

    type DrawOptions = {
        drawPoints: bool;
        fillPolygons: bool;
    }

    let draw (ctx: Browser.Types.CanvasRenderingContext2D) drawOptions (shape: V.DrawShape) =
        // printf "%A" shape
        match shape with 
        | V.DrawPoint((x, y), options) ->
            if drawOptions.drawPoints
            then 
                ctx.beginPath()
                ctx.arc(x, y, 5.0, 0.0, 3.141592 * 2.0)
                ctx.fillStyle <- !^ options.color
                ctx.closePath()
                ctx.fill()
                ctx.strokeStyle <- !^ "ffffff"
                ctx.fillStyle <- !^ "ffffff"
                ctx.font <- "18px Arial";
                ctx.fillText(options.label, x- 10.0, y - 5.0)
        | V.DrawPolygon(segments, options) -> 
            ctx.beginPath()
            ctx.strokeStyle <- !^ options.color
            ctx.fillStyle <- !^ options.color
            let (fx, fy) = List.head segments
            ctx.moveTo(fx, fy)
            List.iter ctx.lineTo (List.tail segments)
            // ctx.lineTo(fx, fy)
            ctx.closePath()
            match options.drawMode with 
            | V.Stroke -> 
                ctx.stroke()
            | V.Fill -> 
                ctx.stroke()
                ctx.fill()
            ctx.strokeStyle <- !^ "ffffff"
            ctx.fillStyle <- !^ "ffffff"

    let fromResult = function 
        | Ok o -> o 
        | Error e -> failAndPrint e


    let go (ctx: Browser.Types.CanvasRenderingContext2D) (writeError: obj -> unit) program = 
        try 
            let lexed = Lex.lex program 
            // printf "%A" lexed 

            let parsed = lexed |> fromResult |> List.map fst |> Parse.parseList

            let result = parsed |> fromResult |> fst |> E.eval

            let cells = 
                Map.mapList (fun k v -> E.asCell v) result.runtime.environment 
                |> okays 

            let runTimeTessellations = result.runtime.tessellations
                // Append so we have at least one cell to map over. It's empty so it won't do anything.
                // |> List.append [L.Primary []]
            let tessellations = 
                if not <| List.isEmpty runTimeTessellations 
                then runTimeTessellations 
                else List.map (fun cell -> L.Tessellation(cell, [])) cells

            let labels = result.runtime.labels
            let hidePoints = 
                Map.tryFind E.hidePointsVariable result.runtime.dynamicEnvironment 
                |> Option.cata (function | E.Bool b -> b | _ -> false) false

            let maxX = 2000.0
            let maxY = 2000.0
            let targets = {V.boundingHeight = 1950.0; V.boundingWidth = 1950.0; V.topLeft = (25.0, 25.0); V.xMax = maxX; V.yMax = maxY}
            let drawable = List.map (fun tessellation -> V.solveTessellation targets tessellation labels) tessellations  |> Result.sequence

            match drawable with
            | Ok draws -> 
                ctx.fillStyle <- !^ "ffffff"
                ctx.clearRect(0.0, 0.0, maxX, maxY)
                ctx.globalCompositeOperation <- "screen"
                List.iter (draw ctx {drawPoints = not hidePoints; fillPolygons = false;}) <| List.concat draws
            | Error e -> ()

        with 
            | e -> writeError (e.Message, e.StackTrace)

    // Mutable variable to count the number of times we clicked the button
    // let mutable count = 0
    // https://github.com/fable-compiler/fable2-samples/blob/master/browser/src/App.fs
    let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "canvas" // myCanvas is defined in public/index.html 
    myCanvas.width <- float 2000 
    myCanvas.height <- float 2000
    let ctx = myCanvas.getContext_2d()

    // go ctx program

    let textArea =  window.document.getElementById "text" :?> Browser.Types.HTMLTextAreaElement
    let mutable errorTextArea = window.document.getElementById "errors" :?> Browser.Types.HTMLTextAreaElement
    textArea.onkeydown <- fun event -> 
        errorTextArea.value <- ""
        if event.keyCode = float 192 
        then 
            event.preventDefault ()
            go ctx (fun e -> (errorTextArea.value <- errorTextArea.value + "\n" + sprintf "%A" e)) textArea.value
        else ()

    // Get a reference to our button and cast the Element to an HTMLButtonElement
    // let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

    // // Register our listener
    // myButton.onclick <- fun _ ->
    //     count <- count + 1
    //     myButton.innerText <- sprintf "You clicked: %i time(s)" count
