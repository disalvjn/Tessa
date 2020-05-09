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
                ctx.font <- "18px Arial";
                ctx.fillText(options.label, x- 10.0, y - 5.0)
        | V.DrawPolygon(segments, options) -> 
            ctx.beginPath()
            let (fx, fy) = List.head segments
            ctx.moveTo(fx, fy)
            List.iter ctx.lineTo (List.tail segments)
            // ctx.lineTo(fx, fy)
            ctx.closePath()
            ctx.fillStyle <- !^ options.color
            ctx.strokeStyle <- !^ options.color
            ctx.stroke()
            ctx.fill()

    let fromResult = function 
        | Ok o -> o 
        | Error e -> failAndPrint e


    let go (ctx: Browser.Types.CanvasRenderingContext2D) (writeError: obj -> unit) program = 
        try 
            // let lexed = Lex.lex program 
            // // printf "%A" lexed 

            // let parsed = lexed |> fromResult |> List.map fst |> Parse.parseList
            // // printf "%A" parsed

            // let result = parsed |> fromResult |> fst |> E.eval
            // Option.iter writeError result.error

            // // printf "%A" result
            let (a, b, c, d) = (L.Absolute(0.0, 0.0), L.Absolute(1.0, 0.0), L.Absolute(1.0, 1.0), L.Absolute (0.0, 1.0))
            let c2 = L.(@) (L.linkpp c d) 2.0
            let labels = Map.ofList [("a", a); ("b", b); ("c", c); ("d", d); ("c2", c2)]
            let border = [a |> L.linkpp b |> L.linksp c |> L.linksp d |> L.linksp a] // |> L.linksp a]
            let halfwayAB = L.(@) (L.linkpp a b) 0.5
            let halfwayCD = L.(@) (L.linkpp c d) 0.5
            // writeError border
            let cell = L.Primary <| border  @ [L.linkpp a c] @ [L.linkpp b d]
            let mirrored1 = L.Transformed (L.MirrorOver (L.ExtendSegment <| L.linkpp b c), cell)
            let mirrored2 = L.Transformed (L.MirrorOver (L.ExtendSegment <| L.linkpp d c), mirrored1)
            let repeated = L.Transformed (L.Repeat(L.linkpp d c2, L.C4, 3), mirrored2)
            let tessellation = L.Tessellation(repeated, [
                ([L.Any; L.Any; L.Ind 0], L.Color("#ccffdd"));
                ([L.Any; L.Any; L.Ind 1], L.Color ("#4dff88"));
                ([L.Any; L.Any; L.Ind 2], L.Color ("#009933"));
                ([L.Any; L.Any; L.Ind 3], L.Color ("#003311"))])

            let targets = {V.boundingHeight = 800.0; V.boundingWidth = 800.0; V.topLeft = (100.0, 100.0); V.xMax = 1000.0; V.yMax = 1000.0;}
            let drawable = V.solveTessellation targets tessellation labels |> fromResult

            // writeError drawable

            // let (drawable, errs) = V.drawableFromEvalResult result targets
            // List.iter writeError errs
            // printf "%A" drawable
            // printf "%A" errs
            ctx.clearRect(0.0, 0.0, 1000.0, 1000.0)

            List.iter (draw ctx {drawPoints = false}) drawable
        with 
            | e -> writeError e

    // Mutable variable to count the number of times we clicked the button
    // let mutable count = 0
    // https://github.com/fable-compiler/fable2-samples/blob/master/browser/src/App.fs
    let mutable myCanvas : Browser.Types.HTMLCanvasElement = unbox window.document.getElementById "canvas" // myCanvas is defined in public/index.html 
    myCanvas.width <- float 1000 
    myCanvas.height <- float 1000
    let ctx = myCanvas.getContext_2d()

    // Test case 1: Value is an Array, so we don't catch GeoExp!! let program = "[] 'a 'b 'c 'd = (:square)"
    let program = 
        """
        [] 'a 'b 'c 'd = (:square); 
        a + b + c + d + a ! 'k;
        """ 

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
