namespace Tessa.App
open Tessa.View
open Tessa.View.Types
open Tessa.Solve
open Tessa.Eval
open Tessa.Eval.Types
open Tessa.Lex
open Tessa.Parse
open Tessa.Util

module App = 
    module V = View
    module V = ViewTypes
    module E = Eval 
    module E = EvalTypes
    module S = Solve
    module Lex = Lex 
    module Parse = Parse
    open Util

    open Browser.Dom
    open Fable.Core.JsInterop

    let draw (canvas: Browser.Types.CanvasRenderingContext2D) (shape: V.DrawShape) =
        match shape with 
        | V.DrawPoint(point, options) ->
            canvas.beginPath()
            canvas.arc(point.x, point.y, 10.0, 0.0, 0.0)
            canvas.fillStyle <- !^ options.color
            canvas.fill()
        | V.DrawSegment(segment, options) -> 
            canvas.beginPath()
            canvas.moveTo(segment.orig.x, segment.orig.y)
            canvas.lineTo(segment.dest.x, segment.dest.y)
            canvas.closePath()
            canvas.fillStyle <- !^ options.color
            canvas.fill()

    let fromResult = function 
        | Ok o -> o 
        | Error e -> failAndPrint e

    let go canvas program = 
        let lexAndParse s = 
            let lexed = fromResult <| Lex.lex s 
            List.map fst lexed |>  Parse.parseList |> fromResult |> fst
        
        let parsed = lexAndParse program

        let result = E.eval parsed

        let targets = {V.height = 1000.0; V.width = 1000.0; V.topLeft = (50.0, 50.0)}

        let (drawable, errs) = V.drawableFromEvalResult result (S.makeSolvers ()) targets

        List.iter (draw canvas) drawable

    // Mutable variable to count the number of times we clicked the button
    // let mutable count = 0

    let canvas = document.querySelector(".canvas") :?> Browser.Types.CanvasRenderingContext2D

    let program = 
        """
        [] 'a 'b 'c 'd = (:square);
        """

    go canvas program

    // Get a reference to our button and cast the Element to an HTMLButtonElement
    // let myButton = document.querySelector(".my-button") :?> Browser.Types.HTMLButtonElement

    // // Register our listener
    // myButton.onclick <- fun _ ->
    //     count <- count + 1
    //     myButton.innerText <- sprintf "You clicked: %i time(s)" count
