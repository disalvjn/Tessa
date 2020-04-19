namespace Tessellations.Language

open Microsoft.FSharp.Collections
open System

module Language = 
    type Rotation =
        | C2
        | C3
        | C4
        | C6 

    type Point = 
        | Absolute of x: int * y: int
        | Operated of origin: Point * operation: Operation
        | OnSegment of PointOnSegment
        static member toSegment (p1, p2) = LinkPoints(p1, p2)
        static member expression p1 = PointExp(p1)

    and PointOnSegment =
        | PointOnSegment of position: double * segment: Segment

    and Segment = 
        | LinkPoints of ``from``: Point * ``to``: Point
        | Perpendicular of  position: double * originSegment: Segment * endSegment: Segment
        | Chain of ``from``: Segment * ``to``: Point
        | Concat of Segment * Segment
        | QuadraticBezier of ``from``: Point * ``to``: Point * control: Point
        | Snipped of original: Segment * cutAt: Segment
        static member toSegment (s, p2) = Chain(s, p2)
        static member expression s = SegmentExp(s)

    // Lines are inifinite and Segments are finite
    and Line =
        | Perpendicular of position: double * segment: Segment
        | X of xPosition: double * segment: Segment
        | Y of yPosition: double * segment: Segment

    and Operation =
        | Translate
        | Glide
        | Rotate of angle: Rotation * point: Point
        | Mirror

    and Polygon = 
        | Up of PointOnSegment // must be on line
        | Down of PointOnSegment // must be on line
        | Segments of Segment list
        static member expression p = PolygonExp(p)

    and Expression =
        | PointExp of Point
        | SegmentExp of Segment
        | PolygonExp of Polygon
        | VarExp of name: string * Expression 


    let up location segment = Up <| PointOnSegment(location, segment)
    let down location segment = Down <| PointOnSegment(location, segment)
    let (++) segment location = up location segment
    let (--) segment location = down location segment


    let inline (=>) (p:^t) (q:Point) = (^t: (static member toSegment: ^t * Point -> Segment) (p, q))
    let (+) s1 s2= Concat(s1, s2)

    // let At position segment = PointOnSegment(position, segment)
    // TODO: remember that this needs to take directionality into account somehow
    let (@) segment position = OnSegment(PointOnSegment(position, segment))

    let (-|) segment position = Line.Perpendicular(position, segment)
    let (-|>) segment (position, endSegment) = Segment.Perpendicular(position, segment, endSegment)
    // let T segment position = Line.Perpendicular(position, segment)
    // let T2 segment position endSegment = Segment.Perpendicular(position, segment, endSegment)

    let (|-|) orig cutAt = Snipped(orig, cutAt)

    let (%) p r = Operated(p, r)

    let inline expression (x:^t) = (^t: (static member expression: ^t -> Expression) (x))
    let var name x = VarExp(name, expression x)

module PrimitiveCells = 
    open Language

    // Primitive Cells
    type Square = {
        TopLeft: Point;
        TopRight: Point;
        BottomRight: Point;
        BottomLeft: Point;
    }

    type IsoscelesTriangle = {
        // BottomLeft -> Top and BottomRight -> Top are the same length.
        BottomLeft: Point;
        BottomRight: Point;
        Top: Point;
    }

    type Builder<'a> = 'a -> Polygon list * Expression list


module Example = 
    open Language
    open PrimitiveCells

    // Designing Tessellations p175
    let buildWildOne:Builder<Square> = fun square ->
        let {TopLeft =a; TopRight= b; BottomRight=c; BottomLeft=d;} = square

        let i1 = (a => b) -|> (0.25, d => c) @ 0.25 
        let i2 = (b => a) -|> (0.33, c => d) @ 0.33

        let r1 = Rotate(C4, d)
        let r2 = Rotate(C4, b)

        let leftBorder = i2 % r2 => b => i2 => c => i1 % r1
        let rightBorder = i2 % r2 => a => i1 => b => i1 % r1
        let border = leftBorder + rightBorder

        let focal = (a => d) -|> (0.5, leftBorder) @ 2.0

        let downFrom at = focal => leftBorder @ at |-| rightBorder |> down 0.5
        let upFrom at = focal => leftBorder @ at |-| rightBorder |> up 0.5

        let p1 = downFrom <| 1.0/6.0
        let p2 = downFrom <| 2.0/6.0
        let p3 = downFrom <| 3.0/6.0
        let p4 = downFrom <| 4.0/6.0
        let p5 = downFrom <| 5.0/6.0
        let p6 = upFrom <| 5.0/6.0

        ([p1; p2; p3; p4; p5; p6], [var "i1" i1; var "i2" i2])

    // Designing Tessellations p176
    let buildSoaringHigh:Builder<IsoscelesTriangle> =  fun triangle ->
        let {BottomLeft = a; Top = b; BottomRight = c;} = triangle


        ([], [])





    // let p2 = focal =~ segArc @ 2.0/6.0 -- 0.5
    // let p3 = focal =~ segArc @ 3.0/6.0 -- 0.5
    // let p4 = focal =~ segArc @ 4.0/6.0 -- 0.5
    // let p5 = focal =~ segArc @ 5.0/6.0 -- 0.5
    // let p6 = focal =~ segArc @ 5.0/6.0 ++ 0.5
    

    
