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
        | Intersection of Line * Line
        static member toSegment p1 = Singlet(p1)
        static member expression p1 = PointExp(p1)

    and PointOnSegment =
        | PointOnSegment of position: double * segment: Segment

    and Segment = 
        | Singlet of Point
        | Perpendicular of  position: double * originSegment: Segment * endSegment: Segment
        | Concat of Segment * Segment
        | QuadraticBezier of ``from``: Point * ``to``: Point * control: Point
        | Snipped of original: Segment * cutAt: Segment
        static member toSegment s = s
        static member expression s = SegmentExp(s)
        static member toLine s = ExtendSegment(s)

    // Lines are inifinite and Segments are finite
    and Line =
        | Perpendicular of position: double * segment: Segment
        | AtX of xPosition: double * segment: Segment
        | AtY of yPosition: double * segment: Segment
        | ExtendSegment of Segment
        static member toLine l = l

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

    let inline asSegment (p:^t) = (^t: (static member toSegment: ^t -> Segment) (p))
    let inline (+) p q = Concat(asSegment p, asSegment q)

    let inline asLine (x:^t) = (^t: (static member toLine: ^t -> Line) (x))
    let inline (*) x y = Intersection(asLine x, asLine y)

    let (@) segment position = OnSegment(PointOnSegment(position, segment))

    let (-|) segment position = Line.Perpendicular(position, segment)
    let (-|>) segment (position, endSegment) = Segment.Perpendicular(position, segment, endSegment)

    let X position segment = AtX(position, segment)
    let Y position segment = AtY(position, segment)

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


module Examples = 
    open Language
    open PrimitiveCells

    // Designing Tessellations p175
    let buildWildOne:Builder<Square> = fun square ->
        let {TopLeft =a; TopRight= b; BottomRight=c; BottomLeft=d;} = square

        let i1 = a + b -|> (0.25, d + c) @ 0.25 
        let i2 = b + a -|> (0.33, c + d) @ 0.33

        let r1 = Rotate(C4, d)
        let r2 = Rotate(C4, b)

        let leftBorder = (i2 % r2) + b + i2 + c + (i1 % r1)
        let rightBorder = (i2 % r2) + a + i1 + b + (i1 % r1)
        let border = leftBorder + rightBorder

        let focal = a + d -|> (0.5, leftBorder) @ 2.0

        let downFrom at = focal + (leftBorder @ at) |-| rightBorder |> down 0.5
        let upFrom at = focal + (leftBorder @ at) |-| rightBorder |> up 0.5

        let p1 = downFrom <| 1.0/6.0
        let p2 = downFrom <| 2.0/6.0
        let p3 = downFrom <| 3.0/6.0
        let p4 = downFrom <| 4.0/6.0
        let p5 = downFrom <| 5.0/6.0
        let p6 = upFrom <| 5.0/6.0

        ([p1; p2; p3; p4; p5; p6], [var "i1" i1; var "i2" i2; var "i1 % r1" <| i1 % r1; var "i2 % r2" <| i2 % r2])

    // Designing Tessellations p176
    let buildSoaringHigh:Builder<IsoscelesTriangle> =  fun triangle ->
        // a
        // |\
        // |   \
        // |     \
        // |       \  b
        // |      /
        // |    /
        // |  /
        // |/
        //  c
        let {BottomLeft = a; Top = b; BottomRight = c;} = triangle

        let i1 = (a + c |> Y 0.25) * (a + b |> X 0.25)
        let i2 = (a + c |> Y 0.375) * (a + b |> X 0.125)
        let i3 = (a + c |> Y 0.5) * (a + b |> X 0.375)
        let r1 = Rotate(C2, (a + b @ 0.5))

        let leftBorder = a + i1 + i2 + i3 + (a + b @ 0.625) + (i3 % r1) + (i2 % r1) + (i1 % r1) + c


        ([], [])
