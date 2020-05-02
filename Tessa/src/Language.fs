namespace Tessa.Language

open Microsoft.FSharp.Collections
open System

module Language = 
    type Rotation =
        | C2
        | C3
        | C4
        | C6

    type RotationDirection =
     | Clockwise
     | CounterClockwise

    type Point = 
        | Absolute of x: double * y: double
        | Operated of origin: Point * operation: Operation
        | OnSegment of PointOnSegment
        | Intersection of Line * Line
        static member ToSegmentable p1 = SinglePoint(p1)
        static member Expression p1 = PointExp(p1)

    and PointOnSegment =
        | PointOnSegment of position: double * segment: Segment

    and Segment = 
        | Link of Point * Point
        | Chain of Segment * Point
        | ReverseChain of Point * Segment
        | Concat of Segment * Segment
        | Perpendicular of  position: double * originSegment: Segment * endSegment: Segment
        | Snipped of original: Segment * cutAt: Segment
        // | QuadraticBezier of orig: Point * control: Point * dest: Point
        // | FocalSplit of farSegment: Segment * nearSegment: Segment * focal: Point * numberPolygons: int
        static member ToSegmentable s = AlreadySegment(s)
        static member Expression s = SegmentExp(s)
        static member ToLine s = ExtendSegment(s)

    and CanMakeSegment = 
        | AlreadySegment of Segment
        | SinglePoint of Point

    // Lines are inifinite and Segments are finite
    and Line =
        | Perpendicular of position: double * segment: Segment
        | VerticalThroughX of xPosition: double * segment: Segment
        | HorizontalThroughY of yPosition: double * segment: Segment
        | ExtendSegment of Segment
        static member ToLine l = l

    and Operation =
        | GlideAround of pinned: Point * ``from``: Point * ``to``: Point
        | Rotate of direction: RotationDirection * angle: Rotation * point: Point

    and Polygon = 
        | Up of PointOnSegment // must be on line
        | Down of PointOnSegment // must be on line
        | Segments of Segment list
        static member Expression p = PolygonExp(p)

    and Expression =
        | PointExp of Point
        | SegmentExp of Segment
        | PolygonExp of Polygon
        | VarExp of name: string * Expression 

    let up location segment = Up <| PointOnSegment(location, segment)
    let down location segment = Down <| PointOnSegment(location, segment)

    let inline asSegment (p:^t) = (^t: (static member ToSegment: ^t -> Segment) (p))
    let inline asSegmentable (p:^t) = (^t: (static member ToSegmentable: ^t -> CanMakeSegment) (p))
    // let inline (+) p q = Concat(asSegment p, asSegment q)
    let inline add p q = 
        let ps, qs = asSegmentable p, asSegmentable q
        match ps, qs with 
            | (SinglePoint x, SinglePoint y) -> Link(x, y)
            | (SinglePoint x, AlreadySegment y) -> Chain(y, x)
            | (AlreadySegment x, SinglePoint y) -> Chain(x, y)
            | (AlreadySegment x, AlreadySegment y) -> Concat(x, y)
    let inline (+) p q = add p q

    let inline asLine (x:^t) = (^t: (static member ToLine: ^t -> Line) (x))
    let inline (*) x y = Intersection(asLine x, asLine y)

    let (@) segment position = OnSegment(PointOnSegment(position, segment))

    let (-|) segment position = Line.Perpendicular(position, segment)
    let (-|>) segment (position, endSegment) = Segment.Perpendicular(position, segment, endSegment)

    let X position segment = VerticalThroughX(position, segment)
    let Y position segment = HorizontalThroughY(position, segment)

    let (|-|) orig cutAt = Snipped(orig, cutAt)

    let (%) p r = Operated(p, r)


    let inline Expression (x:^t) = (^t: (static member Expression: ^t -> Expression) (x))
    let var name x = VarExp(name, Expression x)

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

    type Builder<'a> = 'a -> Segment list * Polygon list * Expression list

    let splitFocal2 (focal: Point) (farBorder: Segment) (nearBorder: Segment) =
        let segmentAt at = (farBorder @ at) + focal |-| nearBorder 
        // let downFrom at = segmentAt at |> down 0.5
        // let upFrom at = segmentAt at |> up 0.5
        (segmentAt 0.33, segmentAt 0.66)

    let splitFocal3 (focal: Point) (farBorder: Segment) (nearBorder: Segment) =
        let segmentAt at = (farBorder @ at) + focal |-| nearBorder 
        // let downFrom at = segmentAt at |> down 0.5
        // let upFrom at = segmentAt at |> up 0.5
        (segmentAt 0.25, segmentAt 0.5, segmentAt 0.75)

    let splitFocal4 (focal: Point) (farBorder: Segment) (nearBorder: Segment) =
        let segmentAt at = (farBorder @ at) + focal |-| nearBorder 
        // let downFrom at = segmentAt at |> down 0.5
        // let upFrom at = segmentAt at |> up 0.5
        (segmentAt 0.2, segmentAt 0.4, segmentAt 0.6, segmentAt 0.8)

    let splitFocal5 (focal: Point) (farBorder: Segment) (nearBorder: Segment) =
        let segmentAt at = (farBorder @ at) + focal |-| nearBorder 
        // let downFrom at = segmentAt at |> down 0.5
        // let upFrom at = segmentAt at |> up 0.5
        (segmentAt 0.166, segmentAt 0.33, segmentAt 0.499, segmentAt 0.66, segmentAt 0.813)



module Examples = 
    open Language
    open PrimitiveCells

    // Designing Tessellations p175
    let buildWildOneCell:Builder<Square> = fun square ->
        let {TopLeft =a; TopRight= b; BottomRight=c; BottomLeft=d;} = square

        let i1 = a + b -|> (0.25, d + c) @ 0.25 
        let i2 = b + a -|> (0.33, c + d) @ 0.33

        let r1 = Rotate(Clockwise, C4, d)
        let r2 = Rotate(Clockwise, C4, b)

        let leftBorder = (i2 % r2) + a + i1 + d + (i1 % r1)
        let rightBorder = (i2 % r2) + b + i2 + c + (i1 % r1)
        let border = leftBorder + rightBorder

        let focal = a + d -|> (0.5, b + c) @ 2.0

        let (s1, s2, s3, s4, s5) = splitFocal5 focal leftBorder rightBorder

        let (p1, p2, p3, p4, p5, p6) = (down 0.5 s1, down 0.5 s2, down 0.5 s3, down 0.5 s4, down 0.5 s5, up 0.5 s5)

        ([border; s1; s2; s3; s4; s5], [p1; p2; p3; p4; p5; p6], [var "i1" i1; var "i2" i2; var "i1 % r1" <| i1 % r1; var "i2 % r2" <| i2 % r2])


    // Designing Tessellations p176
    // let buildSoaringHighCell:Builder<IsoscelesTriangle> =  fun triangle ->
    //     // a
    //     // |\
    //     // |   \
    //     // |     \
    //     // |       \  b
    //     // |      /
    //     // |    /
    //     // |  /
    //     // |/
    //     //  c
    //     let {BottomLeft = a; Top = b; BottomRight = c;} = triangle

    //     let i1 = (a + c |> Y 0.25) * (a + b |> X 0.25)
    //     let i2 = (a + c |> Y 0.375) * (a + b |> X 0.125)
    //     let i3 = (a + c |> Y 0.5) * (a + b |> X 0.375)
    //     let r1 = GlideAround(b, c, a)

    //     let control = c + b -|> (0.5, a + b) @ 0.3

    //     let topBorder = a + i1 + i2 + i3 + (a + b @ 0.625) + (i3 % r1) + (i2 % r1) + (i1 % r1) + c + QuadraticBezier(b, control % r1, a)
    //     let lowerBorder = QuadraticBezier(c, control, b)

    //     let focal = (a + c |> Y 1.3) * (c + b |> X 0.5)

    //     let (s1, s2) = splitFocal2 focal topBorder lowerBorder
    //     let (p1, p2, p3) = (down 0.5 s1, down 0.5 s2, up 0.5 s2)

    //     ([topBorder + lowerBorder; s1; s2], [p1; p2; p3], [var "i1" i1; var "i2" i2; var "i3" i3; var "c" control; var "c%r1" <| control % r1])

    // Designing Tessellations p71 -- Mosaic floor in Amber Palace in Jaipur, India
    let buildTriangleMosiacCell:Builder<IsoscelesTriangle> = fun triangle ->
        let {BottomLeft = a; Top = b; BottomRight = c;} = triangle

        let down1 = (a + b @ 0.33) + (a + c @ 0.5)
        let down2 = (c + b @ 0.33) + (c + a @ 0.5)
        let middleUpVertex = ((a + c |> X 0.5) * (a + b |> Y 0.75))
        let upChain = (a + c @ 0.25) + (a + b @ 0.66) + middleUpVertex + (c + b @ 0.66) + (c + a @ 0.25)
        let finalUp = b + middleUpVertex

        // Polygons on row 1
        let (p1, p2, p3, p4) = (up 0.1 <| a + c, up 0.4 <| a + c, up 0.6 <| a + c, up 0.9 <| a + c)
        // Row 2
        let (p5, p6, p7) = (up 0.1 down1, up 0.9 down1, up 0.1 down2)
        // Row 3
        let (p8, p9) = (down 0.9 <| a + b, down 0.9 <| c + b)

        ([a + b + c; down1 + down2; upChain; finalUp], [p1;p2;p3;p4;p5;p6;p7;p8;p9], [])

