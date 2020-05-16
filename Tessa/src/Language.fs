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

    and PointOnSegment =
        | PointOnSegment of position: double * segment: Segment

    and Segment = 
        | Link of Point * Point
        | Chain of Segment * Point
        | ReverseChain of Point * Segment
        | Concat of Segment * Segment
        | Perpendicular of  position: double * originSegment: Segment * endSegment: Segment
        | Snipped of original: Segment * cutAt: Segment
        static member ToSegmentable s = AlreadySegment(s)
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

    type CellOp =
        | MirrorOver of Segment
        | RepeatC4 of span: Segment * times: int
        | Repeat of span: Segment * rotation: Rotation * int 

    type Cell = 
        | Primary of Segment list 
        | Combined of (int * Cell) list 
        | Transformed of CellOp * Cell
        | Embedding of primary: Cell * secondary: Cell * Index list

    type Index = 
        | AllEndingAt of int
        | Ind of int
        | ManyInd of int list
        | Any

    type Effect = 
        | Color of color: string 
        // | Embed of Tessellation 

    and Tessellation = Tessellation of Cell *  (Index list * Effect) list 

    let doubleMirror horizLine vertLine cell = 
        let left = cell
        let right = Transformed(MirrorOver vertLine, cell)
        let bottomRight = Transformed(MirrorOver horizLine, right)
        let bottomLeft =  Transformed(MirrorOver horizLine, cell)
        Combined [(0, left); (1, right); (2, bottomRight); (3, bottomLeft)]




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

    let linkpp p q = Link(q, p)
    let linkps s p = ReverseChain(p, s)
    let linksp p s = Chain(s, p)
    let linkss s r = Concat(r, s)

    let inline asLine (x:^t) = (^t: (static member ToLine: ^t -> Line) (x))
    let inline (*) x y = Intersection(asLine x, asLine y)

    let (@) segment position = OnSegment(PointOnSegment(position, segment))

    let (-|) segment position = Line.Perpendicular(position, segment)
    let (-|>) segment (position, endSegment) = Segment.Perpendicular(position, segment, endSegment)

    let X position segment = VerticalThroughX(position, segment)
    let Y position segment = HorizontalThroughY(position, segment)

    let (|-|) orig cutAt = Snipped(orig, cutAt)

    let (%) p r = Operated(p, r)


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

    type Builder<'a> = 'a -> Segment list

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

        [border; s1; s2; s3; s4; s5]
    
    // page 57, bottom left
    let buildSample:Builder<Square> = fun square ->
        // A recursive tessellation:
        // 'makeIt = ('on-color => '(
        // 'cell = (:cell)
        // [] 'a 'b 'c 'd = (:square cell)
        //
        // a + b + c + d + a !;
        // (a + d @ 1/2) + (a + b @ 1/2) + (b + c @ 1/2) + (a + d @ 1/2) !;
        // (a + d @ 0.5) + (c + d @ 0.5) !;
        // a + d |- 1/2 (b + c) @ 1/2 + c !;
        //
        // cell :doubleMirror (c + d) (b + c);));
        //
        // 'inner = (:makeIt :color 'purple _ ([] 1 4));
        // 'outer = (:makeIt :embed inner 1/10 _ ([] 1 4));
        // outer <#> 10 10;
        //
        // An overlay:
        // 'cell1 = (:cell)
        // 'cell2 = (:cell)
        // ... (:square cell1) ... (:square cell2) ... 
        // cell1 <#> 1 2 ; 
        // cell2 <#> 20 40;
        // So ! adds to a cell, says it's relevant rather than just an assignment. Uses cell passed into primitive like :square or :triangle.
        // <#> tessellates given an x-repetition y-repetition. Overlays work by fitting both to screen, accounting for size.
        //
        // Evaluator's goal is just to build this structure that can be passed off to the Solver and Viewer without them caring where it came from.
        // So Runtime shouldn't have a Solver.
        // 
        // Evaluator generates: bound structures (points, segments) and their labels; the current expression; the cells.

        let {TopLeft =a; TopRight= b; BottomRight=c; BottomLeft=d;} = square
        let border = a + b + c + d + a
        let innerTriangle = (a + d @ 0.5) + (a + b @ 0.5) + (b + c @ 0.5) + (a + d @ 0.5)
        let leftDiagDown = (a + d @ 0.5) + (c + d @ 0.5)
        let rightDiagDown = (a + d -|> (0.5, b + c) @ 0.5) + c

        // we've done some !'s.
        // (:square 'cell)
        // ...
        // cell :doubleMirror (c + d) (b + c)
        // let cell = doubleMirror (asLine (c + d)) (asLine (b + c)) <| Primary [border; innerTriangle; leftDiagDown; rightDiagDown]

        // let effects = [
        //     ([Any; ManyInd [1;4]], Color("purple"))
        //     // Indices are just concatenated
        //     // if we embed, we'd have double-mirror -> polygon -> double-mirror -> polygon
        //     // So solve can rewrite this and produce new effects without touching the actual effects
        //     // Or we can have a pass that rewrites and calls into solve, more accurately
        //     ([Any; ManyInd [0;2;3;5]], Embed(cell, 0.33, [([Any; ManyInd [1;4]], Color("purple"))]))
        // ]

        // let tessellation = Tessellation(cell, effects)

        []


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

        [a + b + c; down1 + down2; upChain; finalUp]


// variadic args -- can take record or multi args for implicit record
// if 2 args, returns val instead of record with bindings.
// <!> '(
//   [] 'a 'b 'c 'd = (:square);
//   c + d @ 2.0 
//     is 'c2;
//   a + b + c + d + a !;
//   a + c !;
//   b + d !;
//   :double-mirror (b + c) (d + c);
//   :repeat-c4 (d + (c + d @ 2.0)) 3 ;
//   &!;
// ) is 'cell;
//
// <#> '(
//    # _ 0 'ccffdd;
//    # _ 1 '4dff88;
//    # _ 2 '009933;
//    # _ 3 '003311;
// ) is 'effects;

// tessa cell effects;

// Then we can have primitive syntactic sugar operators <!> and <#> ?
// For '&! (:cell) <> '( ... ), we have <!> '( ...)
