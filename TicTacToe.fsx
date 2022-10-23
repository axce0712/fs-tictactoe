type Letter =
    | X
    | O

type HorizontalPosition =
    | Left
    | HCenter
    | Right

type VerticalPosition =
    | Top
    | VCenter
    | Bottom

type Cell =
    | Empty
    | Marked of Letter

type Row = Cell * Cell * Cell

type Board = Board of Row * Row * Row

type Position =
    { Horizontal: HorizontalPosition
      Vertical: VerticalPosition }

let emptyBoard =
    Board (
        (Empty, Empty, Empty),
        (Empty, Empty, Empty),
        (Empty, Empty, Empty))

let get (pos: Position) (Board (rt, rc, rb)) : Cell =
    match rt, rc, rb, pos with
    | (x, _, _), _, _, { Horizontal = Left; Vertical = Top } -> x
    | (_, x, _), _, _, { Horizontal = HCenter; Vertical = Top } -> x
    | (_, _, x), _, _, { Horizontal = Right; Vertical = Top } -> x
    | _, (x, _, _), _, { Horizontal = Left; Vertical = VCenter } -> x
    | _, (_, x, _), _, { Horizontal = HCenter; Vertical = VCenter } -> x
    | _, (_, _, x), _, { Horizontal = Right; Vertical = VCenter } -> x
    | _, _, (x, _, _), { Horizontal = Left; Vertical = Bottom } -> x
    | _, _, (_, x, _), { Horizontal = HCenter; Vertical = Bottom } -> x
    | _, _, (_, _, x), { Horizontal = Right; Vertical = Bottom } -> x

let set (pos: Position) (x: Cell) (Board (rt, rc, rb)) : Board =
    let newBoard =
        match rt, rc, rb, pos with
        | (_, c2, c3), r2, r3, { Horizontal = Left; Vertical = Top } -> (x, c2, c3), r2, r3
        | (c1, _, c3), r2, r3, { Horizontal = HCenter; Vertical = Top } -> (c1, x, c3), r2, r3
        | (c1, c2, _), r2, r3, { Horizontal = Right; Vertical = Top } -> (c1, c2, x), r2, r3
        | r1, (_, c2, c3), r3, { Horizontal = Left; Vertical = VCenter } -> r1, (x, c2, c3), r3
        | r1, (c1, _, c3), r3, { Horizontal = HCenter; Vertical = VCenter } -> r1, (c1, x, c3), r3
        | r1, (c1, c2, _), r3, { Horizontal = Right; Vertical = VCenter } -> r1, (c1, c2, x), r3
        | r1, r2, (_, c2, c3), { Horizontal = Left; Vertical = Bottom } -> r1, r2, (x, c2, c3)
        | r1, r2, (c1, _, c3), { Horizontal = HCenter; Vertical = Bottom } -> r1, r2, (c1, x, c3)
        | r1, r2, (c1, c2, _), { Horizontal = Right; Vertical = Bottom } -> r1, r2, (c1, c2, x)

    Board newBoard

let change (f: Cell -> Cell) (position: Position) (board: Board) : Board =
    set position (f (get position board)) board

let tryMark letter =
    let modify cell =
        match cell with
        | Empty -> Marked letter
        | _ -> cell

    change modify

let allPositions =
    [
        for hp in [ Left; HCenter; Right ] do
            for vp in [ Top; VCenter; Bottom ] do
                { Horizontal = hp; Vertical = vp }
    ]

let winningPositions =
    [
        // Horizontals
        { Horizontal = Left; Vertical = Top }, { Horizontal = HCenter; Vertical = Top }, { Horizontal = Right; Vertical = Top }
        { Horizontal = Left; Vertical = VCenter }, { Horizontal = HCenter; Vertical = VCenter }, { Horizontal = Right; Vertical = VCenter }
        { Horizontal = Left; Vertical = Bottom }, { Horizontal = HCenter; Vertical = Bottom }, { Horizontal = Right; Vertical = Bottom }

        // Verticals
        { Horizontal = Left; Vertical = Top }, { Horizontal = Left; Vertical = VCenter }, { Horizontal = Left; Vertical = Bottom }
        { Horizontal = HCenter; Vertical = Top }, { Horizontal = HCenter; Vertical = VCenter }, { Horizontal = HCenter; Vertical = Bottom }
        { Horizontal = Right; Vertical = Top }, { Horizontal = Right; Vertical = VCenter }, { Horizontal = Right; Vertical = Bottom }

        // Diagonals
        { Horizontal = Left; Vertical = Top }, { Horizontal = HCenter; Vertical = VCenter }, { Horizontal = Right; Vertical = Bottom}
        { Horizontal = Right; Vertical = Top }, { Horizontal = HCenter; Vertical = VCenter }, { Horizontal = Left; Vertical = Bottom}
    ]

let tryFindWinner board =
    let mapped =
        winningPositions
        |> List.map (fun (x, y, z) -> get x board, get y board, get z board)

    mapped
    |> List.choose (fun (x, y, z) ->
        if x = y && y = z then
            match x with
            | Marked m -> Some m
            | Empty -> None
        else
            None)
    |> List.tryHead

let areSlotsRemaining board =
    allPositions
    |> List.exists (fun p ->
        match get p board with
        | Empty -> true
        | Marked _ -> false)

type GameState =
    | InProgress
    | Winner of Letter
    | Tie

let outcome (board: Board) : GameState =
    match tryFindWinner board, areSlotsRemaining board with
    | Some letter, _ -> Winner letter
    | None, true -> Tie
    | _ -> InProgress

let renderCell cell =
    match cell with
    | Empty -> " "
    | Marked X -> "X"
    | Marked O -> "O"

let switch letter =
    match letter with
    | X -> O
    | O -> X

let render (Board ((a, b, c), (d, e, f), (g, h, i))) =
    $@"
{renderCell a}|{renderCell b}|{renderCell c}
-+-+-
{renderCell d}|{renderCell e}|{renderCell f}
-+-+-
{renderCell g}|{renderCell h}|{renderCell i}"

emptyBoard
|> tryMark X { Horizontal = Left; Vertical = Top }
|> tryMark O { Horizontal = Left; Vertical = VCenter }
|> render 
