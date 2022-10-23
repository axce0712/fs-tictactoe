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

let emptyBoard : Board =
    Board (
        (Empty, Empty, Empty),
        (Empty, Empty, Empty),
        (Empty, Empty, Empty))

let get (pos: Position) (board: Board) : Cell =
    match board, pos with
    | Board ((x, _, _), _, _), { Horizontal = Left; Vertical = Top } -> x
    | Board ((_, x, _), _, _), { Horizontal = HCenter; Vertical = Top } -> x
    | Board ((_, _, x), _, _), { Horizontal = Right; Vertical = Top } -> x
    | Board (_, (x, _, _), _), { Horizontal = Left; Vertical = VCenter } -> x
    | Board (_, (_, x, _), _), { Horizontal = HCenter; Vertical = VCenter } -> x
    | Board (_, (_, _, x), _), { Horizontal = Right; Vertical = VCenter } -> x
    | Board (_, _, (x, _, _)), { Horizontal = Left; Vertical = Bottom } -> x
    | Board (_, _, (_, x, _)), { Horizontal = HCenter; Vertical = Bottom } -> x
    | Board (_, _, (_, _, x)), { Horizontal = Right; Vertical = Bottom } -> x

let set (pos: Position) (x: Cell) (board: Board) : Board =
    let newBoard =
        match board, pos with
        | Board ((_, c2, c3), r2, r3), { Horizontal = Left; Vertical = Top } -> (x, c2, c3), r2, r3
        | Board ((c1, _, c3), r2, r3), { Horizontal = HCenter; Vertical = Top } -> (c1, x, c3), r2, r3
        | Board ((c1, c2, _), r2, r3), { Horizontal = Right; Vertical = Top } -> (c1, c2, x), r2, r3
        | Board (r1, (_, c2, c3), r3), { Horizontal = Left; Vertical = VCenter } -> r1, (x, c2, c3), r3
        | Board (r1, (c1, _, c3), r3), { Horizontal = HCenter; Vertical = VCenter } -> r1, (c1, x, c3), r3
        | Board (r1, (c1, c2, _), r3), { Horizontal = Right; Vertical = VCenter } -> r1, (c1, c2, x), r3
        | Board (r1, r2, (_, c2, c3)), { Horizontal = Left; Vertical = Bottom } -> r1, r2, (x, c2, c3)
        | Board (r1, r2, (c1, _, c3)), { Horizontal = HCenter; Vertical = Bottom } -> r1, r2, (c1, x, c3)
        | Board (r1, r2, (c1, c2, _)), { Horizontal = Right; Vertical = Bottom } -> r1, r2, (c1, c2, x)

    Board newBoard

let tryMark (position: Position) (letter: Letter) (board: Board) : Board option =
    match get position board with
    | Empty -> set position (Marked letter) board |> Some
    | Marked _ -> None 

let allPositions : Position list =
    [
        for hp in [ Left; HCenter; Right ] do
            for vp in [ Top; VCenter; Bottom ] do
                { Horizontal = hp; Vertical = vp }
    ]

let winningPositions : (Position * Position * Position) list =
    [
        // Horizontals
        ({ Horizontal = Left; Vertical = Top },
         { Horizontal = HCenter; Vertical = Top },
         { Horizontal = Right; Vertical = Top })

        ({ Horizontal = Left; Vertical = VCenter },
         { Horizontal = HCenter; Vertical = VCenter },
         { Horizontal = Right; Vertical = VCenter })

        ({ Horizontal = Left; Vertical = Bottom },
         { Horizontal = HCenter; Vertical = Bottom },
         { Horizontal = Right; Vertical = Bottom })

        // Verticals
        ({ Horizontal = Left; Vertical = Top },
         { Horizontal = Left; Vertical = VCenter },
         { Horizontal = Left; Vertical = Bottom })

        ({ Horizontal = HCenter; Vertical = Top },
         { Horizontal = HCenter; Vertical = VCenter },
         { Horizontal = HCenter; Vertical = Bottom })

        ({ Horizontal = Right; Vertical = Top },
         { Horizontal = Right; Vertical = VCenter },
         { Horizontal = Right; Vertical = Bottom })

        // Diagonals
        ({ Horizontal = Left; Vertical = Top },
         { Horizontal = HCenter; Vertical = VCenter },
         { Horizontal = Right; Vertical = Bottom })

        ({ Horizontal = Right; Vertical = Top },
         { Horizontal = HCenter; Vertical = VCenter },
         { Horizontal = Left; Vertical = Bottom })
    ]

let tryFindWinner (board: Board) : Letter option =
    let mapped =
        winningPositions
        |> List.map (fun (x, y, z) -> get x board, get y board, get z board)

    mapped
    |> List.choose (fun (x, y, z) ->
        if x = y && y = z then
            match x with
            | Marked letter -> Some letter
            | Empty -> None
        else
            None)
    |> List.tryHead

let areSlotsRemaining (board: Board) : bool =
    allPositions
    |> List.exists (fun p ->
        match get p board with
        | Empty -> true
        | Marked _ -> false)

type GameState =
    | InProgress of Letter
    | Winner of Letter
    | Tie

type Game = { Board: Board; State: GameState }

let initialGame = { Board = emptyBoard; State = InProgress X }

let switch letter =
    match letter with
    | X -> O
    | O -> X

let nextState (current: Letter) (board: Board) : GameState =
    match tryFindWinner board, areSlotsRemaining board with
    | Some letter, _ -> Winner letter
    | None, false -> Tie
    | _ -> InProgress (switch current)

let move pos game =
    match game.State with
    | InProgress letter ->
        game.Board
        |> tryMark pos letter
        |> Option.map (fun b -> { Board = b; State = nextState letter b })
        |> Option.defaultValue game
    | Winner _ -> game
    | Tie -> game

let renderCell cell =
    match cell with
    | Empty -> " "
    | Marked X -> "X"
    | Marked O -> "O"
    
let render (Board ((a, b, c), (d, e, f), (g, h, i))) =
    $@"
{renderCell a}|{renderCell b}|{renderCell c}
-+-+-
{renderCell d}|{renderCell e}|{renderCell f}
-+-+-
{renderCell g}|{renderCell h}|{renderCell i}"

initialGame
|> move { Horizontal = Left; Vertical = Top }
|> move { Horizontal = Left; Vertical = VCenter }
|> move { Horizontal = HCenter; Vertical = Top }
|> move { Horizontal = HCenter; Vertical = VCenter }
|> move { Horizontal = Right; Vertical = Top }
|> move { Horizontal = Right; Vertical = VCenter }
|> fun game -> render game.Board


