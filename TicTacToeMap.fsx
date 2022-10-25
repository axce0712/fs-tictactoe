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

type Position =
    { Horizontal: HorizontalPosition
      Vertical: VerticalPosition }

type Cell =
    | Empty
    | Marked of Letter

type Board = private { Value: Map<Position, Cell> }

let emptyBoard : Board =
    let m = Map.ofList [
        for vp in [ Top; VCenter; Bottom ] do
            for hp in [ Left; HCenter; Right ] do
                ({ Horizontal = hp; Vertical = vp }, Empty)
    ]

    { Value = m }

let get (position: Position) (board: Board) : Cell =
    Map.find position board.Value

let set (position: Position) (value: Cell) (board: Board) : Board =
    let m = Map.add position value board.Value
    { Value = m }

let tryMark (position: Position) (letter: Letter) (board: Board) : Board option =
    match get position board with
    | Empty -> set position (Marked letter) board |> Some
    | Marked _ -> None

let anySlotsRemaining (board: Board) : bool =
    Map.values board.Value
    |> Seq.exists (function Empty -> true | Marked _ -> false)

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

let switch (letter: Letter) : Letter =
    match letter with
    | X -> O
    | O -> X

type GameState =
    | InProgress of Letter
    | Winner of Letter
    | Tie

type Game = { Board: Board; State: GameState }

let initial = { Board = emptyBoard; State = InProgress X }

let move (position : Position) (game : Game) : Game =
    match game.State with
    | InProgress letter ->
        match tryMark position letter game.Board with
        | Some newBoard ->
            let newState =
                match tryFindWinner newBoard, anySlotsRemaining newBoard with
                | Some letter, _ -> Winner letter
                | None, false -> Tie
                | _ -> InProgress (switch letter)

            { Board = newBoard; State = newState }
        | None -> game
    | Winner _ -> game
    | Tie -> game

let printCell = function
    | Empty -> " "
    | Marked X -> "X"
    | Marked O -> "O"

let print (board: Board) =
    Map.toSeq board.Value
    |> Seq.sortBy (fun (p, _) -> p.Vertical, p.Horizontal)
    |> Seq.map (snd >> printCell)
    |> Seq.chunkBySize 3
    |> Seq.map (String.concat " | ")
    |> String.concat "\n--+---+--\n"

initial
|> move { Horizontal = Left; Vertical = Top }
|> move { Horizontal = Left; Vertical = VCenter }
|> move { Horizontal = HCenter; Vertical = VCenter }
|> move { Horizontal = Right; Vertical = Bottom }
|> move { Horizontal = Right; Vertical = Top }
|> move { Horizontal = HCenter; Vertical = Top }
|> move { Horizontal = Left; Vertical = Bottom }
|> fun game -> "\n" + print game.Board