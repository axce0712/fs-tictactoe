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

type Board = private Board of Row * Row * Row

type Position =
    { Horizontal: HorizontalPosition
      Vertical: VerticalPosition }
      
module Board =
    let empty : Board =
        Board (
            (Empty, Empty, Empty),
            (Empty, Empty, Empty),
            (Empty, Empty, Empty))
            
    let private allPositions : Position list =
        [
            for hp in [ Left; HCenter; Right ] do
                for vp in [ Top; VCenter; Bottom ] do
                    { Horizontal = hp; Vertical = vp }
        ]

    let private getRow (position: VerticalPosition) (Board (r1, r2, r3)) : Row =
        match position with
        | Top -> r1
        | VCenter -> r2
        | Bottom -> r3

    let private setRow (position: VerticalPosition) (value: Row) (Board (r1, r2, r3)) : Board =
        match position with
        | Top     -> Board (value, r2, r3)
        | VCenter -> Board (r1, value, r3)
        | Bottom  -> Board (r1, r2, value)

    let private getCell (position: HorizontalPosition) ((c1, c2, c3) : Row) : Cell =
        match position with
        | Left -> c1
        | HCenter -> c2
        | Right -> c3
        
    let private setCell (position: HorizontalPosition) (value: Cell) ((c1, c2, c3) : Row) : Row =
        match position with
        | Left    -> (value, c2, c3)
        | HCenter -> (c1, value, c3)
        | Right   -> (c1, c2, value)

    let get (position: Position) (board: Board) : Cell =
        board
        |> getRow position.Vertical
        |> getCell position.Horizontal

    let set (position: Position) (value: Cell) (board: Board) : Board =
        let newRow =
            board
            |> getRow position.Vertical
            |> setCell position.Horizontal value
        
        setRow position.Vertical newRow board

    let tryMark (position: Position) (letter: Letter) (board: Board) : Board option =
        match get position board with
        | Empty -> set position (Marked letter) board |> Some
        | Marked _ -> None 

    let areSlotsRemaining (board: Board) : bool =
        allPositions
        |> List.exists (fun p ->
            match get p board with
            | Empty -> true
            | Marked _ -> false)

    let private renderCell (cell: Cell) : string =
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

type GameState =
    | InProgress of Letter
    | Winner of Letter
    | Tie

type Game = { Board: Board; State: GameState }

module Game =
    let initial = { Board = Board.empty; State = InProgress X }

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

    let private tryFindWinner (board: Board) : Letter option =
        let mapped =
            winningPositions
            |> List.map (fun (x, y, z) -> Board.get x board, Board.get y board, Board.get z board)

        mapped
        |> List.choose (fun (x, y, z) ->
            if x = y && y = z then
                match x with
                | Marked letter -> Some letter
                | Empty -> None
            else
                None)
        |> List.tryHead

    let private switch (letter: Letter) : Letter =
        match letter with
        | X -> O
        | O -> X

    let private moveWhenInProgress (position: Position) (current: Letter) (game: Game) : Game =
        match Board.tryMark position current game.Board with
        | Some newBoard ->
            let newState =
                match tryFindWinner newBoard, Board.areSlotsRemaining newBoard with
                | Some letter, _ -> Winner letter
                | None, false -> Tie
                | _ -> InProgress (switch current)

            { Board = newBoard; State = newState }
        | None -> game

    let move (position : Position) (game : Game) : Game =
        match game.State with
        | InProgress letter -> moveWhenInProgress position letter game
        | Winner _ -> game
        | Tie -> game

Game.initial
|> Game.move { Horizontal = Left; Vertical = Top }
|> Game.move { Horizontal = Left; Vertical = VCenter }
|> Game.move { Horizontal = HCenter; Vertical = VCenter }
|> Game.move { Horizontal = Right; Vertical = Bottom }
|> Game.move { Horizontal = Right; Vertical = Top }
|> Game.move { Horizontal = HCenter; Vertical = Top }
|> Game.move { Horizontal = Left; Vertical = Bottom }
|> fun game -> Board.render game.Board


