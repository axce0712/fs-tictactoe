type Field =
    | Empty
    | X
    | O

type Player =
    | X
    | O

type Point = { X: int; Y: int }

type Board = Field [,]

let emptyBoard = Array2D.create 3 3 Empty

let getAt point (board: Board) = board[point.Y, point.X]

let setAt point value (board: Board) = board[point.Y, point.X] <- value

let applyMove player point (board: Board) =
    let fieldValue =
        match player with
        | X -> Field.X
        | O -> Field.O

    let content = getAt point board

    if content <> Empty then
        Error "Field at given point not empty"
    else
        setAt point fieldValue board
        |> Ok


let checkForWinnerInArray (row: Field array) =
    let distinct = Array.distinct row
    match distinct with
    | [|f|] when f <> Empty -> Some f
    | _ -> None

let checkForWinner (board: Board) =
    let rowWinner =
        seq {0..2}
        |> Seq.map (fun i -> checkForWinnerInArray board[i, *])
    let columnWinner =
        seq {0..2}
        |> Seq.map (fun i -> checkForWinnerInArray board[*, i])
    let firstDiagonalWinner =
        seq {0..2}
        |> Seq.map (fun i -> board[i, i])
        |> Seq.toArray
        |> checkForWinnerInArray
        |> Seq.singleton

    let secondDiagonalWinner =
        Seq.zip {0..2} {2..-1..0}
        |> Seq.map(fun (y, x) -> board[y, x])
        |> Seq.toArray
        |> checkForWinnerInArray
        |> Seq.singleton

    rowWinner
    |> Seq.append columnWinner
    |> Seq.append firstDiagonalWinner
    |> Seq.append secondDiagonalWinner
    |> Seq.tryFind (fun o -> o.IsSome)


let theBoard: Field[,] = emptyBoard

applyMove X {X = 1; Y = 0} theBoard
applyMove O {X = 1; Y = 1} theBoard
applyMove X {X = 0; Y = 1} theBoard
applyMove O {X = 0; Y = 0} theBoard
applyMove X {X = 2; Y = 0} theBoard
applyMove O {X = 2; Y = 2} theBoard

printfn "%A" theBoard

printfn "%A" <| checkForWinner theBoard

let anotherBoard: Field[,] = emptyBoard

applyMove X {X = 2; Y = 0} anotherBoard
applyMove O {X = 0; Y = 1} anotherBoard
applyMove X {X = 1; Y = 1} anotherBoard
applyMove O {X = 2; Y = 1} anotherBoard
applyMove X {X = 0; Y = 2} anotherBoard

printfn "%A" anotherBoard

printfn "%A" <| checkForWinner anotherBoard

