namespace TicTacToe.Avalonia

module TicTacToe =
    open Avalonia.Controls
    open Avalonia.FuncUI
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open Domain

    let getY = function
        | Top -> 0
        | VCenter -> 1
        | Bottom -> 2

    let getX = function
        | Left -> 0
        | HCenter -> 1
        | Right -> 2

    let renderLetter letter =
        match letter with
        | X -> "X"
        | O -> "O"

    let renderCellValue cell =
        match cell with
        | Empty -> ""
        | Marked letter -> renderLetter letter

    let renderCell f winningPositions state position cell =
        Button.create [
            Button.content (renderCellValue cell)
            Button.horizontalAlignment HorizontalAlignment.Stretch
            Button.verticalAlignment VerticalAlignment.Stretch
            Button.horizontalContentAlignment HorizontalAlignment.Center
            Button.verticalContentAlignment VerticalAlignment.Center
            Button.column (getX position.Horizontal)
            Button.row (getY position.Vertical)
            Button.onClick (fun _ -> f position)
            Button.isHitTestVisible (match state with | InProgress _ -> true | _ -> false)
            let backgroundColor =
                if List.contains position winningPositions then
                    "#FFFFCC00"
                else if cell <> Empty then
                    "#FF589FF3"
                else
                    "#FFDDDDDD"
            Button.background backgroundColor
        ]

    let renderBoard f game =
        Grid.create [
            Grid.columnDefinitions "*,*,*"
            Grid.rowDefinitions "*,*,*"
            Grid.children [
                for vp in [ Top; VCenter; Bottom ] do
                    for hp in [ Left; HCenter; Right ] do
                        let position = { Horizontal = hp; Vertical = vp }
                        let cell = get { Horizontal = hp; Vertical = vp } game.Board
                        renderCell f game.WinningPositions game.State position cell
            ]
        ]

    let renderGameState state =
        match state with
        | InProgress letter -> $"Player %s{renderLetter letter} turn"
        | Winner letter -> $"Player %s{renderLetter letter} won"
        | Tie -> "No winner"

    let view : Component =
        Component(fun ctx ->
            let state = ctx.useState(initial)
            DockPanel.create [
                DockPanel.margin 4.0
                DockPanel.children [
                    DockPanel.create [
                        DockPanel.dock Dock.Bottom
                        DockPanel.children [
                            Button.create [
                                Button.dock Dock.Right
                                Button.content "Restart Game"
                                Button.onClick (fun _ -> state.Set(update Restart state.Current))
                            ]
                            TextBlock.create [
                                TextBlock.text (renderGameState state.Current.State)
                                TextBlock.verticalAlignment VerticalAlignment.Center
                            ]
                        ]
                        DockPanel.margin 4.0
                    ]
                    renderBoard
                        (fun position -> state.Set(update (Move position) state.Current))
                        state.Current
                ]
            ]
        )