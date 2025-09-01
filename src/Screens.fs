namespace AdventureGame

open System.Numerics
open Raylib_cs

module Screens =

    // ---- CBool helpers (explicit return types) ----
    let toBool (cb: CBool) : bool = CBool.op_Implicit cb
    let cbool (b: bool) : CBool = CBool(b)

    // -------- Base Screen class --------
    [<AbstractClass>]
    type Screen() =
        // Style defaults
        member val ButtonColor : Color = Color.White with get, set
        member val ButtonHighlight : Color = Color.SkyBlue with get, set
        member val ButtonPressColor : Color = Color.DarkBlue with get, set
        member val ButtonFontSize : int = 60 with get, set
        member val ButtonBorder : int = 10 with get, set

        /// Implement per screen
        abstract member Draw : unit -> unit

        // ---- Helpers ----
        member this.RectIsHovered (rect: Rectangle) : bool =
            toBool (Raylib.CheckCollisionPointRec(Raylib.GetMousePosition(), rect))

        member this.DrawCenteredText (y: int, text: string, fontSize: int, color: Color) : unit =
            let textWidth = Raylib.MeasureText(text, fontSize)
            let x = Raylib.GetScreenWidth() / 2 - textWidth / 2
            let yTop = y - fontSize / 2
            Raylib.DrawText(text, x, yTop, fontSize, color)

        member this.CenteredButton (y: int, text: string) : bool =
            let textWidth = float32 (Raylib.MeasureText(text, this.ButtonFontSize))
            let textX = float32 (Raylib.GetScreenWidth()) / 2f - textWidth / 2f
            let textY = float32 y - float32 this.ButtonFontSize / 2f

            let buttonRect =
                let mutable r = Rectangle()
                r.X <- textX - float32 this.ButtonBorder
                r.Y <- textY - float32 this.ButtonBorder
                r.Width  <- textWidth + float32 (this.ButtonBorder * 2)
                r.Height <- float32 this.ButtonFontSize + float32 (this.ButtonBorder * 2)
                r

            let hovered = this.RectIsHovered(buttonRect)
            let down = hovered && toBool (Raylib.IsMouseButtonDown(MouseButton.Left))

            let color =
                if hovered then if down then this.ButtonPressColor else this.ButtonHighlight
                else this.ButtonColor

            Raylib.DrawRectangleRec(buttonRect, Raylib.ColorAlpha(color, 0.25f))
            Raylib.DrawText(text, int textX, int textY, this.ButtonFontSize, color)
            Raylib.DrawRectangleLinesEx(buttonRect, 2.0f, color)

            let clicked = hovered && toBool (Raylib.IsMouseButtonPressed(MouseButton.Left))
            if clicked then Audio.playSound ResourceIds.ClickSoundId
            clicked

        member this.DimScreen (?alpha: single) : unit =
            let a = defaultArg alpha 0.75f
            Raylib.DrawRectangle(0, 0, Raylib.GetScreenWidth(), Raylib.GetScreenHeight(), Raylib.ColorAlpha(Color.Black, a))

        // Compat alias for C++ typo
        member this.DimSceen (?alpha: single) = this.DimScreen(?alpha = alpha)

    // -------- Active screen plumbing --------
    let mutable private activeScreen : Screen option = None

    let setActiveScreen (screen: Screen) =
        activeScreen <- Some screen

    let drawScreen () =
        match activeScreen with
        | Some s -> s.Draw()
        | None -> ()
