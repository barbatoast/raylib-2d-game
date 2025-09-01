namespace AdventureGame

open Raylib_cs

module Pause =

    // --- CBool helper ---
    let private toBool (cb: CBool) : bool = CBool.op_Implicit cb

    // --- wire-up points (safe defaults) ---
    let mutable onResume        : (unit -> unit) = fun () -> ()
    let mutable onQuitToMenu    : (unit -> unit) = fun () -> ()
    let mutable onQuitToDesktop : (unit -> unit) = fun () -> ()

    let mutable versionString   : string = "v0.1"
    let mutable copyrightString : string = "© 2025"

    /// Configure actions/labels using options (use Some/None).
    let configure
        (resumeOpt     : (unit -> unit) option)
        (quitToMenuOpt : (unit -> unit) option)
        (quitAppOpt    : (unit -> unit) option)
        (versionOpt    : string option)
        (copyrightOpt  : string option) =
        match resumeOpt     with Some f -> onResume <- f        | None -> ()
        match quitToMenuOpt with Some f -> onQuitToMenu <- f     | None -> ()
        match quitAppOpt    with Some f -> onQuitToDesktop <- f  | None -> ()
        match versionOpt    with Some s -> versionString <- s    | None -> ()
        match copyrightOpt  with Some s -> copyrightString <- s  | None -> ()

    // --- Pause Screen ---
    type PauseMenuScreen() =
        inherit Screens.Screen()

        override this.Draw() =
            // dim
            this.DimSceen()

            this.DrawCenteredText(40,  "Raylib RPG Example", 40, Color.Blue)
            this.DrawCenteredText(105, "Paused",             60, Color.Red)

            Raylib.DrawText(versionString, 2, Raylib.GetScreenHeight() - 10, 10, Color.Gray)
            let cw = Raylib.MeasureText(copyrightString, 10)
            Raylib.DrawText(
                copyrightString,
                Raylib.GetScreenWidth() - 2 - cw,
                Raylib.GetScreenHeight() - 10, 10, Color.Gray)

            if this.CenteredButton(Raylib.GetScreenHeight() / 4, "Resume") then
                onResume()

            if this.CenteredButton(Raylib.GetScreenHeight() / 2, "Quit to Menu") then
                onQuitToMenu()

            if this.CenteredButton(Raylib.GetScreenHeight() - (Raylib.GetScreenHeight() / 4), "Quit to Desktop") then
                onQuitToDesktop()

    let private pauseMenu = PauseMenuScreen()

    /// Set active screen; resume on ESC
    let updatePaused () =
        Screens.setActiveScreen pauseMenu
        if toBool (Raylib.IsKeyPressed(KeyboardKey.Escape)) then
            onResume()
