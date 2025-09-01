namespace AdventureGame

open System
open System.Numerics
open Raylib_cs

module Loading =

  // ---------------- Types ----------------

  type LoadJob =
    { Name: string
      Run: unit -> unit }

  type Model =
    { Jobs: LoadJob array
      mutable Index: int
      mutable Message: string
      mutable Done: bool
      Background: Color
      BarBack: Color
      BarFill: Color }

  // ---------------- Helpers ----------------

  let private clamp01 (x: single) = if x < 0.0f then 0.0f elif x > 1.0f then 1.0f else x

  /// Build a rectangle centered on (cx, cy)
  let private centeredRect (cx: single) (cy: single) (w: single) (h: single) =
    let mutable r = Rectangle()
    r.X <- cx - w * 0.5f
    r.Y <- cy - h * 0.5f
    r.Width <- w
    r.Height <- h
    r

  /// Draw a progress bar with outline/background and fill
  let private drawProgressBar (dst: Rectangle) (progress: single) (back: Color) (fill: Color) =
    // background
    Raylib.DrawRectangleRec(dst, back)
    // fill (clip to width * progress)
    let p = clamp01 progress
    let mutable fillRect = dst
    fillRect.Width <- dst.Width * p
    Raylib.DrawRectangleRec(fillRect, fill)

  // ---------------- API ----------------

  /// Initialize loading model.
  /// (Use options instead of F# `?optional` to avoid FS0718 on module functions.)
  let init
      (jobs: LoadJob array)
      (background: Color option)
      (barBack: Color option)
      (barFill: Color option)
      (message: string option)
      : Model =
    { Jobs = jobs
      Index = 0
      Message = defaultArg message "Loading..."
      Done = false
      Background = defaultArg background (Color(10, 10, 14, 255))
      BarBack = defaultArg barBack (Color(60, 60, 70, 255))
      BarFill = defaultArg barFill (Color(140, 220, 140, 255)) }

  /// Step one job per call. Returns true when finished.
  let step (m: Model) =
    if m.Done then true
    else
      if m.Index < m.Jobs.Length then
        let job = m.Jobs[m.Index]
        m.Message <- $"Loading: {job.Name}"
        // run job (load texture, sound, whatever)
        job.Run()
        m.Index <- m.Index + 1
        false
      else
        m.Message <- "Done."
        m.Done <- true
        true

  /// Draw the loading screen: message + progress bar.
  let draw (m: Model) (screenW: int) (screenH: int) =
    Raylib.ClearBackground(m.Background)

    // Message
    let msg = m.Message
    let fontSize = 24
    let tw = Raylib.MeasureText(msg, fontSize) |> float32
    let tx = (float32 screenW - tw) * 0.5f |> int
    let ty = (float32 screenH * 0.5f - 60.0f) |> int
    Raylib.DrawText(msg, tx, ty, fontSize, Color.RayWhite)

    // Progress
    let progress =
      if m.Jobs.Length = 0 then 1.0f
      else float32 m.Index / float32 m.Jobs.Length |> clamp01

    let barW = float32 screenW * 0.6f
    let barH = 24.0f
    let bar = centeredRect (float32 screenW * 0.5f) (float32 screenH * 0.5f) barW barH
    drawProgressBar bar progress m.BarBack m.BarFill

    // Percent text
    let pctStr = $"%i{int (progress * 100.0f)}%%"
    let pctW = Raylib.MeasureText(pctStr, 20)
    Raylib.DrawText(pctStr, int (bar.X + bar.Width + 8.0f), int (bar.Y + bar.Height - 20.0f), 20, Color.RayWhite)

  // ---------------- Convenience asset loaders ----------------
  // These are examples you can wire into your LoadJob list.

  /// Load a texture from disk and return it.
  let loadTexture (path: string) : Texture2D =
    let tex = Raylib.LoadTexture(path)
    // Access fields with PascalCase in Raylib_cs
    if tex.Id = 0u then failwithf "Failed to load texture: %s" path
    tex

  /// Unload a texture (safe).
  let unloadTexture (tex: Texture2D) =
    if tex.Id <> 0u then Raylib.UnloadTexture(tex)

  /// Build a job that loads a texture and registers it into your sprite registry.
  /// `register` is your callback (e.g., Sprites.registerTexture).
  let textureJob (name: string) (path: string) (register: Texture2D -> unit) : LoadJob =
    { Name = name
      Run = fun () ->
        let tex = loadTexture path
        register tex }
