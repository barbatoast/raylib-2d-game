namespace AdventureGame

open System
open System.IO
open System.Numerics
open System.Xml.Linq
open Raylib_cs

open AdventureGame.ResourceIds

module Program =

  // --------------------------
  // XML helpers (null-safe)
  // --------------------------
  let inline xn (s:string) = XName.Get s
  let inline ofObj<'T when 'T : null> (v:'T) = if isNull v then None else Some v
  let private tryElem (e:XElement) (name:string) : XElement option = e.Element(xn name) |> ofObj
  let private tryAttr (e:XElement) (name:string) : string option = e.Attribute(xn name) |> ofObj |> Option.map (fun a -> a.Value)
  let private reqInt (where:string) (name:string) (v:string option) =
    match v with
    | Some s -> match System.Int32.TryParse s with | true, n -> n | _ -> failwithf "Invalid int %s on %s" name where
    | None -> failwithf "Missing attribute %s on %s" name where
  let private optInt (v:string option) = v |> Option.bind (fun s -> match System.Int32.TryParse s with | true, n -> Some n | _ -> None) |> Option.defaultValue 0

  // --------------------------
  // Make sure we run from _resources
  // --------------------------
  let private cdIntoResources () =
    [ Directory.GetCurrentDirectory()
      AppContext.BaseDirectory
      Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, ".."))
      Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", ".."))
      Path.GetFullPath(Path.Combine(AppContext.BaseDirectory, "..", "..", "..")) ]
    |> Seq.map (fun b -> Path.Combine(b, "_resources"))
    |> Seq.tryFind Directory.Exists
    |> Option.iter Directory.SetCurrentDirectory

  // --------------------------
  // Window setup (avoid pre-init monitor queries)
  // --------------------------
  let private setupWindow () =
    Raylib.SetConfigFlags(ConfigFlags.VSyncHint ||| ConfigFlags.Msaa4xHint ||| ConfigFlags.ResizableWindow)
    Raylib.InitWindow(1280, 720, "Adventure Game")
    Raylib.SetTargetFPS(60)
    Raylib.InitWindow(1280, 720, "Adventure Game")
    Raylib.SetTargetFPS(60)

  // --------------------------
  // Tileset meta parsed from TMX
  // --------------------------
  type TilesetMeta = { Columns:int; TileCount:int; TileW:int; TileH:int; Spacing:int; FirstGid:int }

  let private readTilesetMeta (tmxPath:string) : TilesetMeta =
    let doc = XDocument.Load(tmxPath)
    let root =
      match ofObj doc.Root with
      | Some r -> r
      | None -> failwithf "Invalid TMX: no root in %s" tmxPath
    let ts =
      match tryElem root "tileset" with
      | Some t -> t
      | None -> failwithf "TMX has no <tileset>: %s" tmxPath
    let columns   = reqInt "tileset" "columns" (tryAttr ts "columns")
    let tilecount = reqInt "tileset" "tilecount" (tryAttr ts "tilecount")
    let spacing   = optInt (tryAttr ts "spacing")
    let tileW     = reqInt "map" "tilewidth"  (tryAttr root "tilewidth")
    let tileH     = reqInt "map" "tileheight" (tryAttr root "tileheight")
    let firstgid  = reqInt "tileset" "firstgid" (tryAttr ts "firstgid") |> max 1
    { Columns=columns; TileCount=tilecount; TileW=tileW; TileH=tileH; Spacing=spacing; FirstGid=firstgid }

  // --------------------------
  // Register tiles from atlas image
  // --------------------------
  let private registerTilesFrom (texturePath:string) (meta:TilesetMeta) =
    let tex = Raylib.LoadTexture(texturePath)
    Raylib.SetTextureFilter(tex, TextureFilter.Point)
    if tex.Id = 0u then failwithf "Failed to load tileset: %s (cwd=%s)" texturePath (Directory.GetCurrentDirectory())
    AdventureGame.Sprites.registerTexture TileSetTexture tex

    let cols, tc, tw, th, sp = meta.Columns, meta.TileCount, meta.TileW, meta.TileH, meta.Spacing
    let rows = (tc + cols - 1) / cols
    let mutable count = 0
    for r = 0 to rows - 1 do
      for c = 0 to cols - 1 do
        if count < tc then
          let mutable src = Rectangle()
          src.X <- float32 (c * (tw + sp))
          src.Y <- float32 (r * (th + sp))
          src.Width  <- float32 tw
          src.Height <- float32 th
          // Insert in atlas order so gid -> (firstgid + index)
          AdventureGame.Sprites.addSprite TileSetTexture src Vector2.Zero (Rectangle()) |> ignore
          count <- count + 1

  let private loadMap (path:string) =
    AdventureGame.Map.LoadMap path
    AdventureGame.Map.SetVisiblePoint(Vector2(0f, 0f))

  let inline b (v:CBool) : bool = CBool.op_Implicit v

  [<EntryPoint>]
  let main _argv =
    setupWindow()
    cdIntoResources()
    Raylib.InitAudioDevice()

    let mutable currentMap = "maps/level0.tmx"

    let meta = readTilesetMeta currentMap
    registerTilesFrom "colored_tilemap.png" meta
    loadMap currentMap

    // keep real game state around
    let mutable game = AdventureGame.Game.quickStart()

    while not (b (Raylib.WindowShouldClose())) do
      let dt = Raylib.GetFrameTime()

      // hot-swap maps with number keys
      if b (Raylib.IsKeyPressed KeyboardKey.Zero)  then currentMap <- "maps/menu_map.tmx"; loadMap currentMap
      if b (Raylib.IsKeyPressed KeyboardKey.One)   then currentMap <- "maps/level0.tmx";  loadMap currentMap
      if b (Raylib.IsKeyPressed KeyboardKey.Two)   then currentMap <- "maps/level1.tmx";  loadMap currentMap
      if b (Raylib.IsKeyPressed KeyboardKey.Three) then currentMap <- "maps/level2.tmx";  loadMap currentMap
      if b (Raylib.IsKeyPressed KeyboardKey.Four)  then currentMap <- "maps/level3.tmx";  loadMap currentMap
      if b (Raylib.IsKeyPressed KeyboardKey.Five)  then currentMap <- "maps/level4.tmx";  loadMap currentMap

      Raylib.BeginDrawing()
      Raylib.ClearBackground(Color.Black)

      AdventureGame.Game.draw game
      Raylib.DrawFPS(8, 8)
      Raylib.DrawText($"map: {Path.GetFileNameWithoutExtension currentMap} (0..5 to switch)", 8, Raylib.GetScreenHeight() - 24, 12, Color.Gray)

      Raylib.EndDrawing()

      // update state
      game <- AdventureGame.Game.update dt game

    Raylib.CloseAudioDevice()
    Raylib.CloseWindow()
    0
