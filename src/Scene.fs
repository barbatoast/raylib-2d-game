namespace AdventureGame

open System
open System.Numerics
open Raylib_cs
open Types
open AdventureGame.Map

module Scene =
  type State = {
    current: string
    overworld: TileMap
    dungeon: TileMap
    map: TileMap
    playerPos: Vector2
    speed: single
  }

  let initState (overworld:TileMap) (dungeon:TileMap) : State =
    let start = Vector2(5.0f * float32 overworld.tileSize, 5.0f * float32 overworld.tileSize)
    {
      current = "overworld"
      overworld = overworld
      dungeon = dungeon
      map = overworld
      playerPos = start
      speed = 80.0f
    }

  let private isBlocked (tile:int) =
    match enum<Tiles> tile with
    | Tiles.Wall -> true
    | Tiles.DoorLocked -> true
    | _ -> false

  let private toTileXY (m:TileMap) (p:Vector2) =
    int (p.X / float32 m.tileSize), int (p.Y / float32 m.tileSize)

  let inline isDown (key: KeyboardKey) : bool =
    CBool.op_Implicit (Raylib.IsKeyDown key)

  let inline isPressed (key: KeyboardKey) : bool =
    CBool.op_Implicit (Raylib.IsKeyPressed key)

  let inline collides (a: Rectangle) (b: Rectangle) : bool =
    CBool.op_Implicit (Raylib.CheckCollisionRecs(a, b))

  let update (dt:single) (st:State) : State =
    let mutable pos = st.playerPos
    let mutable vx = 0.0f
    let mutable vy = 0.0f

    if isDown KeyboardKey.Left || isDown KeyboardKey.A then vx <- vx - 1.0f
    if isDown KeyboardKey.Right || isDown KeyboardKey.D then vx <- vx + 1.0f
    if isDown KeyboardKey.Up || isDown KeyboardKey.W then vy <- vy - 1.0f
    if isDown KeyboardKey.Down || isDown KeyboardKey.S then vy <- vy + 1.0f

    let len = MathF.Sqrt(vx * vx + vy * vy)
    let nx, ny = if len > 0.0f then vx / len, vy / len else 0.0f, 0.0f
    let attempted = Vector2(pos.X + nx * st.speed * dt, pos.Y + ny * st.speed * dt)

    // Simple AABB vs tile collision using a small player box
    let half = float32 st.map.tileSize * 0.4f
    let blockedAt (p:Vector2) =
      let checks =
        [| Vector2(p.X - half, p.Y - half)
           Vector2(p.X + half, p.Y - half)
           Vector2(p.X - half, p.Y + half)
           Vector2(p.X + half, p.Y + half) |]
      checks
      |> Array.exists (fun q ->
           let tx = int (q.X / float32 st.map.tileSize)
           let ty = int (q.Y / float32 st.map.tileSize)
           isBlocked (tileAt st.map tx ty))

    // slide on X
    let candX = Vector2(attempted.X, pos.Y)
    if not (blockedAt candX) then pos <- candX

    // slide on Y
    let candY = Vector2(pos.X, attempted.Y)
    if not (blockedAt candY) then pos <- candY

    // Scene transition: step on DungeonEntrance in overworld
    let tx, ty = toTileXY st.map pos
    let tile = tileAt st.map tx ty
    if st.current = "overworld" && enum<Tiles> tile = Tiles.DungeonEntrance then
      { st with
          current = "dungeon"
          map = st.dungeon
          playerPos = Vector2(2.0f * float32 st.dungeon.tileSize, 2.0f * float32 st.dungeon.tileSize) }
    else
      { st with playerPos = pos }

  let draw (st:State) =
    Raylib.BeginDrawing()
    Raylib.ClearBackground(Color.Black)
    Map.DrawMap ()

    // Player
    let size = float32 st.map.tileSize * 0.8f
    let px = int (st.playerPos.X - size * 0.5f)
    let py = int (st.playerPos.Y - size * 0.5f)
    Raylib.DrawRectangle(px, py, int size, int size, Color.SkyBlue)

    // UI
    Raylib.DrawText(sprintf "Scene: %s  (Arrows/WASD to move)" st.current, 10, 8, 16, Color.RayWhite)
    Raylib.EndDrawing()
