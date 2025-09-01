namespace AdventureGame

open System.Numerics
open Raylib_cs
open AdventureGame.TileMapTypes

module Map =

    let mutable private current : TileMap option = None

    let mutable private camera : Camera2D =
        let mutable c = Camera2D()
        c.Target  <- Vector2.Zero
        c.Offset  <- Vector2(float32 (Raylib.GetScreenWidth()/2), float32 (Raylib.GetScreenHeight()/2))
        c.Rotation<- 0.0f
        c.Zoom    <- 1.0f
        c

    let LoadMap (file:string) =
        current <- Some (TileMapIO.readTileMap file)

    let ClearMap () = current <- None

    let DrawMap () =
        match current with
        | Some m -> TileMapDrawing.draw &camera m
        | None -> ()

    let GetMapCamera () = camera

    let SetVisiblePoint (p:Vector2) =
        camera.Target <- p

    let GetMapObjectsOfType (objType:string) =
        match current with
        | None -> []
        | Some m ->
            [ for KeyValue(_,ol) in m.ObjectLayers do
                for o in ol.Objects do
                    if o.Type = objType then yield o ]

    let GetFirstMapObjectOfType (objType:string) =
        GetMapObjectsOfType objType |> List.tryHead

    let PointInMap (pt:Vector2) =
        match current with
        | None -> false
        | Some m ->
            m.TileLayers
            |> Seq.exists (fun (KeyValue(_,tl)) ->
                pt.X >= 0f && pt.Y >= 0f &&
                pt.X < tl.Base.Size.X * tl.TileSize.X &&
                pt.Y < tl.Base.Size.Y * tl.TileSize.Y)

    let Ray2DHitsMap (_start:Vector2) (_end:Vector2) = false
