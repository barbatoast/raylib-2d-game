namespace AdventureGame

open System.Numerics
open Raylib_cs
open AdventureGame.TileMapTypes

module TileMapDrawing =

    let private viewRect (cam: Camera2D) =
        let x = cam.Target.X - (cam.Offset.X / cam.Zoom)
        let y = cam.Target.Y - (cam.Offset.Y / cam.Zoom)
        let w = float32 (Raylib.GetScreenWidth())  / cam.Zoom
        let h = float32 (Raylib.GetScreenHeight()) / cam.Zoom
        let mutable r = Rectangle()
        r.X <- x; r.Y <- y; r.Width <- w; r.Height <- h
        r

    let private rectInView (r:Rectangle) (view:Rectangle) =
        not (r.X + r.Width < view.X
             || r.Y + r.Height < view.Y
             || r.X > view.X + view.Width
             || r.Y > view.Y + view.Height)

    let private tileDisplayRect (x:int) (y:int) (mapType:MapType) (tileSize:Vector2) =
        let mutable r = Rectangle()
        match mapType with
        | MapType.Orthographic ->
            r.X <- float32 x * tileSize.X
            r.Y <- float32 y * tileSize.Y
            r.Width <- tileSize.X
            r.Height <- tileSize.Y
        | MapType.Isometric ->
            let halfW = tileSize.X * 0.5f
            let halfH = tileSize.Y * 0.5f
            r.X <- float32 x * halfW - float32 y * halfW - halfW
            r.Y <- float32 y * halfH + (float32 x * halfH)
            r.Width <- tileSize.X
            r.Height <- tileSize.Y
        r

    /// Draw a map
    let draw (camera: byref<Camera2D>) (map: TileMap) =
        let v = viewRect camera

        for (_, content) in map.Layers do
            match content with
            | LayerContent.Tiles tl ->
                let w = int tl.Base.Size.X
                let h = int tl.Base.Size.Y
                for y = 0 to h - 1 do
                    for x = 0 to w - 1 do
                        let idx = y * w + x
                        let t = tl.Tiles[idx]
                        if t.Sprite >= 0s then
                            let dst = tileDisplayRect x y map.MapType tl.TileSize
                            if rectInView dst v then
                                Sprites.drawSprite (int t.Sprite) dst 0.0f Color.White
            | LayerContent.Objects ol ->
                for o in ol.Objects do
                    if o.SubType = TileObjectSubType.Text then
                        Raylib.DrawText(o.Name, int o.Bounds.X, int o.Bounds.Y, 20, Color.RayWhite)
