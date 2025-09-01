namespace AdventureGame

open System
open System.Numerics
open Raylib_cs

module Sprites =

  // ---------------- Texture registry ----------------

  let private textures = System.Collections.Generic.Dictionary<int, Texture2D>()
  let registerTexture (id:int) (tex:Texture2D) = textures[id] <- tex
  let private getTexture (id:int) =
    match textures.TryGetValue id with
    | true, t -> t
    | _ -> invalidOp $"Texture id {id} is not registered. Call Sprites.registerTexture first."

  // ---------------- Sprite DB ----------------

  [<Struct>]
  type SpriteInfo =
    { TextureId: int
      mutable Source: Rectangle
      mutable Origin: Vector2
      /// Borders: X=left, Y=top, Width=right, Height=bottom (for 9-slice)
      mutable Borders: Rectangle }

  let private sprites = System.Collections.Generic.List<SpriteInfo>()

  /// Add a sprite referencing a rectangle in a texture. Returns sprite id.
  let addSprite (textureId:int) (src:Rectangle) (origin:Vector2) (borders:Rectangle) =
    let id = sprites.Count
    sprites.Add({ TextureId = textureId; Source = src; Origin = origin; Borders = borders })
    id

  let spriteCount () = sprites.Count
  let sourceRect (spriteId:int) =
    if spriteId < 0 || spriteId >= sprites.Count then Rectangle() else sprites[spriteId].Source

  // ---------------- Draw helpers ----------------

  /// Draw a sprite by id to a destination rectangle (scaled), with rotation and tint.
  let drawSprite (spriteId:int) (dst:Rectangle) (rotation:single) (tint:Color) =
    if spriteId < 0 || spriteId >= sprites.Count then () else
      let s = sprites[spriteId]
      let tex = getTexture s.TextureId
      // Raylib_cs requires Rectangle fields/properties in PascalCase
      Raylib.DrawTexturePro(tex, s.Source, dst, s.Origin, rotation, tint)

  /// Draw a sprite at position with a uniform scale (origin from the sprite), no rotation.
  let drawSpriteAt (spriteId:int) (pos:Vector2) (scale:single) (tint:Color) =
    if spriteId < 0 || spriteId >= sprites.Count then () else
      let s = sprites[spriteId]
      let tex = getTexture s.TextureId
      let mutable dst = Rectangle()
      dst.X <- pos.X
      dst.Y <- pos.Y
      dst.Width <- s.Source.Width * scale
      dst.Height <- s.Source.Height * scale
      Raylib.DrawTexturePro(tex, s.Source, dst, s.Origin, 0.0f, tint)

  // ---------------- 9-slice panel ----------------

  /// Draw a 9-slice panel based on sprite borders into a destination rect.
  let drawNineSlice (spriteId:int) (dst:Rectangle) (tint:Color) =
    if spriteId < 0 || spriteId >= sprites.Count then () else
      let s = sprites[spriteId]
      let tex = getTexture s.TextureId

      // s.Borders encodes l/t/r/b as (X,Y,Width,Height)
      let l = int s.Borders.X
      let t = int s.Borders.Y
      let r = int s.Borders.Width
      let b = int s.Borders.Height

      // Build NPatchInfo using PascalCase fields
      let mutable patch = NPatchInfo()
      patch.Source <- s.Source
      patch.Left <- l
      patch.Top <- t
      patch.Right <- r
      patch.Bottom <- b
      patch.Layout <- NPatchLayout.NinePatch

      Raylib.DrawTextureNPatch(tex, patch, dst, Vector2.Zero, 0.0f, tint)

  // ---------------- Tiled drawing ----------------

  /// Tile a sprite to fill the destination rectangle (clips last tile).
  let drawTiled (spriteId:int) (dst:Rectangle) (tint:Color) =
    if spriteId < 0 || spriteId >= sprites.Count then () else
      let s = sprites[spriteId]
      let tex = getTexture s.TextureId
      let tileW = s.Source.Width
      let tileH = s.Source.Height

      if tileW <= 0f || tileH <= 0f then () else
        let xCount = int (dst.Width / tileW)
        let yCount = int (dst.Height / tileH)

        let mutable src = s.Source
        let mutable rect = dst
        let mutable drawDst = Rectangle()
        drawDst.Y <- rect.Y
        drawDst.Height <- tileH

        // Full rows
        for y = 0 to yCount - 1 do
          drawDst.X <- rect.X
          drawDst.Y <- rect.Y + float32 y * tileH
          drawDst.Width <- tileW
          for x = 0 to xCount - 1 do
            drawDst.X <- rect.X + float32 x * tileW
            Raylib.DrawTexturePro(tex, src, drawDst, Vector2.Zero, 0.0f, tint)

          // partial tile on the right
          drawDst.X <- rect.X + float32 xCount * tileW
          drawDst.Width <- (rect.X + rect.Width) - drawDst.X
          if drawDst.Width > 0f then
            Raylib.DrawTexturePro(tex, src, drawDst, Vector2.Zero, 0.0f, tint)

        // Partial bottom strip
        drawDst.Y <- rect.Y + float32 yCount * tileH
        drawDst.Height <- (rect.Y + rect.Height) - drawDst.Y
        if drawDst.Height > 0f then
          // full tiles across
          drawDst.Width <- tileW
          for x = 0 to xCount - 1 do
            drawDst.X <- rect.X + float32 x * tileW
            Raylib.DrawTexturePro(tex, src, drawDst, Vector2.Zero, 0.0f, tint)

          // partial tile at bottom-right corner
          drawDst.X <- rect.X + float32 xCount * tileW
          drawDst.Width <- (rect.X + rect.Width) - drawDst.X
          if drawDst.Width > 0f then
            Raylib.DrawTexturePro(tex, src, drawDst, Vector2.Zero, 0.0f, tint)

  // ---------------- Convenience for grid atlases ----------------

  /// Add sprites from a grid atlas (cols x rows) with cell size; returns the created ids.
  let addGridSprites (textureId:int) (startX:int) (startY:int) (cellW:int) (cellH:int) (cols:int) (rows:int) =
    let ids = System.Collections.Generic.List<int>()
    for r = 0 to rows - 1 do
      for c = 0 to cols - 1 do
        let mutable src = Rectangle()
        src.X <- float32 (startX + c * cellW)
        src.Y <- float32 (startY + r * cellH)
        src.Width <- float32 cellW
        src.Height <- float32 cellH
        ids.Add(addSprite textureId src Vector2.Zero (Rectangle())) |> ignore
    ids |> Seq.toArray
