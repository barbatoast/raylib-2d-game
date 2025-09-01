namespace AdventureGame

open System.Numerics
open Raylib_cs

module TileMapTypes =

    type MapType = | Orthographic | Isometric

    [<Struct>]
    type Tile =
        { Sprite: int16
          FlipX: bool
          FlipY: bool
          FlipDiag: bool }

    type Layer =
        { Id: int
          Name: string
          Size: Vector2 // tiles (w,h)
          IsObject: bool }

    type TileLayer =
        { Base: Layer
          TileSize: Vector2
          Tiles: Tile array }

    type Property =
        { Name: string; Type: string; Value: string }
        member p.AsInt()    = match p.Type, System.Int32.TryParse p.Value with | "int", (true,v) -> v | _ -> 0
        member p.AsFloat()  = match p.Type, System.Single.TryParse p.Value with | "float", (true,v) -> v | _ -> 0f
        member p.AsString() = p.Value

    type TileObjectSubType = | Ellipse | Point | Polygon | Polyline | Text | NoSubType

    type TileObject =
        { Id: int
          Name: string
          Bounds: Rectangle
          Visible: bool
          Type: string
          Rotation: single
          GridTile: int
          Template: string
          SubType: TileObjectSubType
          Properties: Property list }

    type ObjectLayer =
        { Base: Layer
          Objects: TileObject list }

    /// Use a DU for draw order instead of (int * obj)
    type LayerContent =
        | Tiles   of TileLayer
        | Objects of ObjectLayer

    type TileMap =
        { MapType: MapType
          Layers: (int * LayerContent) list  // draw order preserved
          TileLayers: Map<int, TileLayer>
          ObjectLayers: Map<int, ObjectLayer>
          Properties: Property list }

    module TmxFlags =
        let [<Literal>] FlippedH = 0x80000000u
        let [<Literal>] FlippedV = 0x40000000u
        let [<Literal>] FlippedD = 0x20000000u
