namespace AdventureGame

open System
open System.Xml.Linq
open System.Numerics
open Raylib_cs
open AdventureGame.TileMapTypes

module TileMapIO =

    // ---------- Safe XML helpers (no nullable APIs) ----------
    let inline private xn (s:string) : XName = XName.Get s

    // Use sequences (never null) + tryHead instead of .Attribute / .Element
    let private tryAttr (el: XElement) (name: string) : XAttribute option =
        el.Attributes(xn name) |> Seq.tryHead

    let private tryElem (c: XContainer) (name: string) : XElement option =
        c.Elements(xn name) |> Seq.tryHead

    let private eachElems (c: XContainer) (name: string) : seq<XElement> =
        c.Elements(xn name) // already non-null

    let private children (c: XContainer) : seq<XElement> =
        c.Elements() // already non-null


    let private strA (el: XElement) (name: string) : string =
        match tryAttr el name with Some a -> a.Value | None -> ""

    let private i32A (el: XElement) (name: string) : int =
        match tryAttr el name with
        | Some a -> match Int32.TryParse a.Value with | true, v -> v | _ -> 0
        | None -> 0

    let private f32A (el: XElement) (name: string) : single =
        match tryAttr el name with
        | Some a -> match Single.TryParse a.Value with | true, v -> v | _ -> 0f
        | None -> 0f

    let private b01A (el: XElement) (name: string) : bool =
        match tryAttr el name with Some a -> a.Value <> "0" | None -> true

    let private mkRect (x:single) (y:single) (w:single) (h:single) =
        let mutable r = Rectangle()
        r.X <- x; r.Y <- y; r.Width <- w; r.Height <- h
        r

    let private objSubtype (el: XElement) : TileObjectSubType =
        if   tryElem el "polygon"  |> Option.isSome then TileObjectSubType.Polygon
        elif tryElem el "polyline" |> Option.isSome then TileObjectSubType.Polyline
        elif tryElem el "ellipse"  |> Option.isSome then TileObjectSubType.Ellipse
        elif tryElem el "text"     |> Option.isSome then TileObjectSubType.Text
        elif tryElem el "point"    |> Option.isSome then TileObjectSubType.Point
        else TileObjectSubType.NoSubType

    let private readProps (node: XElement) : Property list =
        match tryElem node "properties" with
        | None -> []
        | Some props ->
            [ for p in eachElems props "property" ->
                { Name  = strA p "name"
                  Type  = strA p "type"
                  Value = strA p "value" } ]

    /// CSV tile decoder: handles flip bits + gid-1
    let private parseCsvTiles (w:int) (h:int) (csv:string) (_tileW:int) (_tileH:int) : Tile array =
        let rows = csv.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
        let data =
            rows
            |> Array.collect (fun line ->
                line.Split([|','|], StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun s -> UInt32.Parse(s.Trim())))
        data
        |> Array.mapi (fun _ raw ->
            let flipX = (raw &&& TmxFlags.FlippedH) <> 0u
            let flipY = (raw &&& TmxFlags.FlippedV) <> 0u
            let flipD = (raw &&& TmxFlags.FlippedD) <> 0u
            let gid = int (raw &&& ~~~(TmxFlags.FlippedH ||| TmxFlags.FlippedV ||| TmxFlags.FlippedD))
            let sprite = if gid = 0 then -1 else gid - 1
            { Sprite = int16 sprite; FlipX = flipX; FlipY = flipY; FlipDiag = flipD })

    /// Read a TMX (CSV-encoded) into TileMap
    let readTileMap (filePath:string) : TileMap =
        let doc = XDocument.Load(filePath)
        let root =
            match (doc :> XContainer).Elements(xn "map") |> Seq.tryHead with
            | Some r -> r
            | None -> failwithf "Invalid TMX: no <map> in %s" filePath

        let mapType =
            match strA root "orientation" with
            | "orthogonal" -> MapType.Orthographic
            | _            -> MapType.Isometric

        let width  = i32A root "width"
        let height = i32A root "height"
        let tileW  = i32A root "tilewidth"
        let tileH  = i32A root "tileheight"

        let mutable layersOrder : (int * LayerContent) list = []
        let mutable tileLayers = Map.empty
        let mutable objectLayers = Map.empty

        let mapProps =
            match tryElem root "properties" with
            | None -> []
            | Some p ->
                [ for pr in eachElems p "property" ->
                    { Name = strA pr "name"
                      Type = strA pr "type"
                      Value = strA pr "value" } ]

        for child in children root do
            match child.Name.LocalName with
            | "layer" ->
                let id   = i32A child "id"
                let name = strA child "name"
                match tryElem child "data" with
                | None -> ()
                | Some dataNode ->
                    let encoding = strA dataNode "encoding"
                    if encoding <> "csv" then failwith "Only CSV TMX encoding is supported."
                    let tiles = parseCsvTiles width height dataNode.Value tileW tileH
                    let baseL = { Id = id; Name = name; Size = Vector2(float32 width, float32 height); IsObject = false }
                    let tl =
                        { Base = baseL
                          TileSize = Vector2(float32 tileW, float32 tileH)
                          Tiles = tiles }
                    layersOrder <- layersOrder @ [ (id, LayerContent.Tiles tl) ]
                    tileLayers <- tileLayers.Add(id, tl)

            | "objectgroup" ->
                let id   = i32A child "id"
                let name = strA child "name"
                let baseL = { Id = id; Name = name; Size = Vector2.Zero; IsObject = true }

                let objects =
                    [ for xobj in eachElems child "object" ->
                        let x = f32A xobj "x"      |> float32
                        let y = f32A xobj "y"      |> float32
                        let w = f32A xobj "width"  |> float32
                        let h = f32A xobj "height" |> float32
                        { Id        = i32A xobj "id"
                          Name      = strA xobj "name"
                          Bounds    = mkRect x y w h
                          Visible   = b01A xobj "visible"
                          Type      = strA xobj "type"
                          Rotation  = f32A xobj "rotation"
                          GridTile  = i32A xobj "gid"
                          Template  = strA xobj "template"
                          SubType   = objSubtype xobj
                          Properties= readProps xobj } ]

                let ol = { Base = baseL; Objects = objects }
                layersOrder   <- layersOrder @ [ (id, LayerContent.Objects ol) ]
                objectLayers  <- objectLayers.Add(id, ol)

            | "properties" | "tileset" -> ()
            | _ -> ()

        { MapType      = mapType
          Layers       = layersOrder
          TileLayers   = tileLayers
          ObjectLayers = objectLayers
          Properties   = mapProps }
