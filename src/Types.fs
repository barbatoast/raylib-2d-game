namespace AdventureGame

module Types =
  type Tiles =
    | Empty = 0
    | Grass = 1
    | Wall = 2
    | Floor = 3
    | DoorLocked = 4
    | DungeonEntrance = 5

  type TileMap = {
    width: int
    height: int
    tiles: int[]
    tileSize: int
  }

  let inline tileAt (m:TileMap) (x:int) (y:int) =
    if x < 0 || y < 0 || x >= m.width || y >= m.height then int Tiles.Wall
    else m.tiles.[y * m.width + x]
