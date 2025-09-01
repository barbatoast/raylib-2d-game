namespace AdventureGame

open System.Numerics
open Raylib_cs

module Treasure =

  // Mirror of C++ TreasureInstance
  [<Struct>]
  type TreasureInstance =
    { mutable ItemId: int
      mutable Quantity: int
      mutable Position: Vector2
      mutable SpriteId: int }

  // Hold onto the item ids that Items.setupDefaultItems returns
  // You MUST call Treasure.init ids once after setting up items.
  let mutable private ids : Items.DefaultItemIds option = None

  let init (defaultIds: Items.DefaultItemIds) =
    ids <- Some defaultIds

  let private requireIds () =
    match ids with
    | Some v -> v
    | None -> invalidOp "Treasure.init was not called. Call Treasure.init (Items.setupDefaultItems sprites) during game init."

  let inline private make itemId quantity =
    { ItemId = itemId
      Quantity = quantity
      Position = Vector2.Zero
      SpriteId = -1 }

  /// Equivalent of C++ GetLoot(const std::string& loot_name)
  let getLoot (lootName: string) : TreasureInstance array =
    let ids = requireIds ()
    let rnd a b = Raylib.GetRandomValue(a, b)

    let loot = System.Collections.Generic.List<TreasureInstance>()

    match lootName with
    | "tutorial_loot_0" ->
        loot.Add(make ids.LeatherArmor 1)
        loot.Add(make ids.Food (rnd 2 5))

    | "tutorial_loot_1" ->
        loot.Add(make ids.Sword 1)

    | "random_loot" ->
        let count = rnd 1 3
        for _ = 1 to count do
          // random item EXCEPT GoldBag (to avoid pulling “gold” as a non-gold drop)
          loot.Add(make (Items.getRandomExcept ids.GoldBag) 1)

    | "mob_loot" ->
        loot.Add(make (Items.getRandomExcept ids.GoldBag) 1)

    | _ -> () // unknown table: skip to gold addition

    // Always add random gold (1..20), as per C++ version
    let value = rnd 1 20
    loot.Add(make ids.GoldBag value)

    loot |> Seq.toArray
