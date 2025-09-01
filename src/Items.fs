namespace AdventureGame

open System

module Items =

  // ---- Types --------------------------------------------------------------

  type ItemType =
    | NoneT
    | Weapon
    | Armor
    | Activatable

  type ActivatableEffect =
    | NoneE
    | Healing
    | Defense
    | Damage

  type AttackInfo =
    { MinDamage: int
      MaxDamage: int
      Cooldown: float32
      Range: float32
      Melee: bool }

  type DefenseInfo = { Defense: int }

  type Item =
    { Id: int
      Name: string
      Sprite: int
      ItemType: ItemType
      Value: int
      Lifetime: float32
      Attack: AttackInfo
      Defense: DefenseInfo
      Effect: ActivatableEffect
      Duration: float32 }

  // Sprite IDs are provided by the caller so we don't depend on a Sprites module yet.
  type DefaultItemSprites =
    { Sword: int
      LeatherArmor: int
      GoldBag: int
      Food: int
      CoolSword: int
      AwesomeSword: int
      Axe: int
      MightyAxe: int
      Fork: int
      Bow: int
      GoodBow: int
      Club: int
      ChainArmor: int
      PlateArmor: int
      Potion: int
      Shield: int
      Fireball: int }

  /// IDs of items added by SetupDefaultItems (so you can reference them safely).
  type DefaultItemIds =
    { Sword: int
      LeatherArmor: int
      GoldBag: int
      Food: int
      CoolSword: int
      AwesomeSword: int
      Axe: int
      MightyAxe: int
      Fork: int
      Bow: int
      GoodBow: int
      Club: int
      ChainArmor: int
      PlateArmor: int
      Potion: int
      Shield: int
      Fireball: int }

  // ---- DB -----------------------------------------------------------------

  let private db = ResizeArray<Item>()
  let private rng = Random()

  let private defaultAttack =
    { MinDamage = 0; MaxDamage = 0; Cooldown = 1.0f; Range = 16.0f; Melee = true }

  let private defaultDefense = { Defense = 0 }

  let addItem (name: string) (sprite: int) (itemType: ItemType) : int =
    let id = db.Count
    let item =
      { Id = id
        Name = name
        Sprite = sprite
        ItemType = itemType
        Value = 0
        Lifetime = 0.0f
        Attack = defaultAttack
        Defense = defaultDefense
        Effect = ActivatableEffect.NoneE
        Duration = 0.0f }
    db.Add item
    id

  let tryGet (id: int) : Item option =
    if id >= 0 && id < db.Count then Some db[id] else None

  let get (id: int) : Item =
    match tryGet id with
    | Some i -> i
    | None -> invalidArg (nameof id) $"Invalid item id {id}"

  // choose a random item id, optionally excluding one id
  let getRandom (exceptOpt: int option) : int =
    if db.Count = 0 then invalidOp "Item DB is empty"

    let pool =
      db
      |> Seq.map (fun it -> it.Id)
      |> (fun ids ->
        match exceptOpt with
        | Some ex -> ids |> Seq.filter ((<>) ex)
        | None    -> ids)
      |> Seq.toArray

    if pool.Length = 0 then invalidOp "No eligible items to choose from"
    let i = rng.Next(0, pool.Length)
    pool[i]

  // convenience wrappers
  let getRandomAny () = getRandom None
  let getRandomExcept (ex:int) = getRandom (Some ex)

  // ---- Mutators (fluent-ish) ---------------------------------------------

  let withAttack (minDmg:int, maxDmg:int, cooldown: single, rangeOpt: single option, meleeOpt: bool option) (id:int) =
    let it = get id
    let attack =
        { it.Attack with
            MinDamage = minDmg
            MaxDamage = maxDmg
            Cooldown = cooldown
            Range = defaultArg rangeOpt it.Attack.Range
            Melee = defaultArg meleeOpt it.Attack.Melee }
    db[id] <- { it with Attack = attack }
    id

  let withDefense (defense: int) (id: int) =
    let it = get id
    db[id] <- { it with Defense = { Defense = defense } }
    id

  let withEffect (effect: ActivatableEffect, value:int, durationOpt: single option) (id:int) =
    let it = get id
    db[id] <- { it with Effect = effect; Value = value; Duration = defaultArg durationOpt it.Duration }
    id

  // ---- Helpers -------------------------------

  let withAttackBasic (minDmg,maxDmg,cooldown) id =
    withAttack(minDmg,maxDmg,cooldown, None, None) id

  let withAttackRanged (minDmg,maxDmg,cooldown,range,melee) id =
    withAttack(minDmg,maxDmg,cooldown, Some range, Some melee) id

  let withEffectNoDur (effect,value) id =
    withEffect(effect,value,None) id

  let withEffectDur (effect,value,duration) id =
    withEffect(effect,value,Some duration) id

  // ---- Default Items ------------------------------------------------------

  /// Adds the standard set of items (swords, armor, potions, etc) mirroring the RPGExample.
  /// Returns their IDs so you can reference them without relying on insertion order.
  let setupDefaultItems (s: DefaultItemSprites) : DefaultItemIds =
    // basic items
    let sword =
      addItem "Sword" s.Sword ItemType.Weapon
      |> withAttackBasic (0, 2, 1.0f)

    let leatherArmor =
      addItem "Pleather Armor" s.LeatherArmor ItemType.Armor
      |> withDefense 2

    let goldBag = addItem "Bag-o-Gold" s.GoldBag ItemType.NoneT

    let food =
      addItem "Fud" s.Food ItemType.Activatable
      |> withEffectNoDur (ActivatableEffect.Healing, 5)

    // weapons
    let coolSword =
      addItem "Cool Sword" s.CoolSword ItemType.Weapon
      |> withAttackBasic (2, 6, 1.0f)

    let awesomeSword =
      addItem "Awesome Sword" s.AwesomeSword ItemType.Weapon
      |> withAttackBasic (4, 8, 0.75f)

    let axe =
      addItem "Axe" s.Axe ItemType.Weapon
      |> withAttackBasic (1, 4, 1.5f)

    let mightyAxe =
      addItem "Mighty Axe" s.MightyAxe ItemType.Weapon
      |> withAttackBasic (2, 7, 1.5f)

    let fork =
      addItem "Battle Fork Axe" s.Fork ItemType.Weapon
      |> withAttackRanged (1, 3, 0.5f, 20.0f, true)

    let bow =
      addItem "Bow" s.Bow ItemType.Weapon
      |> withAttackRanged (1, 3, 0.25f, 150.0f, false)

    let goodBow =
      addItem "Sweet Bow" s.GoodBow ItemType.Weapon
      |> withAttackRanged (3, 6, 0.25f, 250.0f, false)

    let club =
      addItem "Bonkmaster 5000" s.Club ItemType.Weapon
      |> withAttackRanged (6, 10, 3.0f, 15.0f, true)

    // armor
    let chainArmor =
      addItem "Chain Shirt" s.ChainArmor ItemType.Armor
      |> withDefense 4

    let plateArmor =
      addItem "Full Plate" s.PlateArmor ItemType.Armor
      |> withDefense 10

    // activatables
    let potion =
      addItem "Potion" s.Potion ItemType.Activatable
      |> withEffectNoDur (ActivatableEffect.Healing, 20)

    let shield =
      addItem "Shield" s.Shield ItemType.Activatable
      |> withEffectDur (ActivatableEffect.Defense, 10, 30.0f)

    let fireball =
      addItem "Fireball Scroll" s.Fireball ItemType.Activatable
      |> withEffectNoDur (ActivatableEffect.Damage, 20)

    { Sword = sword
      LeatherArmor = leatherArmor
      GoldBag = goldBag
      Food = food
      CoolSword = coolSword
      AwesomeSword = awesomeSword
      Axe = axe
      MightyAxe = mightyAxe
      Fork = fork
      Bow = bow
      GoodBow = goodBow
      Club = club
      ChainArmor = chainArmor
      PlateArmor = plateArmor
      Potion = potion
      Shield = shield
      Fireball = fireball }
