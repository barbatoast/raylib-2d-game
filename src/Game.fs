namespace AdventureGame

open System
open System.Numerics
open Raylib_cs

open AdventureGame.ResourceIds
open AdventureGame.Sprites
open AdventureGame.Items
open AdventureGame.Monsters
open AdventureGame.Combat
open AdventureGame.Treasure
open AdventureGame.Map

/// Minimal game loop aligned to this project's types.
module Game =

  // --------------------- Types ---------------------

  /// Simple inventory slot (decoupled from HUD)
  type InventorySlot = { ItemId: int; Quantity: int }

  type Player =
    { mutable Pos: Vector2
      mutable Health: int
      mutable MaxHealth: int
      mutable Gold: int
      mutable EquipedWeapon: int
      mutable EquipedArmor: int
      mutable AttackCooldownLeft: single
      mutable ItemCooldownLeft: single
      mutable BuffLifetimeLeft: single
      mutable BuffItemSprite: int
      mutable Backpack: InventorySlot array }

  type MobInstance =
    { mutable MobId: int
      mutable Pos: Vector2
      mutable Health: int
      mutable AttackCooldownLeft: single }

  type Model =
    { Items: Items.DefaultItemIds
      Mobs: Monsters.DefaultMobIds
      mutable Player: Player
      mutable MobsAlive: MobInstance array
      mutable Loot: Treasure.TreasureInstance array }

  // --------------------- Setup helpers ---------------------

  let private makeItemSprites () : Items.DefaultItemSprites =
    { Sword = ResourceIds.SwordSprite
      LeatherArmor = ResourceIds.LeatherArmorSprite
      GoldBag = ResourceIds.BagSprite
      Food = ResourceIds.FoodSprite
      CoolSword = ResourceIds.SwordSprite
      AwesomeSword = ResourceIds.SwordSprite
      Axe = ResourceIds.AxeSprite
      MightyAxe = ResourceIds.AxeSprite
      Fork = ResourceIds.ForkSprite
      Bow = ResourceIds.BowSprite
      GoodBow = ResourceIds.GoodBowSprite
      Club = ResourceIds.ClubSprite
      ChainArmor = ResourceIds.ChainArmorSprite
      PlateArmor = ResourceIds.PlateArmorSprite
      Potion = ResourceIds.PotionSprite
      Shield = ResourceIds.ShieldSprite
      Fireball = ResourceIds.FireballSprite }

  let private makeMobSprites () : Monsters.DefaultMobSprites =
    { Rat = ResourceIds.RatSprite
      Snake = ResourceIds.SnakeSprite
      Ghost = ResourceIds.GhostSprite
      Troll = ResourceIds.TrollSprite
      Turtle = ResourceIds.TurtleSprite
      Blob = ResourceIds.BlobSprite
      Ogre = ResourceIds.OgreSprite
      Monk = ResourceIds.MonkSprite
      Beholder = ResourceIds.BeholderSprite }

  /// Create a brand new game model.
  let init () : Model =
    // register default DBs
    let itemIds = Items.setupDefaultItems (makeItemSprites())
    let mobIds  = Monsters.setupDefaultMobs  (makeMobSprites())

    let player =
      { Pos = Vector2(64f, 64f)
        Health = 10
        MaxHealth = 10
        Gold = 0
        EquipedWeapon = itemIds.Sword
        EquipedArmor = itemIds.LeatherArmor
        AttackCooldownLeft = 0f
        ItemCooldownLeft = 0f
        BuffLifetimeLeft = 0f
        BuffItemSprite = -1
        Backpack = [| |] }

    { Items = itemIds
      Mobs = mobIds
      Player = player
      MobsAlive = [| |]
      Loot = [| |] }

  // --------------------- Spawning ---------------------

  let spawnMob (id:int) (pos:Vector2) (m:Model) =
    let hp = (Monsters.get id).Health
    let inst = { MobId = id; Pos = pos; Health = hp; AttackCooldownLeft = 0f }
    m.MobsAlive <- Array.append m.MobsAlive [| inst |]

  let spawnLoot (itemId:int) (qty:int) (pos:Vector2) (m:Model) =
    let sprite =
      match Items.tryGet itemId with
      | Some it -> it.Sprite
      | None -> -1
    let t : TreasureInstance = { ItemId = itemId; Quantity = qty; Position = pos; SpriteId = sprite }
    m.Loot <- Array.append m.Loot [| t |]

  // --------------------- Update ---------------------

  let private toCombatAttack (name:string) (atk: Items.AttackInfo) : Combat.AttackInfo =
    { Name = name
      Melee = atk.Melee
      MinDamage = atk.MinDamage
      MaxDamage = atk.MaxDamage
      Cooldown = atk.Cooldown
      Range = atk.Range }

  let private tryPickUp (m:Model) =
    let p = m.Player.Pos
    let near (pos:Vector2) = Vector2.Distance(p, pos) < 20.0f
    let goldBag = m.Items.GoldBag
    let mutable kept = System.Collections.Generic.List<Treasure.TreasureInstance>()
    let mutable goldAdded = 0
    for t in m.Loot do
      if near t.Position then
        if t.ItemId = goldBag then goldAdded <- goldAdded + t.Quantity
        else
          // put into backpack or equip if empty weapon/armor
          let before = m.Player.Backpack
          m.Player.Backpack <- Array.append before [| { ItemId = t.ItemId; Quantity = t.Quantity } |]
          Audio.playSound ResourceIds.ItemPickupSoundId
      else kept.Add(t)
    if goldAdded > 0 then
      m.Player.Gold <- m.Player.Gold + goldAdded
      Audio.playSound ResourceIds.ItemPickupSoundId
    m.Loot <- kept |> Seq.toArray

  let private movePlayer (dt:single) (m:Model) =
    let mutable v = Vector2.Zero
    if CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.W)) || CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.Up)) then v.Y <- v.Y - 1f
    if CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.S)) || CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.Down)) then v.Y <- v.Y + 1f
    if CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.A)) || CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.Left)) then v.X <- v.X - 1f
    if CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.D)) || CBool.op_Implicit (Raylib.IsKeyDown(KeyboardKey.Right)) then v.X <- v.X + 1f
    if v.LengthSquared() > 0f then
      v <- Vector2.Normalize v * 80.0f * dt
      let next = m.Player.Pos + v
      if Map.PointInMap next then m.Player.Pos <- next else m.Player.Pos <- next
    ()

  let private attackIfRequested (dt:single) (m:Model) =
    let mutable cd = m.Player.AttackCooldownLeft
    cd <- max 0f (cd - dt)
    if cd <= 0f && CBool.op_Implicit (Raylib.IsMouseButtonPressed(MouseButton.Left)) then
      let p = m.Player.Pos
      let inRange inst =
        let weapon = Items.get m.Player.EquipedWeapon
        Vector2.Distance(p, inst.Pos) <= weapon.Attack.Range + 8.0f
      match m.MobsAlive |> Array.tryFind inRange with
      | None -> Audio.playSound ResourceIds.MissSoundId
      | Some target ->
          let weapon = Items.get m.Player.EquipedWeapon
          let atk = toCombatAttack weapon.Name weapon.Attack
          let def = (Monsters.get target.MobId).Defense
          let dmg = Combat.resolveAttack atk def.Defense
          if dmg > 0 then
            target.Health <- max 0 (target.Health - dmg)
            Audio.playSound ResourceIds.HitSoundId
          else
            Audio.playSound ResourceIds.MissSoundId
          cd <- weapon.Attack.Cooldown
    m.Player.AttackCooldownLeft <- cd

  let update (dt:single) (m:Model) =
    movePlayer dt m
    tryPickUp m
    attackIfRequested dt m
    for mob in m.MobsAlive do
      mob.AttackCooldownLeft <- max 0f (mob.AttackCooldownLeft - dt)
    Map.SetVisiblePoint m.Player.Pos
    m

  // --------------------- Draw ---------------------

  let private drawLoot (m:Model) =
    for t in m.Loot do
      if t.SpriteId >= 0 then
        let mutable dst = Rectangle()
        dst.X <- t.Position.X - 8f
        dst.Y <- t.Position.Y - 8f
        dst.Width <- 16f; dst.Height <- 16f
        Sprites.drawSprite t.SpriteId dst 0.0f Color.White

  let private drawMobs (m:Model) =
    for inst in m.MobsAlive do
      let mob = Monsters.get inst.MobId
      let mutable dst = Rectangle()
      dst.X <- inst.Pos.X - 8f; dst.Y <- inst.Pos.Y - 8f; dst.Width <- 16f; dst.Height <- 16f
      Sprites.drawSprite mob.Sprite dst 0.0f Color.White

  let private drawPlayer (m:Model) =
    let mutable dst = Rectangle()
    dst.X <- m.Player.Pos.X - 8f
    dst.Y <- m.Player.Pos.Y - 8f
    dst.Width <- 16f
    dst.Height <- 16f
    Sprites.drawSprite ResourceIds.PlayerSprite dst 0.0f Color.White

  let draw (m:Model) =
    Map.DrawMap()
    drawLoot m
    drawMobs m
    drawPlayer m

  // --------------------- Convenience ---------------------

  let quickStart () : Model =
    let m = init ()
    spawnMob m.Mobs.Rat (Vector2(140f, 100f)) m
    spawnMob m.Mobs.Snake (Vector2(200f, 160f)) m
    spawnLoot m.Items.GoldBag 5 (Vector2(80f, 80f)) m
    m
