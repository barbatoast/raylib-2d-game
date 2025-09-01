namespace AdventureGame

open System
open AdventureGame.Combat

module Monsters =

  // ---- Types --------------------------------------------------------------

  type Mob =
    { Id: int
      Name: string
      Sprite: int
      Health: int
      Defense: DefenseInfo
      Attack: AttackInfo
      DetectionRadius: single
      Speed: single
      LootTable: string }

  // Sprite IDs provided by caller (so we don't depend on a Sprites module here)
  type DefaultMobSprites =
    { Rat: int
      Snake: int
      Ghost: int
      Troll: int
      Turtle: int
      Blob: int
      Ogre: int
      Monk: int
      Beholder: int }

  /// IDs of mobs created by setupDefaultMobs (reference safely without relying on order).
  type DefaultMobIds =
    { Rat: int
      Snake: int
      Ghost: int
      Troll: int
      Turtle: int
      Blob: int
      Ogre: int
      Monk: int
      Beholder: int }

  // ---- DB -----------------------------------------------------------------

  let private db = ResizeArray<Mob>()

  let addMob (name:string) (sprite:int) (health:int) : int =
    let id = db.Count
    // Defaults to sane values; caller can mutate via replace after creation if needed
    let mob =
      { Id = id
        Name = name
        Sprite = sprite
        Health = health
        Defense = { Defense = 0 }
        Attack =
          { Name = ""
            Melee = true
            MinDamage = 0
            MaxDamage = 0
            Cooldown = 1.0f
            Range = 16.0f }
        DetectionRadius = 200.0f
        Speed = 50.0f
        LootTable = "mob_loot" }
    db.Add mob
    id

  let tryGet (id:int) = if id >= 0 && id < db.Count then Some db[id] else None
  let get (id:int) = match tryGet id with Some m -> m | None -> invalidArg (nameof id) $"Invalid mob id {id}"

  /// Replace a mob (immutable update pattern).
  let set (id:int) (mob:Mob) =
    if id < 0 || id >= db.Count then invalidArg (nameof id) $"Invalid mob id {id}"
    db[id] <- mob

  // ---- Convenience mutators -----------------------------------------------

  let withDefense (def:int) (id:int) =
    let m = get id
    set id { m with Defense = { Defense = def} }
    id

  let withAttack (name:string, melee:bool, minDmg:int, maxDmg:int, cooldown:single, range:single) (id:int) =
    let m = get id
    let atk =
      { Name = name
        Melee = melee
        MinDamage = minDmg
        MaxDamage = maxDmg
        Cooldown = cooldown
        Range = range }
    set id { m with Attack = atk }
    id

  let withAI (detectRadius:single, speed:single) (id:int) =
    let m = get id
    set id { m with DetectionRadius = detectRadius; Speed = speed }
    id

  // ---- Defaults (mirrors RPGExample) --------------------------------------

  let setupDefaultMobs (s:DefaultMobSprites) : DefaultMobIds =
    // Rat
    let rat =
      addMob "Rat" s.Rat 1
      |> withDefense 0
      |> withAttack ("Claw", true, 1, 1, 1.0f, 16.0f)
    // Snake
    let snake =
      addMob "Snek" s.Snake 10
      |> withDefense 4
      |> withAttack ("Bite", true, 1, 2, 1.0f, 16.0f)
    // Ghost (ranged)
    let ghost =
      addMob "Ghust" s.Ghost 10
      |> withDefense 7
      |> withAttack ("Scare", false, 1, 10, 5.0f, 50.0f)
    // Troll
    let troll =
      addMob "Troll" s.Troll 100
      |> withDefense 5
      |> withAttack ("Punch", true, 5, 10, 1.0f, 10.0f)
    // Turtle (slow poke)
    let turtle =
      addMob "Tortile" s.Turtle 15
      |> withDefense 7
      |> withAttack ("Headbut", true, 1, 1, 15.0f, 10.0f)
    // Blob
    let blob =
      addMob "Blorb" s.Blob 30
      |> withDefense 4
      |> withAttack ("Ewwww Gross", true, 8, 15, 2.0f, 15.0f)
    // Ogre
    let ogre =
      addMob "DudeBro" s.Ogre 45
      |> withDefense 4
      |> withAttack ("Rust Talk", true, 15, 20, 2.0f, 20.0f)
    // Monk (ranged)
    let monk =
      addMob "Munk" s.Monk 50
      |> withDefense 5
      |> withAttack ("GPL Virus Attack", false, 20, 25, 5.0f, 100.0f)
    // Beholder (ranged)
    let beholder =
      addMob "Moderator" s.Beholder 100
      |> withDefense 4
      |> withAttack ("Cast Ray", false, 20, 25, 3.0f, 100.0f)

    { Rat = rat; Snake = snake; Ghost = ghost; Troll = troll; Turtle = turtle
      Blob = blob; Ogre = ogre; Monk = monk; Beholder = beholder }
