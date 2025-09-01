namespace AdventureGame

open System

module Combat =

  /// Defensive stats (armor rating)
  type DefenseInfo = { Defense: int }

  /// Offensive stats (weapon/attack profile)
  type AttackInfo =
    { Name: string
      Melee: bool
      MinDamage: int
      MaxDamage: int
      Cooldown: single
      Range: single }

  // RNG (single instance; deterministic seeds optional)
  let mutable private rng = Random()
  let setSeed (seed:int) = rng <- Random(seed)

  /// Resolve a single attack vs. an integer defense value.
  /// Emulates RPGExample: damage = rand(-3..6) + rand(Min..Max) - defense; min 0.
  let resolveAttack (attack: AttackInfo) (defense: int) : int =
    // rng.Next(a,b) is [a, b) so add +1 to include MaxDamage
    let shake = rng.Next(-3, 7)
    let roll  = rng.Next(attack.MinDamage, attack.MaxDamage + 1)
    let total = shake + roll - defense
    if total < 0 then 0 else total

  /// Overload using a DefenseInfo record.
  let resolve (attack: AttackInfo) (target: DefenseInfo) : int =
    resolveAttack attack target.Defense
