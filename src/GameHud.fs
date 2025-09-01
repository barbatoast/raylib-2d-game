namespace AdventureGame

open System
open System.Numerics
open Raylib_cs
open AdventureGame.ResourceIds

module GameHud =

    // --------- Simple view models (no coupling to your Items/Player internals) ---------

    type ItemInfo =
        { Name: string
          Sprite: int
          IsActivatable: bool
          IsWeapon: bool
          IsArmor: bool }

    type BackpackSlot = { ItemId: int; Quantity: int }

    type PlayerView =
        { Gold: int
          Health: int
          MaxHealth: int
          AttackMin: int
          AttackMax: int
          EquipedWeapon: int
          EquipedArmor: int
          AttackCooldown: single        // 0..1
          ItemCooldown: single          // 0..1
          BuffLifetimeLeft: single      // seconds (display only)
          BuffItem: int                 // sprite id
          BackpackContents: BackpackSlot array
          ActivateItemCallback: (int -> unit) option      // index into BackpackContents
          EquipWeaponCallback: (int -> unit) option       // index into BackpackContents
          EquipArmorCallback: (int -> unit) option        // index into BackpackContents
          DropItemCallback: (int -> unit) option }        // index into BackpackContents

    type State =
        { mutable InventoryOpen: bool
          mutable HoveredItem: ItemInfo option
          ButtonSize: single
          ButtonInset: single }
        static member Create(?buttonSize:single, ?buttonInset:single) =
            { InventoryOpen = false
              HoveredItem = None
              ButtonSize = defaultArg buttonSize 70f
              ButtonInset = defaultArg buttonInset 6f }

    // --------------------- helpers ---------------------

    let inline private rect (x:single) (y:single) (w:single) (h:single) =
        let mutable r = Rectangle()
        r.X <- x; r.Y <- y; r.Width <- w; r.Height <- h
        r

    // explicit mapping avoids enum math incompatibilities
    let private keyNum (i:int) : KeyboardKey =
        match i with
        | 0 -> KeyboardKey.One
        | 1 -> KeyboardKey.Two
        | 2 -> KeyboardKey.Three
        | 3 -> KeyboardKey.Four
        | 4 -> KeyboardKey.Five
        | 5 -> KeyboardKey.Six
        | 6 -> KeyboardKey.Seven
        | _ -> KeyboardKey.K  // fallback

    let private drawSpriteIn (spriteId:int) (dst:Rectangle) =
        if spriteId >= 0 then
            Sprites.drawSprite spriteId dst 0.0f Color.White

    let private drawCenteredSprite (spriteId:int) (cx:single) (cy:single) (scale:single) =
        if spriteId >= 0 then
            // draw as a square centered on (cx,cy)
            let s = 32f * scale
            let dst = rect (cx - s/2f) (cy - s/2f) s s
            Sprites.drawSprite spriteId dst 0.0f Color.White

    /// Convert Raylib_cs CBool -> bool (F# needs explicit conversion)
    let inline private cb (x: CBool) : bool = CBool.op_Implicit x

    /// Returns true if mouse is over rect and LMB was pressed _this_ frame
    let private clicked (r:Rectangle) =
        cb (Raylib.CheckCollisionPointRec(Raylib.GetMousePosition(), r))
        && cb (Raylib.IsMouseButtonPressed(MouseButton.Left))

    /// Draw a square item/slot button; returns true if mouse is hovering the button
    let private drawButton (st:State) (x:single) (y:single) (sprite:int) (quantity:int) (border:Color) (center:Color) =
        let b = rect x y st.ButtonSize st.ButtonSize
        Raylib.DrawRectangleRec(b, border)
        Raylib.DrawRectangleRec(rect (x+st.ButtonInset) (y+st.ButtonInset) (st.ButtonSize - 2f*st.ButtonInset) (st.ButtonSize - 2f*st.ButtonInset), center)

        if sprite >= 0 then
            // slight drop shadow + sprite
            let cx = x + st.ButtonSize/2f
            let cy = y + st.ButtonSize/2f
            drawCenteredSprite sprite (cx+2f) (cy+2f) 2f
            drawCenteredSprite sprite cx cy 2f

        if quantity > 1 then
            Raylib.DrawText(sprintf "x%d" quantity, int (x + st.ButtonSize/2f), int (y + st.ButtonSize - 22f), 20, Color.White)

        cb (Raylib.CheckCollisionPointRec(Raylib.GetMousePosition(), b))

    // --------------------- public API ---------------------

    /// Returns true if a click at 'pos' should be considered "UI" (and not gameplay)
    let IsUiClick (st:State) (pos:Vector2) =
        let barHeightY = float32 (Raylib.GetScreenHeight()) - 80f
        if pos.Y > barHeightY then true
        else
            // inventory window area
            let invRect = rect (float32 (Raylib.GetScreenWidth()) - 475f) (float32 (Raylib.GetScreenHeight()) - 500f) 354f 400f
            st.InventoryOpen && Raylib.CheckCollisionPointRec(pos, invRect) |> cb

    /// Draw the HUD and handle HUD input.
    /// - getItem : int -> ItemInfo option  (resolve an itemId to metadata)
    let Draw (st:State) (player:PlayerView) (getItem:int -> ItemInfo option) =
        let sw = float32 (Raylib.GetScreenWidth())
        let sh = float32 (Raylib.GetScreenHeight())
        let barY = sh - 80f

        // background bar
        Raylib.DrawRectangleRec(rect 0f barY sw 80f, Raylib.ColorAlpha(Color.DarkGray, 0.25f))

        // score (coin)
        drawCenteredSprite CoinSprite (sw - 200f) (barY + 40f) 4f
        Raylib.DrawText(sprintf "x %03d" player.Gold, int (sw - 170f), int (barY + 20f), 40, Color.White)

        // health bar
        Raylib.DrawText("Health", 20, int (barY + 5f), 20, Color.Red)
        let healthBarW = 300f
        Raylib.DrawRectangleLinesEx(rect 20f (barY + 30f) (float32 healthBarW) 32f, 1f, Color.White)
        let hp = if player.MaxHealth <= 0 then 0f else float32 player.Health / float32 player.MaxHealth
        Raylib.DrawRectangleRec(rect 22f (barY + 32f) (max 0f ((float32 healthBarW) * hp - 4f)) 28f, Color.Red)

        // clear hover each frame
        st.HoveredItem <- None

        // Equipped weapon button
        let mutable buttonX = 20f + float32 healthBarW + 10f
        let buttonY = barY + 4f

        let weaponSprite =
            match getItem player.EquipedWeapon with
            | Some it -> it.Sprite
            | None -> -1
        ignore (drawButton st buttonX buttonY weaponSprite 0 Color.DarkGray Color.Gray)

        if player.AttackCooldown > 0f then
            let height = st.ButtonSize * player.AttackCooldown
            Raylib.DrawRectangleRec(rect buttonX (buttonY + (st.ButtonSize - height)) st.ButtonSize height, Raylib.ColorAlpha(Color.Red, 0.5f))

        // 1..7 activatable items from backpack (left-to-right)
        let activatable =
            [| for i in 0 .. player.BackpackContents.Length - 1 do
                   match getItem player.BackpackContents[i].ItemId with
                   | Some it when it.IsActivatable -> yield i
                   | _ -> () |]

        let mutable activatedIndex = -1

        for i = 0 to 6 do
            buttonX <- buttonX + st.ButtonSize + 4f
            if i < activatable.Length then
                let slotIndex = activatable[i]
                let slot = player.BackpackContents[slotIndex]
                match getItem slot.ItemId with
                | None -> ()
                | Some it ->
                    let hovered = drawButton st buttonX buttonY it.Sprite slot.Quantity Color.Brown Color.Beige
                    let shortcut = cb (Raylib.IsKeyPressed(keyNum i))
                    if hovered then
                        // left click uses item if no cooldown, else just show tooltip
                        if (cb (Raylib.IsMouseButtonPressed(MouseButton.Left)) || shortcut) && player.ItemCooldown = 0f then
                            activatedIndex <- slotIndex
                        else
                            st.HoveredItem <- Some it
                    // numeric label
                    Raylib.DrawText(string (i+1), int buttonX, int buttonY, 20, Color.White)
                    // item cooldown overlay
                    if player.ItemCooldown > 0f then
                        let height = st.ButtonSize * player.ItemCooldown
                        Raylib.DrawRectangleRec(rect buttonX (buttonY + (st.ButtonSize - height)) st.ButtonSize height, Raylib.ColorAlpha(Color.Black, 0.5f))

        // trigger activation if any
        match activatedIndex, player.ActivateItemCallback with
        | idx, Some cb when idx >= 0 -> cb idx
        | _ -> ()

        // backpack (bag) button
        buttonX <- buttonX + st.ButtonSize + 4f
        let bagRect = rect buttonX buttonY st.ButtonSize st.ButtonSize
        let _hover = drawButton st buttonX buttonY BagSprite 0 Color.Gray Color.LightGray
        if clicked bagRect || cb (Raylib.IsKeyPressed(KeyboardKey.I)) then
            st.InventoryOpen <- not st.InventoryOpen

        // buff icon (if active)
        buttonX <- buttonX + st.ButtonSize + 4f
        if player.BuffLifetimeLeft > 0f then
            drawCenteredSprite player.BuffItem (buttonX + st.ButtonSize/2f) (buttonY + st.ButtonSize/2f) 2f
            Raylib.DrawText(sprintf "%.0f" player.BuffLifetimeLeft, int buttonX, int (buttonY + st.ButtonSize - 30f), 30, Color.Red)

        // inventory window
        if st.InventoryOpen then
            let inv = rect (sw - 475f) (sh - 500f) 354f 400f
            // shadow
            let shadow = rect (inv.X + 10f) (inv.Y + 10f) inv.Width inv.Height
            Raylib.DrawRectangleRec(shadow, Raylib.ColorAlpha(Color.DarkBrown, 0.5f))
            // background panel from sprite (if available)
            drawSpriteIn InventoryBackgroundSprite inv

            // equipment slots (weapon/armor)
            let weaponInfo = getItem player.EquipedWeapon
            if drawButton st (inv.X + 20f) (inv.Y + 20f) (weaponInfo |> Option.map (fun i -> i.Sprite) |> Option.defaultValue -1) 0 Color.DarkGray Color.Gray then
                st.HoveredItem <- weaponInfo
            Raylib.DrawText("Weapon", int (inv.X + 20f + st.ButtonSize + 2f), int (inv.Y + 20f), 20, Color.DarkBrown)
            Raylib.DrawText(sprintf "%d - %d" player.AttackMin player.AttackMax, int (inv.X + 20f + st.ButtonSize + 2f), int (inv.Y + 40f), 20, Color.White)

            let armorInfo = getItem player.EquipedArmor
            let ax = inv.X + inv.Width - (20f + st.ButtonSize)
            if drawButton st ax (inv.Y + 20f) (armorInfo |> Option.map (fun i -> i.Sprite) |> Option.defaultValue -1) 0 Color.DarkBrown Color.Brown then
                st.HoveredItem <- armorInfo
            Raylib.DrawText("Armor", int (ax - 62f), int (inv.Y + st.ButtonSize), 20, Color.DarkBrown)

            // backpack grid (5 x 4)
            let cell = 64f
            let pad  = 4f
            Raylib.DrawText("Backpack (LMB)Use/Equip  (RMB)Drop", int (inv.X + 10f), int (inv.Y + 100f), 10, Color.DarkBrown)

            let mutable itemIndex = 0
            for y = 0 to 3 do
                for x = 0 to 4 do
                    let itemY = inv.Y + (inv.Height - pad) - ((pad + cell) * (4f - float32 y))
                    let itemX = inv.X + (pad * 2f) + ((cell + pad) * float32 x)
                    let r = rect itemX itemY cell cell
                    // slot shadow + bg
                    Raylib.DrawRectangleRec(rect (r.X+2f) (r.Y+2f) r.Width r.Height, Raylib.ColorAlpha(Color.Black, 0.5f))
                    drawSpriteIn ItemBackgroundSprite r

                    if itemIndex < player.BackpackContents.Length then
                        let slot = player.BackpackContents[itemIndex]
                        match getItem slot.ItemId with
                        | None -> ()
                        | Some it ->
                            // draw item sprite centered
                            drawCenteredSprite it.Sprite (r.X + r.Width/2f) (r.Y + r.Height/2f) 2f
                            if slot.Quantity > 1 then
                                Raylib.DrawText(sprintf "%d" slot.Quantity, int r.X + 2, int (r.Y + r.Height - 10f), 10, Color.White)

                            let hovered = cb (Raylib.CheckCollisionPointRec(Raylib.GetMousePosition(), r))
                            if hovered then
                                st.HoveredItem <- Some it
                                if cb (Raylib.IsMouseButtonPressed(MouseButton.Left)) then
                                    match it.IsActivatable, it.IsWeapon, it.IsArmor with
                                    | true, _, _ ->
                                        match player.ActivateItemCallback with
                                        | Some cb when player.ItemCooldown = 0f -> cb itemIndex
                                        | _ -> ()
                                    | _, true, _ ->
                                        match player.EquipWeaponCallback with
                                        | Some cb -> cb itemIndex
                                        | _ -> ()
                                    | _, _, true ->
                                        match player.EquipArmorCallback with
                                        | Some cb -> cb itemIndex
                                        | _ -> ()
                                    | _ -> ()
                                elif cb (Raylib.IsMouseButtonPressed(MouseButton.Right)) then
                                    match player.DropItemCallback with
                                    | Some cb -> cb itemIndex
                                    | _ -> ()
                        itemIndex <- itemIndex + 1
                    else
                        itemIndex <- itemIndex + 1

        // tooltip (simple)
        match st.HoveredItem with
        | None -> ()
        | Some it ->
            let mouse = Raylib.GetMousePosition()
            let size = Raylib.MeasureTextEx(Raylib.GetFontDefault(), it.Name, 20f, 2f)
            let tip = rect (mouse.X - (size.X/2f + 2f)) (mouse.Y - (size.Y + 2f)) (size.X + 4f) (size.Y + 4f)
            Raylib.DrawRectangleRec(tip, Raylib.ColorAlpha(Color.Black, 0.5f))
            Raylib.DrawText(it.Name, int tip.X + 2, int tip.Y + 2, 20, Color.White)
