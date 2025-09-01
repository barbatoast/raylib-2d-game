namespace AdventureGame

open Raylib_cs
open System.Collections.Generic

module Audio =

    // Background music stream
    let mutable private bgm : Music option = None

    // Sound effects registry
    let private sounds = List<Sound>()

    // ---------------- Background Music ----------------

    let stopBGM () =
        match bgm with
        | Some m when m.FrameCount > 0u ->
            Raylib.StopMusicStream(m)
            Raylib.UnloadMusicStream(m)
            bgm <- None
        | _ -> ()

    let startBGM (musicFile: string) =
        stopBGM ()
        // Music is a struct; make the local mutable before setting properties
        let mutable m = Raylib.LoadMusicStream(musicFile)
        m.Looping <- CBool true
        Raylib.PlayMusicStream(m)
        bgm <- Some m

    // ---------------- Init / Shutdown ----------------

    let init () =
        Raylib.InitAudioDevice()
        Raylib.SetMasterVolume(0.25f)
        // Start default background music
        startBGM "sounds/Flowing Rocks.ogg"

    let shutdown () =
        // Stop and unload music
        stopBGM ()
        // Unload all sounds
        for s in sounds do
            Raylib.UnloadSound(s)
        sounds.Clear()
        Raylib.CloseAudioDevice()

    // ---------------- Update ----------------

    let update () =
        match bgm with
        | Some m when m.FrameCount > 0u -> Raylib.UpdateMusicStream(m)
        | _ -> ()

    // ---------------- Sound Effects ----------------

    let loadSoundFile (path: string) : int =
        let s = Raylib.LoadSound(path)
        sounds.Add(s)
        sounds.Count - 1

    let playSound (soundId: int) =
        if soundId >= 0 && soundId < sounds.Count then
            Raylib.PlaySound(sounds[soundId])
