![shmup-ds screenshot](
https://opengameart.org/sites/default/files/deep-space-ships-screenshot.01.png)

Here is a simple shmup game made from `shmup_av` and
the sprites from `Bazlik_Commander`.

You can play with the keyboard or with a joystick.  
With the keyboard shoot with `Z`.  
When you lost, restart with `Space` (or button 3 of the joystick).  

This game are released under a restrictionless Zlib license,  
see the file LICENSE.txt for details.

The sprites from `Bazlik_Commander` are released under the CC0 license,  
see [Commander/LICENSE.txt](Commander/LICENSE.txt)
and [Commander/README.txt](Commander/README.txt) for details.

Run the following command to uncompress the sprites:
```
make -C Commander
```

You need ocamlsdl2 to run and/or compile:  
https://github.com/fccm/OCamlSDL2

This game is known to work with:
- SDL2 version 2.0.10
- OCaml version 4.11.1
- OCamlSDL2 version 0.04 (git)

After installing ocamlsdl2 you can run this game with:
```
ocaml -I $(ocamlfind query sdl2) sdl2.cma shmup_ds.ml
```

If you just compiled ocamlsdl2 without installing it:
```
ocaml -I ../OCamlSDL2/src sdl2.cma shmup_ds.ml
```

Write to me to tell me what you think about this game,  
or to tell me what is your higher score:  
monnier.florent (at) gmail.com

