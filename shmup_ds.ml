(* A Simple Minimalist Shmup Game
 Copyright (C) 2019, 2020 Florent Monnier
 
 This software is provided "AS-IS", without any express or implied warranty.
 In no event will the authors be held liable for any damages arising from
 the use of this software.
 
 Permission is granted to anyone to use this software and associated elements
 for any purpose, including commercial applications, and to alter it and
 redistribute it freely.
*)
open Sdl

type point2d = int * int  (* (x, y) *)
type vector2d = int * int  (* (x, y) *)

module Vector2d : sig
  type t = vector2d  (** (x, y) *)

  val add : t -> t -> t
  (** [a + b] *)

  val sub : t -> t -> t
  (** [a - b] *)

  val mul : t -> int -> t
  (** [v * k] *)

  val div : t -> int -> t
  (** [v / k] *)

  module Infix : sig
    val ( +. ) : t -> t -> t
    val ( -. ) : t -> t -> t
    val ( *. ) : t -> int -> t
    val ( /. ) : t -> int -> t
  end
end = struct
  type t = vector2d

  let add (ax, ay) (bx, by) =
    (ax + bx,
     ay + by)

  let sub (ax, ay) (bx, by) =
    (ax - bx,
     ay - by)

  let mul (x, y) k =
    (x * k,
     y * k)

  let div (x, y) k =
    (x / k,
     y / k)

  module Infix = struct
    let ( +. ) = add ;;
    let ( -. ) = sub ;;
    let ( *. ) = mul ;;
    let ( /. ) = div ;;
  end
end

module QuadraticBezierCurves : sig
  val interval : int * int
  (** The interval for interpolation is [(0, 1000)]
      instead of [(0.0, 1.0)] for [floats]. *)

  val point_on_curve :
    point2d * point2d * point2d ->
    int -> point2d
  (** [point_on_curve (p1, p2, p3) t] returns a point on the quadratic bezier
      curve defined by p1, p2 and p3, with t in the interval predefined above *)

end = struct
  let interval = (0, 1000)

  let point_on_curve (p1, p2, p3) t =
    let ti = 1000 - t in
    Vector2d.Infix.(
      ( p1 *. ((ti * ti) / 1000) +.
        p2 *. ((2 * ti * t) / 1000) +.
        p3 *. ((t * t) / 1000)
      ) /. 1000
    )
end

module Timeline : sig
  type time = int

  type ('a, 'b) animated = [
    | `From of time * 'a
      (** [From (t, v)] after time [t] is reach (and before next timeline chunk)
          the returned value will be [v] *)
    | `Evol of time * time * (time -> time -> time -> 'b -> 'a) * 'b
      (** [Evol (t1, t2, f, d)] when [t] is between [t1] and [t2] the value is
          the result of [f t1 t2 t d] *)
    ]

  val val_at :
    time -> ('a, 'b) animated list -> 'a

  val finished :
    time -> ('a, 'b) animated list -> bool

end = struct
  type time = int

  (* animating a value over time *)

  type ('a, 'b) animated = [
    | `From of time * 'a
    | `Evol of time * time * (time -> time -> time -> 'b -> 'a) * 'b
    ]

  (* timeline function *)

  let rec val_at t = function
    | `From(t1, v) :: `From(t2,_) :: _
    | `From(t1, v) :: `Evol(t2,_,_,_) :: _
      when t1 <= t && t < t2 -> v
    
    | `From(t, v) :: [] -> v
    
    | `Evol(t1, t2, f, v) :: []
      when t >= t2 -> f t1 t2 t2 v
    
    | `Evol(t1, t2, f, v) :: _
      when t1 <= t && t <= t2 -> f t1 t2 t v
    
    | _ :: tl -> val_at t tl
    
    | [] -> invalid_arg "val_at"


  let rec finished t = function
    | `From _ :: [] -> true
    | `Evol(_, t2, _, _) :: [] -> t > t2

    | `From(t2, _) :: tl -> if t < t2 then false else finished t tl
    | `Evol(_, t2, _, _) :: tl -> if t < t2 then false else finished t tl

    | _ -> false
end


module PausableTimer = struct
  type t = {
    mutable t1: int;
    mutable time: int;
    mutable paused: bool
  }

  let create () = {
      t1 = 0;
      time = 0;
      paused = false;
    }

  let is_paused timer =
    timer.paused

  let get_ticks timer =
    if timer.paused
    then timer.t1 - timer.time
    else (Timer.get_ticks ()) - timer.time

  let pause timer =
    if not timer.paused then begin
      timer.t1 <- Timer.get_ticks ();
      timer.paused <- true;
    end

  let restart timer =
    if timer.paused then begin
      let t2 = Timer.get_ticks () in
      timer.time <- timer.time + (t2 - timer.t1);
      timer.paused <- false;
    end

  let toggle_pause timer =
    if not timer.paused
    then pause timer
    else restart timer
end

module QBCurve = QuadraticBezierCurves


type foe = {
  foe_pos: int * int;
  foe_anim:
    (point2d, point2d * point2d * point2d) Timeline.animated list;
  foe_last_shot: int;
  foe_shoot_freq: int;
  foe_texture: Texture.t;
  foe_tex_dims: int * int * int;  (* width, height, display-ratio *)
}

type foe_bullet = {
  bullet_pos: int * int;
  bullet_line: (int * int) * (int * int);
  bullet_birth: int;
}

type player_dir = {
  left: bool;
  right: bool;
  up: bool;
  down: bool;
}

type player = {
  p_pos: int * int;
  p_last_shot: int;
  p_shoot_freq: int;
  p_shooting: bool;
  p_dir: player_dir;
  p_texture: Texture.t;
  p_tex_dims: int * int * int;  (* width, height, display-ratio *)
}

type game_state = {
  player: player;
  foes: foe list;
  f_bullets: foe_bullet list;  (* foes bullets *)
  p_bullets: (int * int) list;  (* player bullets *)
}

type game_data = {
  renderer: Render.t;
  f_bullet_tex: Texture.t;
  p_bullet_tex: Texture.t;
  foes_tex: Texture.t array;
  foes_tex_dims: (int * int * int) array;  (* width, height, display-ratio *)
  foes_tex_n: int;
  letters_tex: (char * Texture.t) list;
}

let width, height = (800, 600)

let blue   = (0, 0, 255)
let green  = (0, 255, 0)
let yellow = (255, 255, 0)
let alpha  = 255

let shot = ref 0
let missed = ref 0

let timer = PausableTimer.create ()


let letters = [
  '0', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '1', [|
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '2', [|
    [| 1; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  '3', [|
    [| 1; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 1; 1; 0; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 1; 1; 1; 0; 0 |];
  |];
  '4', [|
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 1; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
  |];
  '5', [|
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 0; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  '6', [|
    [| 0; 0; 1; 1; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '7', [|
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
  |];
  '8', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  '9', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 1 |];
    [| 0; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  ' ', [|
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
  |];
  ':', [|
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
  |];
  '-', [|
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 0; 0; 0; 0 |];
  |];
  'a', [|
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'b', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  'c', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'd', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  'e', [|
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 1 |];
  |];
  'f', [|
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
  |];
  'g', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'h', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'i', [|
    [| 0; 1; 1; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'j', [|
    [| 0; 0; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 0; 1; 0 |];
    [| 1; 0; 0; 1; 0 |];
    [| 0; 1; 1; 0; 0 |];
  |];
  'k', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 1; 0 |];
    [| 1; 1; 1; 0; 0 |];
    [| 1; 0; 0; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'l', [|
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  'm', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 0; 1; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'n', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 0; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 0; 1; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'o', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'p', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 0 |];
  |];
  'q', [|
    [| 0; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 1; 0 |];
    [| 0; 1; 1; 0; 1 |];
  |];
  'r', [|
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
    [| 1; 0; 1; 0; 0 |];
    [| 1; 0; 0; 1; 0 |];
  |];
  's', [|
    [| 0; 1; 1; 1; 1 |];
    [| 1; 0; 0; 0; 0 |];
    [| 0; 1; 1; 1; 0 |];
    [| 0; 0; 0; 0; 1 |];
    [| 1; 1; 1; 1; 0 |];
  |];
  't', [|
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
  |];
  'u', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 1; 1; 0 |];
  |];
  'v', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
  |];
  'w', [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 1; 0; 1; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
  |];
  'x', [|
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 1; 0; 0; 0; 1 |];
  |];
  'y', [|
    [| 1; 0; 0; 0; 1 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 0; 1; 0; 0 |];
  |];
  'z', [|
    [| 1; 1; 1; 1; 1 |];
    [| 0; 0; 0; 1; 0 |];
    [| 0; 0; 1; 0; 0 |];
    [| 0; 1; 0; 0; 0 |];
    [| 1; 1; 1; 1; 1 |];
  |];
]


let fill_rect40 renderer color x y =
  let rect = Rect.make4 x y 40 40 in
  Render.set_draw_color renderer color alpha;
  Render.fill_rect renderer rect;
;;


let make_background () =
  let rows = height / 40 in
  let colomns = width / 40 in
  let rows = if (height mod 40) = 0 then rows else succ rows in
  let colomns = if (width mod 40) = 0 then colomns else succ colomns in
  Array.init rows (fun y ->
    Array.init colomns (fun x ->
      let v = 50 + Random.int 16 in
      (v, v, v)))


let background = make_background ()


let display_background renderer playing =
  Array.iteri (fun y row ->
    Array.iteri (fun x rgb ->
      let _x = x * 40
      and _y = y * 40 in
      if playing
      then fill_rect40 renderer rgb _x _y
      else
        let r, g, b = rgb in
        fill_rect40 renderer (r + 40, g / 2, b / 3) _x _y
    ) row
  ) background


let src_rect = Rect.make4 0 0 5 5


let display ~playing game_state game_data =

  display_background game_data.renderer playing;

  let draw_letter texture x y size =
    let dst_rect = Rect.make4 x y size size in
    Render.copy game_data.renderer ~texture ~src_rect ~dst_rect ();
  in
  let s = Printf.sprintf "shot: %d" !shot in
  String.iteri (fun i c ->
    let tex = List.assoc c game_data.letters_tex in
    let x = i * 20 + 10 in
    let y = 10 in
    draw_letter tex x y 15;
  ) s;
  let s = Printf.sprintf "missed: %d" !missed in
  String.iteri (fun i c ->
    let tex = List.assoc c game_data.letters_tex in
    let x = i * 15 + width - 170 in
    let y = 10 in
    draw_letter tex x y 10;
  ) s;
  let s = Printf.sprintf "score: %d" (!shot - !missed) in
  String.iteri (fun i c ->
    let tex = List.assoc c game_data.letters_tex in
    let x = i * 15 + 10 in
    let y = height - 25 in
    draw_letter tex x y 10;
  ) s;

  List.iter (fun foe ->
    let x, y = foe.foe_pos in
    let texture = foe.foe_texture in
    let w, h, r = foe.foe_tex_dims in
    let src_rect = Rect.make4 0 0 w h in
    let w, h = (w / r, h / r) in
    let dst_rect = Rect.make4 x y w h in
    Render.copy game_data.renderer ~texture ~src_rect ~dst_rect ();
  ) game_state.foes;

  List.iter (fun bullet ->
    let x, y = bullet.bullet_pos in
    let dst_rect = Rect.make4 x y 20 20 in
    let texture = game_data.f_bullet_tex in
    Render.copy game_data.renderer ~texture ~src_rect ~dst_rect ();
  ) game_state.f_bullets;

  List.iter (fun pos ->
    let x, y = pos in
    let dst_rect = Rect.make4 x y 20 20 in
    let texture = game_data.p_bullet_tex in
    Render.copy game_data.renderer ~texture ~src_rect ~dst_rect ();
  ) game_state.p_bullets;

  begin
    let x, y = game_state.player.p_pos in
    let w, h, r = game_state.player.p_tex_dims in
    let src_rect = Rect.make4 0 0 w h in
    let w, h = (w / r, h / r) in
    let dst_rect = Rect.make4 x y w h in
    let texture = game_state.player.p_texture in
    let flip = Render.Flip_Vertical in
    Render.copyEx game_data.renderer ~texture ~src_rect ~dst_rect ~flip ();
  end;

  Render.render_present game_data.renderer;
;;


let proc_events player renderer = function
  | Event.KeyDown { Event.keycode = Keycode.Left } ->
      { player with p_dir = { player.p_dir with left = true } }
  | Event.KeyDown { Event.keycode = Keycode.Right } ->
      { player with p_dir = { player.p_dir with right = true } }
  | Event.KeyDown { Event.keycode = Keycode.Up } ->
      { player with p_dir = { player.p_dir with up = true } }
  | Event.KeyDown { Event.keycode = Keycode.Down } ->
      { player with p_dir = { player.p_dir with down = true } }

  | Event.KeyUp { Event.keycode = Keycode.Left } ->
      { player with p_dir = { player.p_dir with left = false } }
  | Event.KeyUp { Event.keycode = Keycode.Right } ->
      { player with p_dir = { player.p_dir with right = false } }
  | Event.KeyUp { Event.keycode = Keycode.Up } ->
      { player with p_dir = { player.p_dir with up = false } }
  | Event.KeyUp { Event.keycode = Keycode.Down } ->
      { player with p_dir = { player.p_dir with down = false } }

  | Event.KeyDown { Event.keycode = Keycode.Z } ->
      { player with p_shooting = true }
  | Event.KeyUp { Event.keycode = Keycode.Z } ->
      { player with p_shooting = false }

  | Event.KeyDown { Event.keycode = Keycode.F } ->
      Gc.full_major (); player

  (*
  | Event.KeyDown { Event.keycode = Keycode.S } ->
      let surf = Surface.create_rgb ~width ~height ~depth:32 in
      Render.read_pixels renderer surf;
      Surface.save_bmp surf ~filename:"screenshot.bmp";
      Surface.free surf;
      player
  *)

  | Event.KeyUp { Event.keycode = Keycode.P }
  | Event.KeyDown { Event.keycode = Keycode.Space }

  | Event.Joy_Button_Down { Event.jb_which = 0; Event.jb_button = 2 }
  | Event.Joy_Button_Down { Event.jb_which = 0; Event.jb_button = 9 } ->

      PausableTimer.toggle_pause timer;
      Mouse.show_cursor (PausableTimer.is_paused timer);
      player

  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0

  | Event.Joy_Button_Down { Event.jb_which = 0; Event.jb_button = 0 } ->
      { player with p_shooting = true }
  | Event.Joy_Button_Up { Event.jb_which = 0; Event.jb_button = 0 } ->
      { player with p_shooting = false }

  | Event.Joy_Axis_Motion e -> player
  | Event.Joy_Hat_Motion e ->
      begin match e.Event.jh_dir with
      | Hat.Up ->
          { player with p_dir =
            { left = false; right = false; up = true; down = false } }
      | Hat.Down ->
          { player with p_dir =
            { left = false; right = false; up = false; down = true } }
      | Hat.Left ->
          { player with p_dir =
            { left = true; right = false; up = false; down = false } }
      | Hat.Right ->
          { player with p_dir =
            { left = false; right = true; up = false; down = false } }
      | Hat.Right_Up ->
          { player with p_dir =
            { left = false; right = true; up = true; down = false } }
      | Hat.Right_Down ->
          { player with p_dir =
            { left = false; right = true; up = false; down = true } }
      | Hat.Left_Up ->
          { player with p_dir =
            { left = true; right = false; up = true; down = false } }
      | Hat.Left_Down ->
          { player with p_dir =
            { left = true; right = false; up = false; down = true } }
      | Hat.Centered ->
          { player with p_dir =
            { left = false; right = false; up = false; down = false } }
      end

  | _ -> player


let rec event_loop  game_state game_data =
  match Event.poll_event () with
  | None -> game_state
  | Some ev ->
      let player = proc_events game_state.player game_data.renderer ev in
      event_loop { game_state with player } game_data


let proc_events_r = function
  | Event.KeyDown { Event.keycode = Keycode.Q }
  | Event.KeyDown { Event.keycode = Keycode.Escape }
  | Event.Quit _ -> Sdl.quit (); exit 0

  | Event.KeyDown { Event.keycode = Keycode.Return }
  | Event.KeyDown { Event.keycode = Keycode.Space } -> true

  | Event.Joy_Button_Down { Event.jb_which = 0; Event.jb_button = 2 }
  | Event.Joy_Button_Down { Event.jb_which = 0; Event.jb_button = 9 } -> true
  | _ -> false


let rec event_restart () =
  let rec aux acc =
    match Event.poll_event () with
    | None -> List.exists (fun restart -> restart) acc
    | Some ev -> aux (proc_events_r ev :: acc)
  in
  aux []


let pixel_for_surface ~surface ~rgb =
  let fmt = Surface.get_pixelformat_t surface in
  let pixel_format = Pixel.alloc_format fmt in
  let pixel = Pixel.map_RGB pixel_format rgb in
  Pixel.free_format pixel_format;
  (pixel)


let texture_of_pattern renderer pattern ~color =
  let surface = Surface.create_rgb ~width:5 ~height:5 ~depth:32 in
  let rgb = (255, 255, 255) in
  let key = pixel_for_surface ~surface ~rgb in
  Surface.set_color_key surface ~enable:true ~key;
  let color = pixel_for_surface ~surface ~rgb:color in
  Array.iteri (fun y row ->
    Array.iteri (fun x v ->
      if v = 1
      then Surface.fill_rect surface (Rect.make4 x y 1 1) color
      else Surface.fill_rect surface (Rect.make4 x y 1 1) 0xFFFFFFl
    ) row
  ) pattern;
  let texture = Texture.create_from_surface renderer surface in
  Surface.free surface;
  (texture)


let f_bullet_inside bullet =
  let x, y = bullet.bullet_pos in
  (y < height) &&
  (x < width) &&
  (y > -20) &&
  (x > -20)


let point_on_line (p1, p2) i t =
  let ti = i - t in
  Vector2d.Infix.(
    ( (p1 *. ti) +.
      (p2 *. t)
    ) /. i
  )


let step_foes_bullets  game_state t =
  let step_bullet bullet =
    let dt = t - bullet.bullet_birth in
    let p = point_on_line bullet.bullet_line 6000 dt in
    { bullet with bullet_pos = p }
  in
  let f_bullets = List.map step_bullet game_state.f_bullets in
  let f_bullets = List.filter f_bullet_inside f_bullets in
  { game_state with f_bullets }


let inter1 t t1 t2 v1 v2 =
  ((v2 - v1) * (t - t1)) / (t2 - t1) + v1

let min_t, max_t = QBCurve.interval

let fe t1 t2 t ps =
  let t = inter1 t t1 t2 min_t max_t in
  QBCurve.point_on_curve ps t


let make_foe_anim (w, h, r) t =
  let w, h = (w / r, h / r) in
  let t1 = t
  and t2 = t + 6000 + Random.int 4000 in
  match Random.int 7 with
  | 0 ->  (* left to right *)
      let p1, p2, p3 =
        (-w, Random.int (height - h)),
        (Random.int width, Random.int (height - h)),
        (width, Random.int (height - h))
      in
      let ps = (p1, p2, p3) in
      [ `Evol (t1, t2, fe, ps) ]
  | 1 ->  (* right to left *)
      let p1, p2, p3 =
        (width, Random.int (height - h)),
        (Random.int width, Random.int (height - h)),
        (-w, Random.int (height - h))
      in
      let ps = (p1, p2, p3) in
      [ `Evol (t1, t2, fe, ps) ]
  | 2 | 3 | 4 ->  (* top to bottom *)
      let p1, p2, p3 =
        (Random.int (width - w), -h),
        (Random.int (width - w), Random.int (height - h)),
        (Random.int (width - w), height)
      in
      let ps = (p1, p2, p3) in
      [ `Evol (t1, t2, fe, ps) ]
  | 5 | 6 ->  (* top to middle, pause, middle to bottom *)
      let t1 = t
      and t2 = t + 4000 + Random.int 3000 in
      let t3 = t2 + 2000 + Random.int 2000 in
      let t4 = t3 + 4000 + Random.int 3000 in
      let p1, p2, p3, p4, p5 =
        (Random.int (width - w), -h),
        (Random.int (width - w), Random.int (height - h)),
        (Random.int (width - w), Random.int (height - h)),
        (Random.int (width - w), Random.int (height - h)),
        (Random.int (width - w), height)
      in
      let ps1 = (p1, p2, p3) in
      let ps2 = (p3, p4, p5) in
      [ `Evol (t1, t2, fe, ps1);
        `From (t2, p3);
        `Evol (t3, t4, fe, ps2); ]
  | _ -> assert false


let new_foe renderer game_data t =
  let foe_texture, foe_tex_dims =
    let i = Random.int game_data.foes_tex_n in
    (game_data.foes_tex.(i),
     game_data.foes_tex_dims.(i))
  in
  let foe_pos = (Random.int (width - 20), -20) in
  let foe_anim = make_foe_anim foe_tex_dims t in
  let foe_last_shot = t in
  let foe_shoot_freq = 1600 + Random.int 1800 in
  { foe_texture; foe_tex_dims; foe_pos; foe_anim; foe_last_shot; foe_shoot_freq }


let new_foes_opt game_state game_data t =
  if Random.int 100 > 2
  then game_state.foes
  else
    let new_foe = new_foe game_data.renderer game_data t in
    new_foe :: game_state.foes


let gun_new_f_bullets game_state foes t =
  let rec aux acc1 acc2 foes =
    match foes with
    | [] -> (acc1, acc2)
    | foe :: foes ->
        if t - foe.foe_last_shot < foe.foe_shoot_freq
        then aux acc1 (foe :: acc2) foes
        else
          let updated_foe = { foe with foe_last_shot = t } in
          let w, h =
            let w, h, r = foe.foe_tex_dims in
            (w / r, h / r)
          in
          let bx, by = (20, 20) in  (* bullet size *)
          let bullet_pos =
            let fx, fy = foe.foe_pos in
            (fx + (w/2) - (bx/2), fy + h - by)
          in
          let bullet =
            { bullet_pos;
              bullet_line = (bullet_pos, game_state.player.p_pos);
              bullet_birth = t; }
          in
          aux (bullet :: acc1) (updated_foe :: acc2) foes
  in
  let new_f_bullets, foes = aux [] [] foes in
  let f_bullets = List.rev_append new_f_bullets game_state.f_bullets in
  let foes = List.rev foes in
  (f_bullets, foes)


let foe_inside t foe =
  not (Timeline.finished t foe.foe_anim)


let foe_touched p_bullets foe =
  let x, y = foe.foe_pos in
  let w, h, r = foe.foe_tex_dims in
  let w, h = (w / r, h / r) in
  let foe_rect = Rect.make4 x y w h in
  List.exists (fun (x, y) ->
    let bullet_rect = Rect.make4 x y 20 20 in
    Rect.has_intersection foe_rect bullet_rect
  ) p_bullets


let step_foes  game_state game_data t =
  let step_foe foe =
    let new_pos = Timeline.val_at t foe.foe_anim in
    { foe with foe_pos = new_pos }
  in
  let foes = new_foes_opt game_state game_data t in
  let f_bullets, foes = gun_new_f_bullets game_state foes t in
  let foes = List.map step_foe foes in
  let foes =
    List.filter (fun foe ->
      if foe_inside t foe
      then true
      else (incr missed; false)
    ) foes
  in
  let foes =
    List.filter (fun foe ->
      if foe_touched game_state.p_bullets foe
      then (incr shot; false)
      else true
    ) foes
  in
  { game_state with foes; f_bullets }


let player_touched  game_state =
  let x, y = game_state.player.p_pos in
  let w, h, r = game_state.player.p_tex_dims in
  let w, h = (w / r, h / r) in
  let player_rect = Rect.make4 x y w h in
  List.exists (fun bullet ->
    let x, y = bullet.bullet_pos in
    let x, y = x + 4, y + 4 in
    let bullet_rect = Rect.make4 x y 12 12 in
    Rect.has_intersection player_rect bullet_rect
  ) game_state.f_bullets


let player_moving player =
  let x, y = player.p_pos in
  let _x, _y =
    match player.p_dir with
    | { left = true; right = false; up = false; down = false } -> (x - 8, y)
    | { left = false; right = true; up = false; down = false } -> (x + 8, y)
    | { left = false; right = false; up = true; down = false } -> (x, y - 8)
    | { left = false; right = false; up = false; down = true } -> (x, y + 8)

    | { left = true; right = false; up = true; down = false } -> (x - 6, y - 6)
    | { left = true; right = false; up = false; down = true } -> (x - 6, y + 6)
    | { left = false; right = true; up = true; down = false } -> (x + 6, y - 6)
    | { left = false; right = true; up = false; down = true } -> (x + 6, y + 6)

    | _ -> (x, y)
  in
  let w, h, r = player.p_tex_dims in
  let w, h = (w / r, h / r) in
  let x = min (max _x 0) (width - w)
  and y = min (max _y 0) (height - h) in
  { player with p_pos = (x, y) }


let step_player_bullets  game_state =
  let p_bullets =
    game_state.p_bullets
      |> List.map (fun (x, y) -> (x, y - 8))
      |> List.filter (fun (x, y) -> y > -20)
  in
  { game_state with p_bullets }


let player_shooting  player p_bullets t =
  if player.p_shooting
  && t - player.p_last_shot > player.p_shoot_freq
  then (* shoot *)
    let w, h, r = player.p_tex_dims in
    let w, h = (w / r, h / r) in
    let x, y = player.p_pos in
    let bw, bh = (20, 20) in  (* bullet_size *)
    let bullet = (x + w/2 - bw/2, y - bh) in
    let player = { player with p_last_shot = t } in
    (player, bullet :: p_bullets)
  else
    (player, p_bullets)


let step_player  game_state t =
  let player = player_moving game_state.player in
  let player, p_bullets = player_shooting  player game_state.p_bullets t in
  { game_state with player; p_bullets }


let rec main_loop  game_state game_data =
  let t1 = PausableTimer.get_ticks timer in

  let game_state = event_loop  game_state game_data in

  let t = PausableTimer.get_ticks timer in

  let game_state =
    if PausableTimer.is_paused timer
    then (game_state)
    else
      let game_state = step_foes  game_state game_data t in
      let game_state = step_foes_bullets  game_state t in
      let game_state = step_player_bullets  game_state in
      let game_state = step_player  game_state t in
      (game_state)
  in

  display ~playing:true game_state game_data;

  let player_is_touched = player_touched  game_state in

  let t2 = PausableTimer.get_ticks timer in
  let dt = t2 - t1 in

  Timer.delay (max 0 (30 - dt));

  if player_is_touched
  then begin
    Printf.printf "# shot: %d\n" !shot;
    Printf.printf "# missed: %d\n" !missed;
    Printf.printf "# score: %d\n\n%!" (!shot - !missed);
    game_over  game_state game_data
  end
  else main_loop  game_state game_data


and game_over  game_state game_data =
  let restart = event_restart () in
  if not restart then begin
    display ~playing:false game_state game_data;
    Timer.delay 200;
    game_over  game_state game_data
  end else begin
    let game_state = reinit_game game_state game_data in
    main_loop  game_state game_data
  end


and reinit_game game_state game_data =
  shot := 0;
  missed := 0;

  let p_pos =
    let w, h, r = game_state.player.p_tex_dims in
    let w, h = (w / r, h / r) in
    (width / 2 - w / 2, height - 140)
  in
  let player = { game_state.player with
    p_pos;
    p_last_shot = PausableTimer.get_ticks timer;
    p_shooting = false;
    p_dir =
      { left = false;
        right = false;
        up = false;
        down = false;
      };
  } in

  let game_state = {
    player;
    foes = [];
    p_bullets = [];
    f_bullets = [];
  } in
  (game_state)


let init_game renderer =
  let fb_pattern = [|
    [| 0; 0; 0; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 0; 0; 0 |];
    [| 0; 1; 0; 1; 0 |];
    [| 0; 0; 0; 0; 0 |];
  |] in
  let pb_pattern = [|
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
    [| 0; 0; 0; 0; 0 |];
    [| 1; 0; 0; 0; 1 |];
    [| 1; 0; 0; 0; 1 |];
  |] in

  let f_bullet_tex = texture_of_pattern renderer fb_pattern ~color:yellow in
  let p_bullet_tex = texture_of_pattern renderer pb_pattern ~color:green in

  let gp =
    let cwd = Sys.getcwd () in
    Filename.concat cwd (Filename.dirname Sys.argv.(0))
  in
  let deep_space_dir =
    (String.concat Filename.dir_sep [gp; "Commander"; "deep-space-ships"])
  in
  let armada_dir =
    (String.concat Filename.dir_sep [gp; "Commander"; "spaceshipspack"])
  in
  let realistic_dir =
    (String.concat Filename.dir_sep [gp; "Commander"; "Realistic_Ships"])
  in
  let com_pack3_dir =
    (String.concat Filename.dir_sep [gp; "Commander"; "Space_Ships_pack3"])
  in
  let ships_files = [|
    deep_space_dir, "dipy00.bmp", 4;
    deep_space_dir, "dipy02.bmp", 4;
    deep_space_dir, "dipy03.bmp", 4;
    deep_space_dir, "dipy04.bmp", 4;
    deep_space_dir, "dipy05.bmp", 4;
    deep_space_dir, "dipy05a.bmp", 4;
    deep_space_dir, "dipy08.bmp", 4;
    deep_space_dir, "dipy09.bmp", 3;
    deep_space_dir, "dipy10.bmp", 4;
    deep_space_dir, "dipy12.bmp", 3;
    deep_space_dir, "dipy12a.bmp", 3;
    deep_space_dir, "dipy14.bmp", 3;
    deep_space_dir, "dipy15.bmp", 3;
    deep_space_dir, "dipy17.bmp", 3;
    deep_space_dir, "dipy18.bmp", 3;

    armada_dir, "a01.bmp", 4;
    armada_dir, "a05.bmp", 4;
    armada_dir, "a09.bmp", 4;
    armada_dir, "a14.bmp", 2;
    armada_dir, "a15.bmp", 2;
    armada_dir, "a18.bmp", 2;

    realistic_dir, "a3.bmp", 2;
    realistic_dir, "a4.bmp", 2;
    realistic_dir, "a2.bmp", 2;
    realistic_dir, "a7.bmp", 2;
    realistic_dir, "a6.bmp", 2;

    com_pack3_dir, "a16.bmp", 2;
    com_pack3_dir, "a20.bmp", 2;
    com_pack3_dir, "a21.bmp", 2;
    com_pack3_dir, "a24.bmp", 2;
    com_pack3_dir, "a28.bmp", 2;
    com_pack3_dir, "a31.bmp", 2;
    com_pack3_dir, "a32.bmp", 1;
    com_pack3_dir, "a35.bmp", 2;
    com_pack3_dir, "a38.bmp", 2;
  |] in
  let ship_player =
    deep_space_dir, "dipy16.bmp", 4
  in

  let load_ship (dir, filename, ratio) =
    let file_path = Filename.concat dir filename in
    let surf = Surface.load_bmp file_path in
    let p0 = Surface.get_pixel8_unsafe surf 0 0 in
    let p1 = Surface.get_pixel8_unsafe surf 1 0 in
    let p2 = Surface.get_pixel8_unsafe surf 2 0 in
    let w, h = Surface.get_dims surf in
    let dims = (w, h, ratio) in
    begin (* transparent background *)
      let rgb =
        (Int32.to_int p2,
         Int32.to_int p1,
         Int32.to_int p0)
      in
      let key = pixel_for_surface surf rgb in
      Surface.set_color_key surf ~enable:true ~key;
    end;
    let tex = Texture.create_from_surface renderer surf in
    Surface.free surf;
    (tex, dims)
  in
  let ships =
    Array.map load_ship ships_files
  in
  let foes_tex = Array.map fst ships in
  let foes_tex_dims = Array.map snd ships in
  let foes_tex_n = Array.length foes_tex in

  let player_texture, p_tex_dims =
    load_ship ship_player
  in
  let p_pos =
    let w, h, r = p_tex_dims in
    let w, h = (w / r, h / r) in
    (width / 2 - w / 2, height - 140)
  in
  let player = {
    p_pos;
    p_last_shot = PausableTimer.get_ticks timer;
    p_shoot_freq = 200;
    p_shooting = false;
    p_dir =
      { left = false;
        right = false;
        up = false;
        down = false;
      };
    p_texture = player_texture;
    p_tex_dims;
  } in

  let letters_tex =
    List.map (fun (c, pat) ->
      let tex = texture_of_pattern renderer pat ~color:green in
      (c, tex)
    ) letters
  in

  let game_state = {
    player;
    foes = [];
    p_bullets = [];
    f_bullets = [];
  } in
  let game_data = {
    f_bullet_tex;
    p_bullet_tex;
    foes_tex;
    foes_tex_dims;
    foes_tex_n;
    letters_tex;
    renderer;
  } in
  (game_state, game_data)


let main () =
  Random.self_init ();
  Sdl.init [`VIDEO; `JOYSTICK];
  let window, renderer =
    Render.create_window_and_renderer ~width ~height ~flags:[]
  in
  Render.set_logical_size2 renderer width height;
  Window.set_title ~window ~title:"Shmup-ds";

  let joy_num = Joystick.num_joysticks () in
  if joy_num >= 1
  then ignore(Joystick.j_open 0);

  Mouse.show_cursor false;

  let game_state, game_data = init_game renderer in

  main_loop  game_state game_data


let () =
  try main ()
  with Failure msg ->
    prerr_endline msg;
    prerr_endline (Sdlerror.get_error ());
    exit 1;
;;
