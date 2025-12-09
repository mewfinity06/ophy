module Point = struct
  type t =
    { x : float
    ; y : float
    }

  let distance p1 p2 =
    let dist_x = p1.x -. p2.x in
    let dist_y = p1.y -. p2.y in
    sqrt ((dist_x *. dist_x) +. (dist_y *. dist_y))
  ;;
end

module Circle = struct
  type t =
    { pos : Point.t
    ; radius : float
    }

  let area c = Float.pi *. (c.radius ** 2.)
  let diameter c = 2. *. c.radius
  let circumference c = 2. *. Float.pi *. c.radius
end

module Rect = struct
  type t =
    { pos : Point.t
    ; width : float
    ; height : float
    }

  let square p s = { pos = p; width = s; height = s }
  let area r = r.width *. r.height
  let perimeter r = 2. *. (r.width +. r.height)
  let diagonal r = sqrt ((r.width ** 2.) +. (r.height ** 2.))
end

module Line = struct
  type t =
    { p1 : Point.t
    ; p2 : Point.t
    }

  let slope t = (t.p2.y -. t.p1.y) /. (t.p2.x -. t.p1.x)
end

(** 
    The Collide module provides all the necessary functions to check
    if one type has collided with the other, i.e. {!Collide.p2p} checks if 
    a {!Point.t} has collided with another and {!Collide.l2c} checks if a 
    {!Line.t} collides with a {!Circle.t}. These functions are based off 
    of Jeffrey Thompson's
    {{:https://www.jeffreythompson.org/collision-detection/}Collision Detection}
*)
module Collide = struct
  let p2p (p1 : Point.t) (p2 : Point.t) = p1 = p2
  let p2c (p1 : Point.t) (c1 : Circle.t) = Point.distance p1 c1.pos <= c1.radius

  let c2c (c1 : Circle.t) (c2 : Circle.t) =
    Point.distance c1.pos c2.pos <= c1.radius +. c2.radius
  ;;

  let p2r (p1 : Point.t) (r1 : Rect.t) =
    let open Point in
    let open Rect in
    let { x = px; y = py } = p1 in
    let { pos = rp; width = rw; height = rh } = r1 in
    px >= rp.x && px <= rp.x +. rw && py >= rp.y && py <= rp.y +. rh
  ;;

  let r2r (r1 : Rect.t) (r2 : Rect.t) =
    let open Rect in
    let { pos = { x = r1x; y = r1y }; width = r1w; height = r1h } = r1 in
    let { pos = { x = r2x; y = r2y }; width = r2w; height = r2h } = r2 in
    r1x +. r1w >= r2x && r1x <= r2x +. r2w && r1y +. r1h >= r2y && r1y <= r2y +. r2h
  ;;

  let c2r (c : Circle.t) (r : Rect.t) =
    let test_x =
      if c.pos.x < r.pos.x
      then r.pos.x
      else if c.pos.x > r.pos.x +. r.width
      then r.pos.x +. r.width
      else c.pos.x
    in
    let test_y =
      if c.pos.y < r.pos.y
      then r.pos.y
      else if c.pos.y > r.pos.y +. r.height
      then r.pos.y +. r.height
      else c.pos.y
    in
    let dist_x = c.pos.x -. test_x in
    let dist_y = c.pos.y -. test_y in
    let dist = sqrt ((dist_x *. dist_x) +. (dist_y *. dist_y)) in
    dist <= c.radius
  ;;

  let l2p_buffer (b : float) (l : Line.t) (p : Point.t) =
    let d1 = Point.distance p l.p1 in
    let d2 = Point.distance p l.p2 in
    let len = Point.distance l.p1 l.p2 in
    d1 +. d2 >= len -. b && d1 +. d2 <= len +. b
  ;;

  let l2p = l2p_buffer 0.1

  let l2c (l : Line.t) (c : Circle.t) =
    let inside_1 = p2c l.p1 c in
    let inside_2 = p2c l.p2 c in
    if inside_1 || inside_2
    then true
    else (
      let len = Point.distance l.p1 l.p2 in
      let dot =
        (((c.pos.x -. l.p1.x) *. (l.p2.x -. l.p1.x))
         +. ((c.pos.y -. l.p1.y) *. (l.p2.y -. l.p1.y)))
        /. (len ** 2.)
      in
      let closest_x = l.p1.x +. (dot *. (l.p2.x -. l.p1.x)) in
      let closest_y = l.p1.y +. (dot *. (l.p2.y -. l.p1.y)) in
      let on_seg = l2p l { x = closest_x; y = closest_y } in
      if not on_seg
      then false
      else (
        let dist_x = closest_x -. c.pos.x in
        let dist_y = closest_y -. c.pos.y in
        let dist = sqrt ((dist_x *. dist_x) +. (dist_y *. dist_y)) in
        dist <= c.radius))
  ;;

  let l2l (l1 : Line.t) (l2 : Line.t) =
    let uA =
      (((l2.p2.x -. l2.p1.x) *. (l1.p1.y -. l2.p1.y))
       -. ((l2.p2.y -. l2.p1.y) *. (l1.p1.x -. l2.p1.x)))
      /. (((l2.p2.y -. l2.p1.y) *. (l1.p2.x -. l1.p1.x))
          -. ((l2.p2.x -. l2.p1.x) *. (l1.p2.y -. l1.p1.y)))
    in
    let uB =
      (((l1.p2.x -. l1.p1.x) *. (l1.p1.y -. l2.p1.y))
       -. ((l1.p2.y -. l1.p1.y) *. (l1.p1.x -. l2.p1.x)))
      /. (((l2.p2.y -. l2.p1.y) *. (l1.p2.x -. l1.p1.x))
          -. ((l2.p2.x -. l2.p1.x) *. (l1.p2.y -. l1.p1.y)))
    in
    uA >= 0. && uA <= 1. && uB >= 0. && uB <= 1.
  ;;
end
