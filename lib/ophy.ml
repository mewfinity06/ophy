(* Types *)
type point =
  { x : float
  ; y : float
  }

type circle =
  { pos : point
  ; radius : float
  }

type rect =
  { pos : point
  ; width : float
  ; height : float
  }

type line =
  { p1 : point
  ; p2 : point
  }

(* Functions *)
let distance p1 p2 =
  let dist_x = p1.x -. p2.x in
  let dist_y = p1.y -. p2.y in
  sqrt ((dist_x *. dist_x) +. (dist_y *. dist_y))
;;

let is_collision_p2p (p1 : point) (p2 : point) = p1 = p2
let is_collision_p2c (p1 : point) (c1 : circle) = distance p1 c1.pos <= c1.radius

let is_collision_c2c (c1 : circle) (c2 : circle) =
  let dist_center = distance c1.pos c2.pos in
  dist_center <= c1.radius +. c2.radius
;;

let is_collision_p2r (p1 : point) (r1 : rect) =
  let { x = px; y = py } = p1 in
  let { pos = rp; width = rw; height = rh } = r1 in
  px >= rp.x && rp.x <= rp.x +. rw && py >= rp.y && py <= rp.y +. rh
;;

let is_collision_r2r (r1 : rect) (r2 : rect) =
  let { pos = { x = r1x; y = r1y }; width = r1w; height = r1h } = r1 in
  let { pos = { x = r2x; y = r2y }; width = r2w; height = r2h } = r2 in
  r1x +. r1w >= r2x && r1x <= r2x +. r2w && r1y +. r1h >= r2y && r1y <= r2y +. r2h
;;

let is_collision_c2r (c : circle) (r : rect) =
  let test_x = ref 0.0 in
  let test_y = ref 0.0 in
  if c.pos.x < r.pos.x
  then test_x := r.pos.x
  else if c.pos.x > r.pos.x +. r.width
  then test_x := r.pos.x +. r.width;
  if c.pos.y < r.pos.y
  then test_y := r.pos.y
  else if c.pos.y > r.pos.y +. r.height
  then test_y := r.pos.y +. r.height;
  let dist_x = c.pos.x -. !test_x in
  let dist_y = c.pos.y -. !test_y in
  let dist = sqrt ((dist_x *. dist_x) +. (dist_y *. dist_y)) in
  dist <= c.radius
;;

let is_collision_l2p_buffer (b : float) (l : line) (p : point) =
  let d1 = distance p l.p1 in
  let d2 = distance p l.p2 in
  let line_len = distance l.p1 l.p2 in
  d1 +. d2 >= line_len -. b && d1 +. d2 <= line_len +. b
;;

let is_collision_l2p = is_collision_l2p_buffer 0.1

let is_collision_l2c (l : line) (c : circle) =
  let inside_1 = is_collision_p2c l.p1 c in
  let inside_2 = is_collision_p2c l.p2 c in
  if inside_1 || inside_2
  then true
  else (
    let line_len = distance l.p1 l.p2 in
    let dot =
      (((c.pos.x -. l.p1.x) *. (l.p2.x -. l.p1.x))
       +. ((c.pos.y -. l.p1.y) *. (l.p2.y -. l.p1.y)))
      /. (line_len ** 2.)
    in
    let closest_x = l.p1.x +. (dot *. (l.p2.x -. l.p1.x)) in
    let closest_y = l.p1.y +. (dot *. (l.p2.y -. l.p1.y)) in
    let on_seg = is_collision_l2p l { x = closest_x; y = closest_y } in
    if not on_seg
    then false
    else (
      let dist_x = closest_x -. c.pos.x in
      let dist_y = closest_y -. c.pos.y in
      let dist = sqrt ((dist_x *. dist_x) +. (dist_y *. dist_y)) in
      dist <= c.radius))
;;

let is_collision_l2l (l1 : line) (l2 : line) =
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
