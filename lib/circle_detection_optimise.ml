open Bimage

let not_black pixel =
  Pixel.get pixel 0 > 0. || Pixel.get pixel 1 > 0. || Pixel.get pixel 2 > 0.

let is_circle ~x ~y ~r ~step ~accuracy pixel_selection =
  let valid_points = ref 0 in
  for i = 1 to step do
    let is_white pixel =
      match pixel with
      | None -> false
      | Some pixel ->
          Pixel.get pixel 0 > 0.
          && Pixel.get pixel 1 > 0.
          && Pixel.get pixel 2 > 0.
    in
    let x_radius =
      x
      + int_of_float
          (float_of_int r
          *. cos (8. *. atan 1. *. float_of_int i /. float_of_int step))
    in
    let y_radius =
      y
      + int_of_float
          (float_of_int r
          *. sin (8. *. atan 1. *. float_of_int i /. float_of_int step))
    in
    if is_white (Hashtbl.find_opt pixel_selection (x_radius, y_radius)) then
      incr valid_points
  done;
  float_of_int !valid_points /. float_of_int step >= accuracy

(* let rec circles_group list =
  match list with
  | (x, y, r) :: q ->
      let i = ref 1 in
      let x_mid = ref x in
      let y_mid = ref y in
      let r_max = ref r in
      let is_close (x, y, r) =
        let dist =
          sqrt
            ((float_of_int (x - !x_mid) *. float_of_int (x - !x_mid))
            +. (float_of_int (y - !y_mid) *. float_of_int (y - !y_mid)))
        in
        if dist > float_of_int r then (
          r_max := !r_max + int_of_float dist;
          x_mid := ((!i * !x_mid) + x) / 2;
          y_mid := ((!i * !y_mid) + y) / 2;
          incr i;
          true)
        else false
      in
      (!x_mid, !y_mid, int_of_float (float_of_int !r_max *. 1.5))
      :: circles_group (List.filter is_close q)
  | [] -> []
 *)
let circles_group list =
  let list = List.fast_sort (fun (_, _, r1) (_, _, r2) -> r2 - r1) list in
  let rec aux list =
    match list with
    | (x, y, r) :: q ->
        let is_close (xq, yq, rq) =
          let dist =
            sqrt
              ((float_of_int (x - xq) *. float_of_int (x - xq))
              +. (float_of_int (y - yq) *. float_of_int (y - yq)))
          in
          if dist > float_of_int r then true else false
        in
        (x, y, r) :: aux (List.filter is_close q)
    | [] -> []
  in
  aux list

let potential_circles ~r_min ~r_max ~step pixel_selection =
  let circles = Hashtbl.create 500 in
  let add_points ~x ~y ~step =
    for r = r_min to r_max do
      for i = 1 to step do
        let x_radius =
          x
          + int_of_float
              (float_of_int r
              *. cos (8. *. atan 1. *. float_of_int i /. float_of_int step))
        in
        let y_radius =
          y
          + int_of_float
              (float_of_int r
              *. sin (8. *. atan 1. *. float_of_int i /. float_of_int step))
        in
        if Hashtbl.mem circles (x_radius, y_radius, r) then
          Hashtbl.replace circles (x_radius, y_radius, r)
            (Hashtbl.find circles (x_radius, y_radius, r) + 1)
        else Hashtbl.add circles (x_radius, y_radius, r) 1
      done
    done
  in
  Hashtbl.iter (fun (x, y) pix -> add_points ~x ~y ~step) pixel_selection;
  let liste = ref [] in
  Hashtbl.iter
    (fun (x, y, r) p ->
      if p > 20 && is_circle ~x ~y ~r ~step ~accuracy:0.5 pixel_selection then
        liste := (x, y, r) :: !liste)
    circles;
  !liste

let draw_circle ~x ~y ~r image =
  let step = 100 in
  for i = 1 to step do
    let x_radius =
      x
      + int_of_float
          (float_of_int r
          *. cos (8. *. atan 1. *. float_of_int i /. float_of_int step))
    in
    let y_radius =
      y
      + int_of_float
          (float_of_int r
          *. sin (8. *. atan 1. *. float_of_int i /. float_of_int step))
    in
    let width, height, _ = Image.shape image in
    if x >= 0 && y >= 0 && x < width && y < height then
      Image.set_pixel image x_radius y_radius (Pixel.v rgb [ 1.; 0.; 0. ])
  done

let draw_circle_square ~x ~y ~r image =
  let width, height, _ = Image.shape image in
  for i = 1 to 2 * r do
    if x + i - r >= 0 && y - r >= 0 && x + i - r < width && y - r < height then
      Image.set_pixel image (x + i - r) (y - r) (Pixel.v rgb [ 1.; 0.; 0. ]);
    if x + i - r >= 0 && y + r >= 0 && x + i - r < width && y + r < height then
      Image.set_pixel image (x + i - r) (y + r) (Pixel.v rgb [ 1.; 0.; 0. ]);
    if x - r >= 0 && y + i - r >= 0 && x - r < width && y + i - r < height then
      Image.set_pixel image (x - r) (y + i - r) (Pixel.v rgb [ 1.; 0.; 0. ]);
    if x + r >= 0 && y + i - r >= 0 && x + r < width && y + i - r < height then
      Image.set_pixel image (x + r) (y + i - r) (Pixel.v rgb [ 1.; 0.; 0. ])
  done
