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

let potential_circles ~r_min ~r_max ~step pixel_selection =
  let circles = Hashtbl.create 100 in
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
    (fun (x, y, r) p -> if p > 10 then liste := (x, y, r) :: !liste)
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
