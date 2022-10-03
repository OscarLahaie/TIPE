open Bimage

let is_circle ~x ~y ~r ~step ~accuracy image =
  let valid_points = ref 0 in
  for i = 1 to step do
    let is_white pixel =
      Pixel.get pixel 0 > 0. && Pixel.get pixel 1 > 0. && Pixel.get pixel 2 > 0.
    in
    let x_radius =
      x
      + r
        * int_of_float
            (cos (2. *. atan 1. *. float_of_int i /. float_of_int step))
    in
    let y_radius =
      y
      + r
        * int_of_float
            (sin (2. *. atan 1. *. float_of_int i /. float_of_int step))
    in
    if is_white (Image.get_pixel image x_radius y_radius) then incr valid_points
  done;
  float_of_int !valid_points /. float_of_int step >= accuracy

