open Bimage

let draw_line ~x ~y ~theta ~length image =
  let step = length in
  for i = 1 to step / 2 do
    let x_radius = x + int_of_float (float_of_int i *. cos theta) in
    let y_radius = y + int_of_float (float_of_int i *. sin theta) in
    let width, height, _ = Image.shape image in
    if x >= 0 && y >= 0 && x < width && y < height then
      Image.set_pixel image x_radius y_radius (Pixel.v rgb [ 1.; 0.; 0. ])
  done

(* let potential_lines ~length_min ~length_max pixel_selection =
   for i = 0 to 180 do
     let theta = float_of_int i /. 180. *. atan 1. *. 4. in
     let phi = int_of_float (float_of_int x *. cos theta +. float_of_int y *. sin theta) *)
