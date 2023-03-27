open Bimage

let draw_line ~phi ~theta ~length image =
  let x = int_of_float (cos theta *. float_of_int phi) in
  let y = int_of_float (sin theta *. float_of_int phi) in
  let theta = theta +. (atan 1. *. 2.) in
  let step = length in
  for i = 1 to step / 2 do
    let x_radius = x + int_of_float (float_of_int i *. cos theta) in
    let y_radius = y + int_of_float (float_of_int i *. sin theta) in
    let width, height, _ = Image.shape image in
    if x >= 0 && y >= 0 && x < width && y < height then
      Image.set_pixel image x_radius y_radius (Pixel.v rgb [ 1.; 0.; 0. ])
  done

let potential_lines ~length_min ~length_max pixel_selection =
  let table = Hashtbl.create (Hashtbl.length pixel_selection) in
  let pixel x y =
    for i = 0 to 180 do
      let theta = float_of_int i /. 180. *. atan 1. *. 4. in
      let phi =
        int_of_float
          ((float_of_int x *. cos theta) +. (float_of_int y *. sin theta))
      in
      if Hashtbl.mem table (phi, theta) then
        Hashtbl.replace table (phi, theta) (Hashtbl.find table (phi, theta) + 1)
      else Hashtbl.add table (phi, theta) 1
    done
  in
  Hashtbl.iter pixel pixel_selection;
  table
