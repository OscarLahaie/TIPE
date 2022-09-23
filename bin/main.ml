open Bimage_display
open Bimage

let color_difference pixel1 pixel2 =
  let r1, g1, b1 =
    (Pixel.get pixel1 0, Pixel.get pixel1 1, Pixel.get pixel1 2)
  in
  let r2, g2, b2 =
    (Pixel.get pixel2 0, Pixel.get pixel2 1, Pixel.get pixel2 2)
  in
  (abs_float (r1 -. r2), abs_float (g1 -. g2), abs_float (b1 -. b2))

let is_red pixel =
  let dr, dg, db = color_difference (Pixel.v rgb [ 1.; 0.; 0. ]) pixel in
  dr < 0.4 && dg < 0.45 && db < 0.45

let color_difference_avg pixel1 pixel2 =
  let r1, g1, b1 =
    (Pixel.get pixel1 0, Pixel.get pixel1 1, Pixel.get pixel1 2)
  in
  let r2, g2, b2 =
    (Pixel.get pixel2 0, Pixel.get pixel2 1, Pixel.get pixel2 2)
  in
  (abs_float (r1 -. r2) +. abs_float (g1 -. g2) +. abs_float (b1 -. b2)) /. 3.

let is_border image x y =
  let width, height, _ = Image.shape image in
  let center = Image.get_pixel image x y in
  if x != 0 && x != width - 1 && y != 0 && y != height - 1 then
    List.exists
      (fun pixel ->
        color_difference_avg center pixel > 0.1
        && (not (is_red pixel))
        && is_red center)
      [
        Image.get_pixel image x (y - 1);
        Image.get_pixel image x (y + 1);
        Image.get_pixel image (x + 1) y;
        Image.get_pixel image (x - 1) y;
        Image.get_pixel image (x - 1) (y - 1);
        Image.get_pixel image (x + 1) (y + 1);
        Image.get_pixel image (x + 1) (y - 1);
        Image.get_pixel image (x - 1) (y + 1);
      ]
  else false

let mouse_callback image _window x y =
  let x = Float.to_int x in
  let y = Float.to_int y in
  Printf.printf "%d, %d, color : %f %f %f\n %b \n%!" x y
    (Pixel.get (Image.get_pixel image x y) 0)
    (Pixel.get (Image.get_pixel image x y) 1)
    (Pixel.get (Image.get_pixel image x y) 2)
    (is_border image x y)

let () =
  let original = Bimage_io.read f32 rgb Sys.argv.(1) |> Result.get_ok in
  let image = Image.copy original in
  (* let width, _, _ = (Image.shape image) in *)
  (* Filtre chromatique *)
  let () =
    Image.for_each_pixel
      (fun x y _ ->
        let current_pixel = Image.get_pixel image x y in
        let r, g, b =
          ( Pixel.get current_pixel 0,
            Pixel.get current_pixel 1,
            Pixel.get current_pixel 2 )
        in
        let dr, dg, db =
          color_difference current_pixel (Pixel.v rgb [ 1.; 0.; 0. ])
        in
        Image.set_pixel image x y
          (if is_red current_pixel then current_pixel
          else
            Pixel.v rgb
              [
                (1. -. dr) *. (1. -. dg) *. (1. -. db) *. r;
                (1. -. dr) *. (1. -. dg) *. (1. -. db) *. g;
                (1. -. dr) *. (1. -. dg) *. (1. -. db) *. b;
              ]))
      image
  in
  (* Detection de contours *)
  let filter = Image.copy image in
  let () =
    Image.for_each_pixel
      (fun x y _ ->
        let current_pixel = Image.get_pixel original x y in
        Image.set_pixel image x y
          (if is_border image x y then Pixel.v rgb [ 1.; 1.; 1. ]
          else Pixel.v rgb [ 0.; 0.; 0. ]))
      image
  in
  (* Erosion *)
  let erosion = Image.copy image in
  let erosion =
    TIPE.Erosion.filter_noise ~surrounding_pixels:3 ~round:1 erosion
  in

  (* Affichage *)
  let original = Window.create "Original" original () in
  let filter = Window.create "Filter" filter () in
  let modified = Window.create "Border" image () in
  let erosion = Window.create "Erosion" erosion () in
  let () = Window.on_mouse_move (mouse_callback image) modified in
  Bimage_display.show_all [ original; filter; modified ]
