open Bimage_display
open Bimage

let mouse_callback image _window x y =
  let x = Float.to_int x in
  let y = Float.to_int y in
  let est_cercle = ref false in
  let r = ref 10 in
  while
    est_cercle :=
      TIPE.Circle_detection.is_circle ~x ~y ~r:!r ~accuracy:0.6 ~step:100 image;
    (not !est_cercle) && !r < 100
  do
    incr r
  done;
  if !est_cercle then (
    print_endline "Est cercle";
    TIPE.Circle_detection.draw_circle ~x ~y ~r:!r image)
  else print_endline "Pas un cercle"

let () =
  let original = Bimage_io.read f32 rgb Sys.argv.(1) |> Result.get_ok in
  let width, height, _ = Image.shape original in
  let hsv = Image.v (Image.ty original) Color.hsv width height in
  let hsv_rgb = Image.v (Image.ty original) Color.rgb width height in

  (* Couleurs *)
  let () =
    Image.for_each_pixel
      (fun x y pix -> Image.set_pixel hsv x y (Pixel.of_rgb Color.hsv pix))
      original
  in
  let () =
    Image.for_each_pixel
      (fun x y pix ->
        Image.set_pixel hsv_rgb x y
          (Pixel.v rgb [ Pixel.get pix 0; Pixel.get pix 1; Pixel.get pix 2 ]))
      hsv
  in
  (* Filtre chromatique *)
  let image = Image.copy original in
  let () =
    Image.for_each_pixel
      (fun x y _ ->
        let current_pixel = Image.get_pixel hsv_rgb x y in
        let hue, saturation, value =
          ( Pixel.get current_pixel 0,
            Pixel.get current_pixel 1,
            Pixel.get current_pixel 2 )
        in
        Image.set_pixel image x y
          (if
           (hue >= 0. && hue < 0.1)
           && saturation > 0.3 && value > 0. && value < 1.
          then current_pixel
          else Pixel.v rgb [ 0.; 0.; 0. ]))
      hsv_rgb
  in
  let erosion =
    Image.copy image
    |> TIPE.Erosion.filter_noise ~surrounding_pixels:4 ~round:3
    |> TIPE.Erosion.filter_noise ~surrounding_pixels:4 ~round:3
  in

  let circles =
    TIPE.Circle_detection.potential_circles ~r_min:20 ~r_max:100 erosion
  in
  let () =
    List.iter
      (fun (x, y, r) ->
        if
          TIPE.Circle_detection.is_circle ~x ~y ~r ~step:100 ~accuracy:0.5
            erosion
        then TIPE.Circle_detection.draw_circle ~x ~y ~r erosion)
      circles
  in
  let original = Window.create "original" original () in
  let hsv = Window.create "hsv" hsv_rgb () in
  let filter = Window.create "filter" image () in
  let erosion_win = Window.create "Erosion" erosion () in
  let () = Window.on_mouse_move (mouse_callback erosion) erosion_win in
  Bimage_display.show_all [ original; hsv; filter; erosion_win ]
