open Bimage_display
open Bimage

let mouse_callback image _window x y =
  let x = Float.to_int x in
  let y = Float.to_int y in
  Printf.printf "%d, %d, color : %f %f %f \n%!" x y
    (Pixel.get (Image.get_pixel image x y) 0)
    (Pixel.get (Image.get_pixel image x y) 1)
    (Pixel.get (Image.get_pixel image x y) 2)

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
    |> TIPE.Erosion.filter_noise ~surrounding_pixels:4 ~round:10
    |> TIPE.Erosion.filter_noise ~surrounding_pixels:4 ~round:10
  in

  let original = Window.create "original" original () in
  let hsv = Window.create "hsv" hsv_rgb () in
  let filter = Window.create "filter" image () in
  let erosion = Window.create "Erosion" erosion () in
  let () = Window.on_mouse_move (mouse_callback hsv_rgb) filter in
  Bimage_display.show_all [ original; hsv; filter; erosion ]
