open Bimage_display
open Bimage

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

  let original = Window.create "original" original () in
  let hsv = Window.create "hsv" hsv_rgb () in

  Bimage_display.show_all [ original; hsv ]
