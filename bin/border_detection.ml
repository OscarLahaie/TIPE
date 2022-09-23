open Bimage_display
open Bimage

let () =
  let original = Bimage_io.read f32 rgb Sys.argv.(1) |> Result.get_ok in
  let r_image = Image.copy original in
  let g_image = Image.copy original in
  let b_image = Image.copy original in

  (* Couleurs *)
  let () =
    Image.for_each_pixel
      (fun x y _ ->
        let current_pixel = Image.get_pixel original x y in
        Image.set_pixel r_image x y
          (Pixel.v rgb [ Pixel.get current_pixel 0; 0.; 0. ]);
        Image.set_pixel g_image x y
          (Pixel.v rgb [ 0.; Pixel.get current_pixel 1; 0. ]);
        Image.set_pixel b_image x y
          (Pixel.v rgb [ 0.; 0.; Pixel.get current_pixel 2 ]))
      original
  in
  let original = Window.create "original" original () in
  let r_only = Window.create "red" r_image () in
  let g_only = Window.create "green" g_image () in
  let b_only = Window.create "blue" b_image () in
  Bimage_display.show_all [ original; r_only; g_only; b_only ]