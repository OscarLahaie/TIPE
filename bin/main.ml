open Bimage_display
open Bimage

let () =
  let t = Sys.time () in
  let original = Bimage_io.read f32 rgb Sys.argv.(1) |> Result.get_ok in

  let ty = Image.ty original in
  let heigth, width, _ = Image.shape original in
  let grayscale = Image.v ty gray heigth width in
  Image.for_each_pixel
    (fun x y pix -> Image.set_pixel grayscale x y (Pixel.of_rgb gray pix))
    original;

  let gauss = TIPE.Canny.noise_reduction_gray ~image:grayscale ~mask_size:5 in

  let gauss = gauss in
  let gradient_gray = TIPE.Canny.gradient_map_gray gauss in

  print_endline ("After circles : " ^ string_of_float (Sys.time () -. t));
  let gradient_gray = Window.create "gradientgray" gradient_gray () in
  let original = Window.create "original" original () in

  Bimage_display.show_all [ original; gradient_gray ]
