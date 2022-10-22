open Bimage_display
open Bimage

let () =
  let original = Bimage_io.read f32 rgb Sys.argv.(1) |> Result.get_ok in
  let width, height, _ = Image.shape original in
  let pixel_selection : (int * int, 'a Bimage.Pixel.t) Hashtbl.t =
    Hashtbl.create (width * height)
  in

  (* Couleurs *)
  let color_in_range pixel =
    let hue, saturation, value =
      (Pixel.get pixel 0, Pixel.get pixel 1, Pixel.get pixel 2)
    in
    (hue >= 0. && hue < 0.1) && saturation > 0.3 && value > 0. && value < 1.
  in

  let () =
    Image.for_each_pixel
      (fun x y pix ->
        if color_in_range (Pixel.of_rgb Color.hsv pix) then
          Hashtbl.add pixel_selection (x, y) pix)
      original
  in

  let pixel_selection =
    pixel_selection
    |> TIPE.Erosion_optimise.filter_noise ~surrounding_pixels:4 ~round:3
  in

  let circles =
    TIPE.Circle_detection_optimise.potential_circles ~r_min:20 ~r_max:100
      ~step:50 pixel_selection
  in
  let () =
    List.iter
      (fun (x, y, r) ->
        if
          TIPE.Circle_detection_optimise.is_circle ~x ~y ~r ~step:50
            ~accuracy:0.5 pixel_selection
        then TIPE.Circle_detection_optimise.draw_circle ~x ~y ~r original)
      circles
  in
  (* Filtre chromatique *)
  let original = Window.create "original" original () in
  Bimage_display.show_all [ original ]
