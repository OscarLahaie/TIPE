open Bimage_display
open Bimage

let () =
  let t = Sys.time () in
  let original = Bimage_io.read f32 rgb Sys.argv.(1) |> Result.get_ok in

  let ty = Image.ty original in
  let heigth, width, _ = Image.shape original in
  let grayscale = Image.v ty gray heigth width in
  let hsvImage = Image.v ty Color.hsv heigth width in
  Image.for_each_pixel
    (fun x y pix -> Image.set_pixel grayscale x y (Pixel.of_rgb gray pix))
    original;
  print_newline ();
  print_endline ("After grayscale : " ^ string_of_float (Sys.time () -. t));
  flush_all ();
  (*   let gauss = TIPE.Canny.noise_reduction_gray ~image:grayscale ~mask_size:3 in
 *)
  print_endline ("After noise : " ^ string_of_float (Sys.time () -. t));
  flush_all ();
  Image.convert_to ~dest:hsvImage original;
  let gauss = grayscale in
  let gradient_gray = TIPE.Canny.gradient_map_gray gauss in
  print_endline ("After gradient : " ^ string_of_float (Sys.time () -. t));
  flush_all ();
  let color_in_range pixel =
    let hue, saturation, value =
      (Pixel.get pixel 0, Pixel.get pixel 1, Pixel.get pixel 2)
    in
    (hue >= -0.055556 && hue < 0.078591)
    && saturation > 0.405405 && value > 0.141176 && value < 1.
  in
  let gradient_rgb = Image.copy gradient_gray in

  let pixel_selection = Hashtbl.create (heigth * width / 8) in

  let filtrer x y pix =
    let value = Pixel.get (Image.get_pixel gradient_gray x y) 0 in
    if color_in_range pix && value > 0.01 then (
      Image.set_pixel gradient_rgb x y (Pixel.v rgb [ 1.; 0.; 0. ]);
      Hashtbl.add pixel_selection (x, y) pix)
    else Image.set_pixel gradient_rgb x y (Pixel.v rgb [ 0.; 0.; 0. ])
  in
  Image.for_each_pixel filtrer hsvImage;
  print_endline ("After filtrage : " ^ string_of_float (Sys.time () -. t));
  flush_all ();
  print_newline ();
  print_string "Nombre de pixels avant erosion : ";
  print_int (Hashtbl.length pixel_selection);
  print_endline "";
  let tab =
    TIPE.Erosion_optimise.erosion ~surrounding_pixels:4 ~round:2 pixel_selection
  in
  print_endline ("After erosion : " ^ string_of_float (Sys.time () -. t));
  flush_all ();
  print_endline "Après erosion : ";
  print_int (Hashtbl.length tab);
  print_endline "";

  Image.for_each_pixel
    (fun x y _ ->
      if Hashtbl.mem tab (x, y) then
        Image.set_pixel gradient_rgb x y (Pixel.v rgb [ 1.; 0.; 0. ])
      else Image.set_pixel gradient_rgb x y (Pixel.v rgb [ 0.; 0.; 0. ]))
    gradient_rgb;
  let circles =
    TIPE.Circle_detection_optimise.potential_circles ~r_min:10 ~r_max:50
      ~step:50 pixel_selection
  in
  print_endline ("After circles : " ^ string_of_float (Sys.time () -. t));
  (* let () = TIPE.Extract_data.save_sub_pictures original circles "subtest" in
   *)
  let circle = Image.copy original in
  print_endline "Nombre de cercles : ";
  print_int (List.length circles);
  print_endline "";
  List.iter
    (fun (x, y, r) ->
      TIPE.Circle_detection_optimise.draw_circle_square ~x ~y ~r circle)
    (TIPE.Circle_detection_optimise.circles_group circles);
  print_endline ("After circles : " ^ string_of_float (Sys.time () -. t));
  let gradient_rgb = Window.create "gradientrgb" gradient_rgb () in

  let gradient_r = Window.create "gradientrgb" circle () in
  Bimage_display.show_all [ gradient_rgb (* gradient_r *) ]
