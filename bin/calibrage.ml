open Bimage_display
open Bimage

let show = ref true
let temp = ref []
let selectedColors = ref []

let rec getcolorrange
    ?range:((minh, maxh), (mins, maxs), (minv, maxv) =
        ((1., 0.), (1., 0.), (1., 0.))) list =
  match list with
  | (h, s, v) :: q ->
      getcolorrange
        ~range:
          ( (min minh h, max maxh h),
            (min mins s, max maxs s),
            (min minv v, max maxv v) )
        q
  | [] ->
      Printf.printf "resultat : ((%f,%f), (%f, %f), (%f, %f))\n%!" minh maxh
        mins maxs minv maxv

let key_action _window key _ _ _ =
  if key = GLFW.Backspace && !temp <> [] then temp := List.tl !temp
  else if key = GLFW.Enter then (
    Window.close _window;
    selectedColors := List.append !temp !selectedColors)
  else if key = GLFW.Escape then (
    Window.close _window;
    selectedColors := List.append !temp !selectedColors;
    show := false;
    getcolorrange !selectedColors)

let mouse_callback image _window _ _ _ =
  let x, y = Window.get_mouse_position _window in
  let x = Float.to_int x in
  let y = Float.to_int y in
  let pixel = Pixel.of_rgb Color.hsv (Image.get_pixel image x y) in
  let h = Pixel.get pixel 0 in
  let s = Pixel.get pixel 1 in
  let v = Pixel.get pixel 2 in
  if !temp = [] || List.hd !temp <> (h, s, v) then (
    temp := (h, s, v) :: !temp;
    Printf.printf "%d, %d : (%f, %f, %f)\n%!" x y h s v)

let () =
  let list_img =
    ref
      (Sys.readdir Sys.argv.(1)
      |> Array.to_list
      |> List.filter (fun x -> Filename.extension x = ".jpeg"))
  in
  for _ = 0 to List.length !list_img - 1 do
    if !show then (
      let image =
        Bimage_io.read f32 rgb (Sys.argv.(1) ^ List.hd !list_img)
        |> Result.get_ok
      in
      list_img := List.tl !list_img;
      let heigth, width, _ = Image.shape image in
      let ty = Image.ty image in
      let imageup = Image.v ty rgb (heigth * 2) (width * 2) in
      Image.for_each_pixel
        (fun x y pix ->
          Image.set_pixel imageup (x * 2) (y * 2) pix;
          Image.set_pixel imageup (x * 2) ((y * 2) + 1) pix;
          Image.set_pixel imageup ((x * 2) + 1) (y * 2) pix;
          Image.set_pixel imageup ((x * 2) + 1) ((y * 2) + 1) pix)
        image;
      let window =
        Window.create ("pick a color from " ^ List.hd !list_img) imageup ()
      in
      let () = Window.on_mouse_button (mouse_callback imageup) window in
      let () = Window.on_key key_action window in
      Bimage_display.show_all [ window ])
  done
