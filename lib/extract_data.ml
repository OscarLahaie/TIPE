open Bimage

let distance (x1, y1) (x2, y2) =
  int_of_float
    (sqrt
       (((float_of_int x1 -. float_of_int x2) ** 2.)
       +. (float_of_int y1 -. float_of_int y2)))

let save_circle folder_name original circle num =
  let x, y, r = circle in
  let x_start = x - r in
  let y_start = y - r in
  let crop =
    Image.crop ~x:x_start ~y:y_start ~width:(2 * r) ~height:(2 * r) original
  in
  ignore
    (Bimage_io.write (folder_name ^ "/" ^ string_of_int num ^ ".jpeg") crop)

let save_sub_pictures original circles folder_name =
  List.iteri (fun i c -> save_circle folder_name original c i) circles