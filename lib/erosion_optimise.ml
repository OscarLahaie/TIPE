open Bimage

let is_isolated pixel_selection x y surrounding_pixels =
  List.length
    (List.filter
       (function
         | None -> false
         | Some pixel ->
             Pixel.get pixel 0 > 0.
             && Pixel.get pixel 1 > 0.
             && Pixel.get pixel 2 > 0.)
       [
         Hashtbl.find_opt pixel_selection (x, y - 1);
         Hashtbl.find_opt pixel_selection (x, y + 1);
         Hashtbl.find_opt pixel_selection (x + 1, y);
         Hashtbl.find_opt pixel_selection (x - 1, y);
         Hashtbl.find_opt pixel_selection (x - 1, y - 1);
         Hashtbl.find_opt pixel_selection (x + 1, y + 1);
         Hashtbl.find_opt pixel_selection (x + 1, y - 1);
         Hashtbl.find_opt pixel_selection (x - 1, y + 1);
       ])
  < surrounding_pixels

(* let is_surrounded pixel_selection x y surrounding_pixels =
   List.length
     (List.filter
        (function
          | None -> false
          | Some pixel ->
              Pixel.get pixel 0 > 0.
              && Pixel.get pixel 1 > 0.
              && Pixel.get pixel 2 > 0.)
        [
          Hashtbl.find_opt pixel_selection (x, y - 1);
          Hashtbl.find_opt pixel_selection (x, y + 1);
          Hashtbl.find_opt pixel_selection (x + 1, y);
          Hashtbl.find_opt pixel_selection (x - 1, y);
          Hashtbl.find_opt pixel_selection (x - 1, y - 1);
          Hashtbl.find_opt pixel_selection (x + 1, y + 1);
          Hashtbl.find_opt pixel_selection (x + 1, y - 1);
          Hashtbl.find_opt pixel_selection (x - 1, y + 1);
        ])
   < surrounding_pixels *)

let erosion ~surrounding_pixels ~round pixel_selection =
  let selection = ref (Hashtbl.copy pixel_selection) in
  for _ = 1 to round do
    let transition = Hashtbl.copy !selection in
    Hashtbl.iter
      (fun (x, y) _ ->
        if is_isolated pixel_selection x y surrounding_pixels then
          Hashtbl.remove transition (x, y))
      pixel_selection;
    selection := transition
  done;
  !selection

(* let rev_erosion ~surrounding_pixels ~round pixel_selection =
   let selection = ref (Hashtbl.copy pixel_selection) in
   for _ = 1 to round do
     let transition = Hashtbl.copy !selection in

   done;
   !selection
*)
let filter_noise ~surrounding_pixels ~round
    (pixel_selection : (int * int, 'a Bimage.Pixel.t) Hashtbl.t) =
  pixel_selection |> erosion ~surrounding_pixels ~round
(*  |> rev_erosion ~surrounding_pixels ~round *)
