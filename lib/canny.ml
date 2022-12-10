open Bimage

let gaussian_mask mask_size =
  (* Ecart type de la distribution gaussienne *)
  let gauss = 1.4 in
  let sqrt_gauss = gauss *. gauss in
  let operator x y =
    int_of_float
      (1.
      /. (4. *. atan 1. *. sqrt_gauss)
      *. exp
           (0.
           -. (float_of_int (x * x) +. float_of_int (y * y))
              /. (2. *. sqrt_gauss))
      *. 100.)
  in
  let convolution_array = Array.make_matrix mask_size mask_size 0 in
  let sum = ref 0 in
  for i = 0 to mask_size - 1 do
    for j = 0 to mask_size - 1 do
      let res = operator (i - (mask_size / 2)) (j - (mask_size / 2)) in
      sum := res + !sum;
      convolution_array.(i).(j) <- res
    done
  done;
  (!sum, convolution_array)

let rec ( |+| ) list1 list2 =
  match (list1, list2) with
  | [], _ -> list2
  | _, [] -> list1
  | h1 :: t1, h2 :: t2 -> (h1 +. h2) :: (t1 |+| t2)

let ( *| ) lambda list = List.map (fun x -> lambda *. x) list

let noise_reduction_gray ~image ~mask_size =
  let new_image = Image.copy image in
  let sum, convolution_array = gaussian_mask mask_size in
  let ty = Image.ty image in
  let heigth, width, _ = Image.shape image in
  let rgb_image = Image.v ty rgb heigth width in
  let apply_convolution x y =
    Image.set_pixel new_image x y (Pixel.v gray [ 0. ]);
    for i = 0 to mask_size - 1 do
      for j = 0 to mask_size - 1 do
        try
          let current_pixel = Image.get_pixel image x y in
          let current_data = Pixel.get current_pixel 0 in

          let other_pixel =
            Image.get_pixel image
              (x + (i - (mask_size / 2)))
              (y + (j - (mask_size / 2)))
          in
          let other_data =
            float_of_int convolution_array.(i).(j) *. Pixel.get other_pixel 0
          in
          Image.set_pixel new_image x y
            (Pixel.v gray
               [ current_data +. (1. /. float_of_int sum *. other_data) ])
        with _ -> ()
      done
    done;

    let current_pixel = Image.get_pixel new_image x y in
    let current_data = Pixel.get current_pixel 0 in

    Image.set_pixel rgb_image x y
      (Pixel.v rgb [ current_data; current_data; current_data ])
  in
  Image.for_each_pixel (fun x y _ -> apply_convolution x y) image;
  rgb_image

let noise_reduction ~image ~mask_size =
  let new_image = Image.copy image in
  let sum, convolution_array = gaussian_mask mask_size in
  let apply_convolution x y =
    Image.set_pixel new_image x y (Pixel.v rgb [ 0.; 0.; 0. ]);
    for i = 0 to mask_size - 1 do
      for j = 0 to mask_size - 1 do
        try
          let current_pixel = Image.get_pixel image x y in
          let current_data =
            [
              Pixel.get current_pixel 0;
              Pixel.get current_pixel 1;
              Pixel.get current_pixel 2;
            ]
          in
          let other_pixel =
            Image.get_pixel image
              (x + (i - (mask_size / 2)))
              (y + (j - (mask_size / 2)))
          in
          let other_data =
            float_of_int convolution_array.(i).(j)
            *| [
                 Pixel.get other_pixel 0;
                 Pixel.get other_pixel 1;
                 Pixel.get other_pixel 2;
               ]
          in
          Image.set_pixel new_image x y
            (Pixel.v rgb
               (current_data |+| 1. /. float_of_int sum *| other_data))
        with _ -> ()
      done
    done
  in
  Image.for_each_pixel (fun x y _ -> apply_convolution x y) image;
  new_image

(** Prend en entrée une image au format HSV et renvoie une carte de ses gradients en fonction de la value *)
let gradient_map ?(gradient_floor = 0.) (image : (_, _, Color.hsv) Image.t) =
  let ty = Image.ty image in
  let heigth, width, _ = Image.shape image in
  let gradient_image = Image.v ty gray heigth width in
  let gradient_x x y =
    let left_value =
      try Pixel.get (Image.get_pixel image (x - 1) y) 2 with _ -> 0.
    in
    let right_value =
      try Pixel.get (Image.get_pixel image (x + 1) y) 2 with _ -> 0.
    in
    right_value -. left_value
  in
  let gradient_y x y =
    let up_value =
      try Pixel.get (Image.get_pixel image x (y + 1)) 2 with _ -> 0.
    in
    let down_value =
      try Pixel.get (Image.get_pixel image x (y - 1)) 2 with _ -> 0.
    in
    up_value -. down_value
  in
  let gradient_evalutation x y =
    let grad_x = gradient_x x y in
    let grad_y = gradient_y x y in
    let gradient_tot = sqrt ((grad_x *. grad_x) +. (grad_y *. grad_y)) in
    Image.set_pixel gradient_image x y
      (Pixel.v Color.gray
         [ (if gradient_tot >= gradient_floor then gradient_tot else 0.) ])
  in
  Image.for_each_pixel (fun x y _ -> gradient_evalutation x y) image;
  Image.convert ty rgb gradient_image