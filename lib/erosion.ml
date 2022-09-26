open Bimage

let is_isolated image x y surrounding_pixels =
  let width, height, _ = Image.shape image in
  let center = Image.get_pixel image x y in
  if
    Pixel.get center 0 = 0.
    && Pixel.get center 1 = 0.
    && Pixel.get center 2 = 0.
  then true
  else if x != 0 && x != width - 1 && y != 0 && y != height - 1 then
    List.length
      (List.filter
         (fun pixel ->
           Pixel.get pixel 0 > 0.
           && Pixel.get pixel 1 > 0.
           && Pixel.get pixel 2 > 0.)
         [
           Image.get_pixel image x (y - 1);
           Image.get_pixel image x (y + 1);
           Image.get_pixel image (x + 1) y;
           Image.get_pixel image (x - 1) y;
           Image.get_pixel image (x - 1) (y - 1);
           Image.get_pixel image (x + 1) (y + 1);
           Image.get_pixel image (x + 1) (y - 1);
           Image.get_pixel image (x - 1) (y + 1);
         ])
    < surrounding_pixels
  else true

let is_surrounded image x y surrounding_pixels =
  let width, height, _ = Image.shape image in
  let center = Image.get_pixel image x y in
  if
    Pixel.get center 0 = 1.
    && Pixel.get center 1 = 1.
    && Pixel.get center 2 = 1.
  then true
  else if x != 0 && x != width - 1 && y != 0 && y != height - 1 then
    List.length
      (List.filter
         (fun pixel ->
           Pixel.get pixel 0 > 0.
           && Pixel.get pixel 1 > 0.
           && Pixel.get pixel 2 > 0.)
         [
           Image.get_pixel image x (y - 1);
           Image.get_pixel image x (y + 1);
           Image.get_pixel image (x + 1) y;
           Image.get_pixel image (x - 1) y;
           Image.get_pixel image (x - 1) (y - 1);
           Image.get_pixel image (x + 1) (y + 1);
           Image.get_pixel image (x + 1) (y - 1);
           Image.get_pixel image (x - 1) (y + 1);
         ])
    > surrounding_pixels
  else false

let erosion ~surrounding_pixels ~round image =
  for _ = 1 to round do
    let original = Image.copy image in
    Image.for_each_pixel
      (fun x y _ ->
        Image.set_pixel image x y
          (if is_isolated original x y surrounding_pixels then
           Pixel.v rgb [ 0.; 0.; 0. ]
          else Pixel.v rgb [ 1.; 1.; 1. ]))
      image
  done;
  image

let rev_erosion ~surrounding_pixels ~round image =
  for _ = 1 to round do
    let original = Image.copy image in
    Image.for_each_pixel
      (fun x y _ ->
        Image.set_pixel image x y
          (if is_surrounded original x y surrounding_pixels then
           Pixel.v rgb [ 1.; 1.; 1. ]
          else Pixel.v rgb [ 0.; 0.; 0. ]))
      image
  done;
  image

let filter_noise ~surrounding_pixels ~round image =
  image
  |> erosion ~surrounding_pixels ~round
  |> rev_erosion ~surrounding_pixels ~round
