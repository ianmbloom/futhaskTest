import "pixel"
import "hoas"
import "random"



let randomImage (h:i64) (w:i64) (rng:Rng):(Rng, [h][w]Pixel) =
    let (rng, field) = randomField_3d h w 4 rng
    in  (rng, map_3d f32.f64 field)

let composite (bias:Channel)
              (r:Channel)
              (v:Channel):
              Channel =
    ((1 - bias) * v) + (bias * r)

entry addNoise [h][w] (factor:f32)
                      (seed:i32)
                      (source:[h][w]Pixel)
                      :([h][w]Pixel) =
    let rng = mk_rng seed
    let (_rng, noiseField) = randomImage h w rng
    in  (map2_3d (composite factor) noiseField source)
