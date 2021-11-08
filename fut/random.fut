import "cpprandom"
import "hoas"

module dist = uniform_real_distribution f64 minstd_rand

type Rng = minstd_rand.rng
let mk_rng (seed:i32) = minstd_rand.rng_from_seed [seed]

let uniformField (rng:Rng) = dist.rand (0,1) rng

let randomField_1d (w:i64)
                   (rng:Rng):
                   (Rng, [w]f64) =
    let rngs = minstd_rand.split_rng w rng
    let (rngs', rs) = unzip (map uniformField rngs)
    let rng' = minstd_rand.join_rng rngs'
    in  (rng', rs)

let randomField_2d (h:i64) (w:i64)
                   (rng:Rng):
                   (Rng, [h][w]f64) =
    let (rng', rs) = randomField_1d (h*w) rng
    in  (rng', unflatten h w rs)

let randomField_3d (h:i64) (w:i64) (d:i64)
                   (rng:Rng):
                   (Rng, [h][w][d]f64) =
    let (rng', rs) = randomField_1d (h*w*d) rng
    in  (rng', unflatten_3d h w d rs)
