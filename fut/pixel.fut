type Channel = f32
type Pixel = [4]Channel

let addPixel (a:Pixel) (b:Pixel) : Pixel = map2 (+) a b
let dividePixel (b:f32) (a:Pixel) : Pixel = map (/b) a
let pixelToRgba (p:Pixel):(Channel,Channel,Channel,Channel) = (p[0],p[1],p[2],p[3])
