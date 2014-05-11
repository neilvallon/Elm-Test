import Mouse

main = lift circleEQ circleSize

circleEQ x = collage 1000 1000 [filled (rgba 0 0 0 1) (circle (0.5 * x))]

circleSize = smooth 0.9 (fps 25) mouseVector

mouseVector = (\(x, y) -> toFloat x + toFloat y) <~ Mouse.position


smooth factor interval sig = 
  let
    tick = sampleOn interval sig
    scale sn s = ((1.0 - factor) * sn) + (factor * s)
    outp = foldp scale 0.0 tick
  in sampleOn interval outp
