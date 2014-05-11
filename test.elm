import Window
import Mouse

main = draw <~ Window.dimensions ~ circleSize

draw (x, y) cs = collage x y [filled (rgba 0 0 0 1) (circle (0.5 * cs))]

circleSize = smooth 0.9 (fps 25) velocity

velocity =
  let
    vecSub ((x1, y1), (x2, y2)) = (x1 - x2, y1 - y2)
    vecLen (x, y) = sqrt (toFloat x^2 + toFloat y^2)

    delayPos = sampleOn (every (50 * millisecond)) Mouse.position
    moveVec = foldp (\n (a, b) -> (b, n)) ((0, 0), (0, 0)) delayPos
  in vecLen <~ (vecSub <~ moveVec)

smooth factor interval sig = 
  let
    tick = sampleOn interval sig
    scale sn s = ((1.0 - factor) * sn) + (factor * s)
    outp = foldp scale 0.0 tick
  in sampleOn interval outp
