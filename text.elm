import Window

main = lift draw Window.dimensions

draw (x, y) = container x y middle <| boxText 15 50 75 ["Hello", "World"]


boxText margin padding tsize ln =
  let
    hspcr = (spacer 0 margin)
    vspcr = (spacer margin 0)
    pad = padding + tsize

    blockLines ll = foldr (\l1 l2 -> l1 `above` hspcr `above` l2) (head ll) (tail ll)

    blockLine s =
      let
        charCont c = (container pad pad middle <| centered . Text.height (toFloat tsize) <| toText c)
        charBox c = opacity 0.8 <| color red <| charCont c

        charlst = String.split "" (String.reverse s)
        boxlst = map charBox charlst
      in foldl (\l1 l2 -> l1 `beside` vspcr `beside` l2) (head boxlst) (tail boxlst)

  in blockLines (map blockLine (reverse ln))
