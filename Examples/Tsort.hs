import HFrag
import HFrag.Types
import HFrag.Instances
import HFrag.Algorithms(tsort)

people = Graph
  (map Node ["Ron","April","Ann","Leslie","Andy","Jerry"])
  [Edge (Node "Ron") (Node "April"), Edge (Node "Ron") (Node "Andy"), Edge (Node "Ron") (Node "Ann"),
   Edge (Node "April") (Node "Ann"), Edge (Node "April") (Node "Andy"), Edge (Node "Leslie") (Node "April"),
   Edge (Node "Leslie") (Node "Ron"), Edge (Node "Leslie") (Node "Andy"), Edge (Node "Ann") (Node "Andy"),
   Edge (Node "Ann") (Node "Jerry"), Edge (Node "Andy") (Node "Jerry")]

{-
Produces:
["Leslie", "Ron", "April", "Ann", "Andy", "Jerry"]
-}
example = tsort people