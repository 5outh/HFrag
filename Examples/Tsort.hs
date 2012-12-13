import HFrag
import HFrag.Types
import HFrag.Instances
import HFrag.Algorithms(tsort)

people = Graph
  (map Node ["Ron","April","Ann","Leslie","Andy","Jerry"])
  (map (fmap Node) 
  [Edge "Ron" "April", Edge "Ron" "Andy", Edge "Ron" "Ann",
   Edge "April" "Ann", Edge "April" "Andy", Edge "Leslie" "April",
   Edge "Leslie" "Ron", Edge "Leslie" "Andy", Edge "Ann" "Andy",
   Edge "Ann" "Jerry", Edge "Andy" "Jerry"])

{-
Produces:
["Leslie", "Ron", "April", "Ann", "Andy", "Jerry"]
-}
example = tsort people