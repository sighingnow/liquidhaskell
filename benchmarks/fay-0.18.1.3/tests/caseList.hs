import           Prelude

main = putStrLn (case [1,2,3,4,5] of
  [1,2,3,4,6] -> "6!"
  [1,2,4,2,4] -> "a!"
  [1,2,3,4,5] -> "OK."
  _           -> "Broken.")

