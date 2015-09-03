import Turtle

tuple :: Pattern (Int, Int)
tuple = do
  "("
  x <- decimal
  ","
  y <- decimal
  ")"
  return (x, y)

-- match tuple "(3,4)"
-- why it doesn't compile??!?!!?
