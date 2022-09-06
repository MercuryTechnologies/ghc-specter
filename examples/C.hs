module C (test3) where

import A (test)
import B (test2)

test3 :: IO ()
test3 = do
  test
  test2
