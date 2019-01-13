module Main where
import Network.MateLight.Simple

import Data.Maybe
import qualified Network.Socket as Sock

move :: (Int, Int) -> String -> (Int,Int) -> (Int, Int)
move (xdim,ydim) '
