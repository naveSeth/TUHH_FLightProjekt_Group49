import qualified Network.Socket as Sock
import Data.Maybe
import Control.Monad.State
import Control.Monad

import Network.MateLight.Simple

eventTest :: [Int] -> [Event String] -> () -> (ListFrame, ())
eventTest ints _ () = (frame, ())
  where
  getPixel :: State [Int] Pixel
  getPixel = state $ \ints -> (let [r, g, b] = map fromIntegral $ take 3 ints in Pixel r g b, drop 3 ints)
  getRow :: State [Int] [Pixel]
  getRow = replicateM (fst dim) getPixel
  getFrame :: State [Int] ListFrame
  getFrame = ListFrame `fmap` replicateM (snd dim) getRow
  frame = evalState getFrame ints

eventTestNoMonad :: [Int] -> [Event String] -> () -> (ListFrame, ())
eventTestNoMonad ints _ () = (frame, ())
  where
  thread i f ints = foldr (\_ (res, is) -> let (p, is') = f is in (p : res, is')) ([], ints) [1..i]
  getPixel :: [Int] -> (Pixel, [Int])
  getPixel ints = (let [r, g, b] = map fromIntegral $ take 3 ints in Pixel r g b, drop 3 ints)
  getRow :: [Int] -> ([Pixel], [Int])
  getRow = thread (fst dim) getPixel
  getFrame :: [Int] -> (ListFrame, [Int])
  getFrame ints = let (a, b) = thread (snd dim) getRow ints in (ListFrame a, b)
  frame = fst $ getFrame ints

dim :: (Int, Int)
dim = (30, 12)

main :: IO ()
main = Sock.withSocketsDo $ runMateRandom (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) False []) eventTest ()
