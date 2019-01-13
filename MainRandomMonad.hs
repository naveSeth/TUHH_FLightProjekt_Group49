import Network.MateLight
import Network.MateLight.ListFrame

import Data.Maybe
import qualified Network.Socket as Sock
import Control.Monad.Trans
import System.Random
import Control.Monad
import Data.Word

eventTest :: (Int, Int) -> [EventT] -> MateMonad ListFrame s IO ListFrame
eventTest dim events = liftIO frame >>= return . ListFrame
  where
  line = replicateM (fst dim) randomPixel
  frame = replicateM (snd dim) line
  randomPixel = do
    r <- randomIO :: IO Word8
    g <- randomIO :: IO Word8
    b <- randomIO :: IO Word8
    return $ Pixel r g b

main :: IO ()
main = let dim = (30,12) in Sock.withSocketsDo $ runMateM (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) True []) (eventTest dim) undefined
