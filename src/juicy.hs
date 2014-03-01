-- Krisztian Pinter, 2014
import Codec.Picture
import Codec.Picture.Types
import Control.Monad
import Control.Monad.ST.Safe as ST
import Data.Vector.Storable as VS (length)
import Data.Vector.Storable.Mutable (new)
import Data.Time.Clock

type PixelFun = Image PixelRGB8 -> Int -> Int -> PixelRGB8
type ImageFun = Image PixelRGB8 -> Image PixelRGB8

mapImage :: PixelFun -> Image PixelRGB8 -> Image PixelRGB8
mapImage f img@(Image w h idat) = ST.runST $ do
  idat' <- new $ VS.length idat
  let img' = MutableImage w h idat'
  forM_ [(x,y)| x <- [0..w-1], y <- [0..h-1]]
    (\(x, y) -> writePixel img' x y $ f img x y)
  freezeImage img'

compFun :: [PixelFun] -> ImageFun
compFun ls = foldl (.) (mapImage id_fun) $ map mapImage ls

runFun :: ImageFun -> String -> String -> IO ()
runFun f in_fname out_fname = do
  img <- readImage in_fname
  case img of
    Right (ImageRGB8  img') -> writePng out_fname $ f img'
    Left ex -> putStr ex

main = do
  t1 <- getCurrentTime
  putStrLn "running example 1"
  runFun (compFun [invert_fun                        ]) "spj.png" "spj_0.png"
  putStrLn "running example 2"
  runFun (compFun [            warp_fun 50 (280, 220)]) "spj.png" "spj_1.png"
  putStrLn "running example 3"
  runFun (compFun [invert_fun, warp_fun 50 (280, 220)]) "spj.png" "spj_2.png"
  t2 <- getCurrentTime
  putStrLn $ "done in " ++ show (diffUTCTime t2 t1)

---------------------------------------------------------

id_fun :: PixelFun
id_fun img x y = pixelAt img x y

invert_fun :: PixelFun
invert_fun img x y = PixelRGB8 (255-r) (255-g) (255-b)
  where
    PixelRGB8 r g b = pixelAt img x y

warp_fun :: Float -> (Float, Float) -> PixelFun
warp_fun p (cx, cy) img@(Image w h _) x y = pixelAt img x' y'
  where
    (fx, fy) = (fromIntegral x, fromIntegral y)
    (dx, dy) = (cx - fx,  cy - fy)
    len = sqrt(dx**2 + dy**2)
    (dxn, dyn) = (dx/len, dy/len)
    weight = sqrt $ len * (abs p)
    (x', y') = (round $ fx + dxn * weight, round $ fy + dyn * weight)