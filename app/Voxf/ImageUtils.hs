module Voxf.ImageUtils where

import Data.IntMap.Strict as IntMap
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as MutVector
import Data.Atlas as Atlas
import Data.Word (Word8)
import Control.Monad
import Control.Monad.ST
import Codec.Picture

packImages :: Int -> Int -> [Image PixelRGB8] -> Image PixelRGB8
packImages atlasWidth atlasHeight images = packedImage
    where
        indexedSizes = fmap (\(ix, Image w h _) -> (ix, Pt w h)) $ zip [0..] images
        packedCoords = runST $ do
            atlas <- Atlas.create atlasWidth atlasHeight
            result <- Atlas.pack atlas snd
                (\mpos (ix, _) -> maybe (ix, Pt (-1) (-1)) (\p -> (ix, p)) mpos)
                (\pos  (ix, _) -> (ix, pos))
                indexedSizes
            return $ either id id result
        packedImage = runST $ do
            dstBuf <- MutVector.unsafeNew @_ @Word8 (atlasWidth * atlasHeight)
            let imageByIndex = IntMap.fromList $ zip [0..] images
            forM_ packedCoords $ \(ix, Pt x0 y0) -> do
                let (Image w h srcBuf) = imageByIndex ! ix
                forM_ [0 .. h] $ \row -> do
                    let dst = MutVector.slice ((y0 + row) * atlasWidth + x0) w dstBuf
                    let src = Vector.slice (row * w) w srcBuf
                    Vector.unsafeCopy dst src
            frozenDstBuf <- Vector.unsafeFreeze dstBuf
            return $
                Image
                    { imageWidth = atlasWidth
                    , imageHeight = atlasHeight
                    , imageData = frozenDstBuf
                    }
