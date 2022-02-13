module Voxf.TextureAtlas (TextureAtlas, build) where

import Voxf.Prelude
import Voxf.EntityDef
import Data.IntMap.Strict as IntMap
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as MutVector
import qualified Data.Text as Text
import Data.Atlas as Atlas
import Data.Word (Word8)
import Control.Monad
import Control.Monad.ST
import Codec.Picture

data TextureAtlas = TextureAtlas
    { width :: Int
    , height :: Int
    , textureOffsets :: HashMap Text (Int, Int)
    , texture :: Image PixelRGB8
    }

build :: Int -> Int -> [EntityDef Any Any] -> IO TextureAtlas
build atlasWidth atlasHeight entityDefs = do
    images <- forM entityDefs $ \def -> do
        result <- readImage (Text.unpack def.texture)
        case result of
            Left msg -> error msg
            Right dynImage -> do
                return (convertRGB8 dynImage)
    let (offsetsByIndex, atlasImage) = packImages atlasWidth atlasHeight images
    return $
        TextureAtlas
            { width = atlasWidth
            , height = atlasHeight
            , textureOffsets = zip [0..] entityDefs
                & fmap (\(ix, def) -> (def.name, offsetsByIndex ! ix))
                & HashMap.fromList
            , texture = atlasImage
            }

packImages :: Int -> Int -> [Image PixelRGB8] -> (IntMap (Int, Int), Image PixelRGB8)
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
                ( IntMap.fromList $
                    fmap (\(ix, Pt x y) -> (ix, (x, y))) packedCoords
                , Image
                    { imageWidth = atlasWidth
                    , imageHeight = atlasHeight
                    , imageData = frozenDstBuf
                    }
                )
