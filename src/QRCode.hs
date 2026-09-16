{-# LANGUAGE RecordWildCards #-}
module QRCode 
( genQRCodeSVG
, toSvg
) where

import qualified Graphics.Svg as Svg
import qualified Codec.QRCode as QR
import Control.Lens 
import Codec.Picture (PixelRGBA8(..))
import Data.Vector.Unboxed ((!?))

genQRCodeSVG :: Int -> Double -> QR.QRImage -> Svg.Document
genQRCodeSVG borderW scaleFactor (QR.QRImage {..}) =
    let s = Svg.Px (scaleFactor * (fromIntegral borderW * 2 + fromIntegral qrImageSize))
        black = Svg.ColorRef (PixelRGBA8 0 0 153 0)
        attrs = 
            Svg.defaultSvg 
                & Svg.fillColor .~ pure black
        tree = 
            [ Svg.RectangleTree $
                Svg.defaultSvg 
                    & Svg.drawAttr .~ attrs 
                    & Svg.rectWidth .~ Svg.Px scaleFactor
                    & Svg.rectHeight .~ Svg.Px scaleFactor
                    & Svg.rectUpperLeftCorner .~ 
                        ( Svg.Px (scaleFactor * fromIntegral (borderW + x))
                        , Svg.Px (scaleFactor * fromIntegral (borderW + y))
                        )
            | x <- [0..qrImageSize - 1] 
            , y <- [0..qrImageSize - 1] 
            , qrImageData !? (x + y * qrImageSize) == Just True
            ]
     in Svg.Document Nothing (Just s) (Just s) tree mempty mempty mempty mempty

errorDocument :: Svg.Document
errorDocument = 
    let w = Svg.Px 100
        h = Svg.Px 100 
     in Svg.Document Nothing (Just w) (Just h) [Svg.defaultSvg] mempty mempty mempty mempty

toSvg :: QR.ToText a =>  Int -> Double -> QR.QRCodeOptions -> QR.TextEncoding -> a -> Svg.Document
toSvg borderW scaleFactor options encoding content = 
    case QR.encode options encoding content of
        Nothing -> errorDocument
        Just qrImage -> genQRCodeSVG borderW scaleFactor qrImage

