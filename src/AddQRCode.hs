module AddQRCode
( addQRCode
) where 

import Text.Pandoc
import QRCode (toSvg)
import qualified Codec.QRCode as QR
import qualified Graphics.Svg as Svg
import Data.Text (Text)
import Text.Pandoc.Walk (walk)
import qualified Data.Text as T
import Text.XML.Light (ppcTopElement, prettyConfigPP, Attr (..))
import qualified Text.XML.Light as XML

svgToText :: [Text] -> [(Text, Text)] -> Svg.Document -> Text
svgToText classnames attrs =
    let addOneAttr (k, v) = XML.add_attr (Attr (XML.unqual . T.unpack $ k) (T.unpack v))
        addAllAttrs x = foldr addOneAttr x attrs
    in T.pack 
        . ppcTopElement prettyConfigPP 
        . XML.add_attr (Attr (XML.unqual "class") (T.unpack . T.unwords $ classnames))
        . addAllAttrs
        . Svg.xmlOfDocument

addInline :: Inline -> Inline
addInline (Link (_id, classnames, attrs) [Str "QR_CODE_HERE"] (url, _title)) = 
    let qrCodeSvg = toSvg 4 7 (QR.defaultQRCodeOptions QR.Q) QR.Iso8859_1OrUtf8WithECI url
     in RawInline (Format "html") (svgToText classnames attrs qrCodeSvg)
addInline x = x

addQRCode :: Pandoc -> Pandoc
addQRCode = walk addInline