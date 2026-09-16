module AddSlideDiv 
(addSlideDiv
) where

import Text.Pandoc
import Data.List (partition, zipWith, foldl')
import Data.List.Split (splitOn)
import qualified Data.Text as T
import Text.Pandoc.Shared (uniqueIdent, inlineListToIdentifier)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

isNote :: Block -> Bool
isNote (Div (_, classes, _) _) | "notes" `elem` classes = True
isNote _ = False

optionalHeader :: Set T.Text -> [Block] -> Maybe T.Text
optionalHeader usedIdentifiers ((Header _ _ inlines):_) = Just (uniqueIdent pandocExtensions inlines usedIdentifiers)
optionalHeader _ _ = Nothing

addSlideDiv :: Pandoc -> Pandoc
addSlideDiv (Pandoc meta blocks) = 
    let blocks' = splitOn [HorizontalRule] blocks
        contentAttr = ("", ["slide"], [])
        noteAttr = ("", ["noteGroup"], [])
        makeGroup (i, usedIdentifiers, blocksSoFar) bs = 
            let (notes, regular) = partition isNote bs
                defaultSlideIdentifier = "slide-" <> (T.pack . show  $ i)
                slideIdentifier = fromMaybe defaultSlideIdentifier $ optionalHeader usedIdentifiers regular
                slideAttr = (slideIdentifier, ["pair"], [])
                selflink = Link ("", ["selflink"], []) [] ("#" <> slideIdentifier, slideIdentifier)
                newBlock =  Div slideAttr 
                    [ Div contentAttr regular
                    , Div noteAttr notes
                    , Plain [selflink]
                    ]  
            in (i+1, usedIdentifiers <> Set.singleton slideIdentifier, newBlock : blocksSoFar)
        (_, _, blocks'') = foldl' makeGroup (0, mempty, []) blocks'
        in Pandoc meta (reverse blocks'')

