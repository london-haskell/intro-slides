module Main (main) where


import Text.Pandoc
import qualified Data.Text.IO as T
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Text.DocTemplates
import AddSlideDiv (addSlideDiv)
import AddQRCode (addQRCode)
import qualified System.FSNotify as FSNotify
import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless)
import Control.Arrow ((&&&), Kleisli (..))
import Options.Applicative
import Text.Pandoc.Highlighting
import System.Directory (createDirectoryIfMissing)
import System.Process (callProcess)
import Data.Function ((&))
parseInput :: Text -> IO Pandoc
parseInput txt =
    let readerOptions = def 
            { readerExtensions = pandocExtensions 
                & disableExtension Ext_auto_identifiers
            , readerStandalone = True
            }
    in runIOorExplode $ 
        readMarkdown readerOptions txt 

printSlides :: Pandoc -> Template Text -> Text -> IO Text
printSlides doc template dzcore = 
    let
        writerOptions = def
            { writerTemplate = Just template
            , writerReferenceLinks = True
            , writerSlideLevel = Just 1
            , writerVariables = Context (M.singleton "dzslides-core" (toVal dzcore))
            }
    in runIOorExplode $ 
            writeDZSlides writerOptions doc

printHTML :: Pandoc -> Template Text -> IO Text
printHTML doc template = let
        writerOptions = def
            { writerTemplate = Just template
            , writerReferenceLinks = True
            , writerHighlightStyle = Just pygments
            , writerVariables = 
               Context (M.singleton "highlighting-css" (toVal $ T.pack $ styleToCss pygments))
            }
    in runIOorExplode $ 
            writeHtml5String writerOptions doc

compileSlides :: Pandoc -> IO ()
compileSlides doc = do
    templateTxt <- T.readFile "template.html" 
    dzcoreTxt <- T.readFile "dz-core.html" 
    template <- either error id <$> compileTemplate "." templateTxt
    T.writeFile "output/slides.html" =<< printSlides doc template dzcoreTxt

compileOverview :: Pandoc -> IO ()
compileOverview doc = do
    templateTxt <- T.readFile "index-template.html" 
    template <- either error id <$> compileTemplate "." templateTxt
    T.writeFile "output/index.html" =<< printHTML doc template

copyFiles :: IO ()
copyFiles = do
    callProcess "cp" ["-R", "assets", "output/assets"]
    callProcess "cp" ["onstage.html", "output/onstage.html"]


rebuild :: IO ()
rebuild = do 
    putStrLn "rebuilding"
    createDirectoryIfMissing True "output/generated"
    _ <- (runKleisli $ Kleisli compileSlides &&& Kleisli (compileOverview . addSlideDiv))
         . addQRCode
         =<< parseInput
         =<< T.readFile "Presentation.md"
    copyFiles
    putStrLn "done"

main :: IO ()
main = do
    once <- execParser $ 
        info (switch (long "once") <**> helper)
            ( fullDesc <> progDesc "do the thing")
    rebuild
    unless once $
        FSNotify.withManager $ \mgr -> do
            FSNotify.watchDir mgr "Presentation.md" (const True) (const rebuild)
            -- sleep until interrupted
            forever $ threadDelay 1000000
    

