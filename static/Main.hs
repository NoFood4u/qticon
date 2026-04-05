module Main (main) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Control.Monad.IO.Class
import Control.Monad (when, unless)
import Data.Aeson
import Data.IORef
import Graphics.UI.Gtk
import System.Environment
import System.FilePath

type Dataset = V.Vector (T.Text, (V.Vector T.Text))
type Json = M.Map T.Text (V.Vector T.Text)

main = do
  __file__ <- getExecutablePath
  json <- LBS.readFile $ takeDirectory __file__ </> "emoticons.json"
  let dataset = decodeDataset (case decode json of
        Nothing -> M.empty
        Just dataset -> dataset
        )
  refDataset <- newIORef dataset
  refFullDataset <- newIORef dataset

  initGUI
  
  font <- fontDescriptionNew
  fontDescriptionSetFamily font "Noto"

  window <- windowNew
  windowSetDefaultSize window 800 600
  windowSetKeepAbove window True
  windowSetPosition window WinPosCenter
  on window objectDestroy mainQuit
  
  vBox <- vBoxNew False 0
  containerAdd window vBox
  
  entry <- entryNew
  boxPackStart vBox entry PackNatural 0

  scroll <- scrolledWindowNew Nothing . Just =<< adjustmentNew 0 0 100 1 1 10
  boxPackStart vBox scroll PackGrow 0
  list <- textViewNew
  textViewSetWrapMode list $ WrapWord
  widgetOverrideFont list $ Just font
  containerAdd scroll list

  on entry keyPressEvent $ do
    key <- eventKeyName
    when (T.length key == 1) $ liftIO $ do
      query <- (++ T.unpack key) <$> entryGetText entry
      updateDataset refDataset refDataset query
      readIORef refDataset >>= (updateList list . datasetGetText . V.take 100000)
    return False
  on entry entryBackspace $ do
    query <- entryGetText entry
    unless (query == "") $ do
      readIORef refFullDataset >>= writeIORef refDataset
      readIORef refDataset >>= (updateList list . datasetGetText . V.take 100000)
  on entry entryDeleteFromCursor $ \_ _ -> do
    query <- entryGetText entry
    unless (query == "") $ do
      updateDataset refFullDataset refDataset $ take (length query - 1) query
      readIORef refDataset >>= (updateList list . datasetGetText . V.take 100000)

  updateList list $ datasetGetText $ V.take 100000 dataset

  widgetShowAll window

  mainGUI

decodeDataset :: Json -> Dataset
decodeDataset json = V.fromList $ zip (M.keys json) (M.elems json)

updateDataset :: IORef Dataset -> IORef Dataset -> String -> IO ()
updateDataset refReadDataset refWriteDataset query = readIORef refReadDataset >>= (writeIORef refWriteDataset . V.filter (incSearch query))
    where
      incSearchTag :: T.Text -> T.Text -> Bool
      incSearchTag subquery text = subquery `T.isPrefixOf` text
      incSearch :: String -> (T.Text, V.Vector T.Text) -> Bool
      incSearch query (text, tags) = all (\subquery -> any (incSearchTag subquery) tags) . T.split (==' ') . T.pack $ query

datasetGetText :: Dataset -> T.Text
datasetGetText = T.unlines . V.toList . V.map fst

updateList :: TextView -> T.Text -> IO ()
updateList list text = do
  buffer <- textViewGetBuffer list
  textBufferSetText buffer text

