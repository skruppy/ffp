-- "THE BEER-WARE LICENSE" (Revision 42):
-- Albatrouss and <skruppy@onmars.eu> wrote this software. As long as you retain
-- this notice you can do whatever you want with this stuff. If we meet some
-- day, and you think this stuff is worth it, you can buy me a beer in return.
--                                                     -- Albatrouss and Skruppy

module GameGUI.Internal where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.Array
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Display.Label
import Sm

data Command
    = ShowBoard Board
    | ShowGameData String GameData
    | ShowMessage MessageType String


data Gui = Gui {
      guiChannel     :: MVar Command
    , guiWindow      :: Window
    , guiContBoard   :: Table
    , guiContPlayers :: VBox
    , guiLbGameType  :: Label
    , guiLbGameName  :: Label
    , guiLbGameId    :: LinkButton
    }


{-== EXTERNAL ==-}
guiNew :: IO Gui
guiNew = do
    channel <- newEmptyMVar
    
    window <- windowNew
    set window [ windowTitle          := "Waiting for input"
               , containerBorderWidth := 5
               , windowDefaultWidth   := 700
               , windowDefaultHeight  := 500
               ]
    
    contWindow <- hBoxNew False 15
    contBoard  <- tableNew 1 1 True
    contInfo   <- vBoxNew False 6
    
    boxPackStart contWindow contBoard PackGrow 0
    boxPackStart contWindow contInfo PackNatural 0
    
    lbGameType   <- addSimpleInfo contInfo "Game type"
    lbGameName   <- addSimpleInfo contInfo "Game name"
    
    addInfoHeader contInfo "Game ID"
    lbGameId <- linkButtonNewWithLabel "" "-"
    buttonSetAlignment lbGameId (0.0, 0.5)
    set lbGameId [widgetCanDefault := False, widgetCanFocus := False]
    boxPackStart contInfo lbGameId PackNatural 0
    
    addInfoHeader contInfo "Players"
    contPlayers   <- vBoxNew False 6
    boxPackStart contInfo contPlayers PackNatural 0
    
    containerAdd window contWindow
    onDestroy window mainQuit
    
    return Gui {
          guiChannel     = channel
        , guiWindow      = window
        , guiContBoard   = contBoard
        , guiContPlayers = contPlayers
        , guiLbGameType  = lbGameType
        , guiLbGameName  = lbGameName
        , guiLbGameId    = lbGameId
        }


runGui :: Gui -> IO ()
runGui gui = do
    handler <- timeoutAdd (poll gui) 100
    widgetShowAll (guiWindow gui)
    mainGUI
    timeoutRemove handler


updateGameData :: Gui -> String -> GameData -> IO ()
updateGameData (Gui {guiChannel = channel}) gameId gameData = do
    putMVar channel $ ShowGameData gameId gameData


updateBoard :: Gui -> Board -> IO ()
updateBoard (Gui {guiChannel = channel}) board = do
    putMVar channel $ ShowBoard board


showGuiMsg :: Gui -> MessageType -> String -> IO ()
showGuiMsg (Gui {guiChannel = channel}) msgType msg = do
    putMVar channel $ ShowMessage msgType msg


{-== INTERNAL ==-}
poll :: Gui -> IO Bool
poll gui = do
    msg <- tryTakeMVar $ guiChannel gui
    maybe (return ()) (update gui) msg
    return True


update :: Gui -> Command -> IO ()
update gui (ShowGameData gameId (GameData serverMajor serverMinor gameType gameName players)) = do
    -- Game type
    labelSetText (guiLbGameType gui) $ "    " ++ gameType
    
    -- Game name
    labelSetText (guiLbGameName gui) $ "    " ++ gameName
    
    -- Game ID
    buttonSetLabel (guiLbGameId gui) $ "    " ++ gameId
    set (guiLbGameId gui) [linkButtonURI := "http://sysprak.priv.lab.nm.ifi.lmu.de/sysprak/"++gameType++"/"++gameId]
    
    -- Players
    let cont = (guiContPlayers gui)
    odlWidgets <- containerGetChildren cont
    mapM (containerRemove cont) odlWidgets
    mapM_ (addPlayer cont gameId gameType) $ assocs players
    widgetShowAll cont
    
    -- Window title (contains everything™)
    set (guiWindow gui) [windowTitle := "Playing "++gameName++" a game of "++gameType++" ("++gameId++")"]


update gui (ShowBoard board) = do
    old <- tableGetSize cont
    case old of
        (oldX, oldY) | oldX == 1 && oldY == 1 -> do
            tableResize cont newX newY
        
        (oldX, oldY) | oldX /= newX || oldY /= newY -> do
            error $ "Board dimensions have changed over time " ++ (show old)
        
        otherwise -> return ()
    
    odlWidgets <- containerGetChildren cont
    mapM (containerRemove cont) odlWidgets
    
    addLabels     cont (newX-1) (newY-1)
    createButtons cont board
    widgetShowAll cont
    where
        cont = guiContBoard gui
        bnds = snd $ bounds board
        newX = 1 + fst bnds
        newY = 1 + snd bnds


update gui (ShowMessage msgType msg) = do
    window <- messageDialogNew
        Nothing
        []
        msgType
        ButtonsOk
        msg
    
    window `on` response $ \_ -> do
        widgetDestroy  window
    
    widgetShowAll window


{-== BOARD ==-}
addLabels :: Table -> Int -> Int -> IO ()
addLabels table sizeX sizeY = do
    let labelsAlpha = map (:[]) $ take sizeX ['A'..'Z']
    let labelsNum   = map show [1..sizeY]
    
    let alphaIndi   = map (\x -> (sizeX+1  ,1+x)) [1..sizeX]
    let numIndi     = map (\y -> (sizeX+1-y,1  )) [1..sizeY]
    
    nlabels <- createLabels table numIndi labelsNum
    alabels <- createLabels table alphaIndi labelsAlpha
    
    return ()


createLabels :: Table -> [(Int,Int)] -> [String] -> IO ([Label])
createLabels _ [] _ = do return []
createLabels table coord labelStrings = do
    labels <- mapM labelNew (map Just labelStrings)
    let trippel = zip labels coord
    mapM_ (\(l,(c,r)) -> tableAttachDefaults table l (r-1) r (c-1) c) trippel
    return labels


createButtons :: Table -> Board -> IO [Button]
createButtons table field = do
    let fieldIndi = indices field
    mapM (createButton table field) fieldIndi


createButton :: Table -> Board -> (Int, Int) -> IO Button
createButton table field (row,col) = do
    -- Get display string
    let s = if ((field ! (row,col)) == "W") then "⛀" else if ((field ! (row,col)) == "B") then "⛂" else " "
    
    -- Calculate actual display position
    let ((_,_),(_, sizeY)) = bounds field
    let row' = row + 1
    let col' = sizeY - col + 1
    
    button <- buttonNewWithLabel s
    tableAttachDefaults table button  (row'-1) (row')  (col'-1) (col')
    return button


{-== GAME DATA ==-}
addInfoHeader :: VBox -> String -> IO ()
addInfoHeader container description = do
    label <- labelNew $ Just ""
    labelSetMarkup label $ "<b>"++description++"</b>"
    miscSetAlignment label 0.0 0.5
    boxPackStart container label PackNatural 0


addSimpleInfo :: VBox -> String -> IO Label
addSimpleInfo container description = do
    addInfoHeader container description
    
    lbValue <- labelNew $ Just "    -"
    miscSetAlignment lbValue 0.0 0.5
    boxPackStart container lbValue PackNatural 0
    
    return lbValue


addPlayer :: VBox -> String -> String -> (Int, PlayerItem) -> IO ()
addPlayer container gameId gameType (i, player) = do
    label <- linkButtonNewWithLabel url $ prefix++(playerName player)++suffix
    buttonSetAlignment label (0.0, 0.5)
    set label [widgetCanDefault := False, widgetCanFocus := False]
    boxPackStart container label PackNatural 0
    
    where
        url = "http://sysprak.priv.lab.nm.ifi.lmu.de/sysprak/"++gameType++"/"++gameId++"#"++(show i)
        prefix = (show i) ++ ": "
        suffix = if      itsMe player   then " (You)"
                 else if isReady player then " (Ready)"
                                        else ""