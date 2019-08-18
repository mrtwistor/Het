{-# LANGUAGE TemplateHaskell #-}

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Control.Monad.Random
import           Control.Monad.State
import           Data.List                        as L
import           Data.Maybe
import qualified Data.Set                         as DS
import           Graphics.Gloss                   hiding (Blank, Circle, Color,
                                                   color)
import qualified Graphics.Gloss                   as G (Color, color)
import           Graphics.Gloss.Interface.IO.Game hiding (Blank, Circle, Color,
                                                   color)
import           System.Exit
import           System.Random


data Shape = Triangle | Square | Circle deriving (Show, Eq, Enum, Bounded, Ord)
data Pattern = Fill | Hatch | Trace deriving (Show, Eq, Enum, Bounded, Ord)
data Number = I | II | III deriving (Show, Eq, Enum, Bounded, Ord)
data Color = Red | Yellow | Blue deriving (Show, Eq, Enum, Bounded, Ord)
maxIndex :: Int
maxIndex = fromEnum (maxBound :: Shape)

toGColor :: Color -> G.Color
toGColor col = case col of
  Red    -> red
  Blue   -> blue
  Yellow -> yellow

data Card = Card {
    shape   :: Shape
  , color   :: Color
  , pattern :: Pattern
  , number  :: Number
} deriving (Show, Eq, Bounded, Ord)

data Block = Block {
    card     :: Card
  , selected :: Bool
                   } deriving (Eq, Ord, Show)


newBlock :: Card -> Block
newBlock c = Block c False

type Deck = [Card]
type Board = [Block]
data World = World
  {
    _board      :: Board
  , _deck       :: Deck
  , _score      :: Int
  , _lastInputs :: [(Int, Int)]
  }
makeLenses ''World

field :: World -> [[Block]]
field w =
  let board0 = w^.board
      go ac []           = ac
      go ac (x1:x2:x3:t) = go ([x1,x2,x3]:ac) t
  in go [] board0






shuffle :: Ord a => StdGen -> [a] -> [a]
shuffle g = snd . unzip . L.sort . zip (randoms g :: [Integer])

randDraw :: Ord a => DS.Set a -> State StdGen (Maybe (a,DS.Set a))
randDraw set
  | DS.null set = return Nothing
  | otherwise =
    do
      seed <- get
      let n = DS.size set
          (i,newSeed) = randomR (0,n-1) seed
          (set',set'') = DS.splitAt i set
          [left, root, right] = DS.splitRoot set''
          x = DS.elemAt 0 root
          newSet = DS.unions [set', left, right]
      return $ Just (x, newSet)

type GameState = State World


initDeck :: Deck
initDeck = [ Card shape color pattern number
           | shape <- toEnum <$> [0,1,2]
           , color <- toEnum <$> [0,1,2]
           , pattern <- toEnum <$> [0,1,2]
           , number <- toEnum <$> [0,1,2]
           ]



match cards attr =
  len == 1 || len == n
  where len = DS.size . DS.fromList $ attr <$> cards
        n = length cards

matchAll cards =
  all (match cards) [fromEnum.shape,fromEnum.color,fromEnum.pattern,fromEnum.number]

findMatch :: [Card] -> [(Card,Card,Card)]
findMatch cards =
  do
    x<-cards
    y<-cards
    z<-cards
    guard $ x < y && y < z
    guard $ matchAll [x,y,z]
    return (x,y,z)

numSelected :: Board -> Int
numSelected = length . L.filter ((True==).selected)

matchSelected :: Board -> Bool
matchSelected board =
  or $ do
    x<-board
    y<-board
    z<-board
    guard $ selected x && selected y && selected z
    guard $ x < y && y < z
    return $ matchAll [card x,card y,card z]


type Done = Bool

dealHand :: GameState Done
dealHand = do
  deck0 <- use deck
  board0 <- use board
  let l = length board0
      nCards = max 3 (12 - l)
      (newCards, deck1) = splitAt nCards deck0
      newBlocks = newBlock <$> newCards
      board1 = newBlocks ++ board0
      matches = findMatch (card <$> board1)
  board .= board1
  deck .= deck1
  case matches of
    []  -> case deck1 of
            []  -> return True
            _:_ -> dealHand
    _:_ -> return False


removeSelected :: GameState ()
removeSelected = do
  board0 <- use board
  let selection = L.filter selected board0
  board .= board0 L.\\ selection
  return ()

unselect :: Block -> Block
unselect (Block c _) = Block c False

select :: Block -> Block
select (Block c _) = Block c True


trySelected :: GameState Bool
trySelected = do
  board0 <- use board
  let matched = matchSelected board0
  case matched of
    False -> board .= (unselect <$> board0)
    True  -> removeSelected
  return matched

pointToCoords :: Point -> (Int, Int)
pointToCoords (posx, posy) =
  ( head $ mapMaybe (\i -> let posxi = fromIntegral i * fieldGLSideSize - fromIntegral screenWidth  / 2 in if posxi >= posx then Just (i - 1) else Nothing) ([0..]::[Int])
  , head $ mapMaybe (\i -> let posyi = fromIntegral i * fieldGLSideSize - fromIntegral screenHeight / 2 in if posyi >= posy then Just (ysize - i) else Nothing) ([0..]::[Int]))


getBlockAt :: Int -> Int -> World -> Maybe Block
getBlockAt x y w = (w^.board)^?(ix $ x+xsize*y)


handleSelect :: Int -> Int -> World -> World
handleSelect x y' w =
  let y = ysize - y' -1
      w' =  case getBlockAt x y w of
        Nothing             -> w
        Just b | selected b ->
                   w & board.(ix $ x+xsize*y) .~ unselect b
               | otherwise  ->
                   w & board.(ix $ x+xsize*y) .~ select b
  in if numSelected (w'^.board) == 3
     then (flip execState) w' $
          do
            matched <- trySelected
            if matched
              then dealHand
              else return False
     else w'

updateLastInputs :: (Int, Int) -> World -> World
updateLastInputs p w = do
  w & lastInputs .~ (tail $ (w^.lastInputs) ++ [p])



eventHandler :: Event -> World -> IO World
eventHandler (EventKey (MouseButton LeftButton) Down _ (posx, posy)) w
  = let
    clickedCoords@(ccx, ccy) = pointToCoords (posx, posy) in
      do
        putStrLn (show posx ++ ", " ++ show posy)
        let w' = handleSelect ccx ccy . updateLastInputs clickedCoords $ w
        print (w'^.deck)
        return w'
eventHandler (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
eventHandler _ w                                       = return w



onTick :: Float -> World -> IO World
onTick _ w = return $ w


initWorld :: StdGen -> World
initWorld g =
  let deck0 = shuffle g initDeck
  in World [] deck0 0 []




bgColor :: G.Color
bgColor = white

fps :: Int
fps = 60


screenWidth :: Int
screenHeight :: Int
screenWidth = 400
screenHeight = 700

displayMode :: Display
--displayMode = FullScreen (screenWidth, screenHeight)
displayMode = InWindow "Set" (screenWidth, screenHeight) (0, 0)

xsize :: Int
ysize :: Int
xsize = 3
ysize = 6

fieldGLSideSize :: Float
fieldGLSideSize = let
  sx = fromIntegral screenWidth / fromIntegral xsize
  sy = fromIntegral screenHeight / fromIntegral ysize in
  min sx sy

upscale :: Picture -> Picture
upscale =
  Translate (fromRational (- fromIntegral screenWidth / 2)) (fromRational (- fromIntegral screenHeight / 2)) . Scale fieldGLSideSize fieldGLSideSize



outlineColor :: G.Color
outlineColor = makeColor 0 0 0 0.35


makePolygonFilled :: G.Color -> [(Float, Float)] -> Picture
makePolygonFilled col pts = let
  mainP = Polygon pts
  ptsClosed = pts ++ [head pts]
  in Pictures $ [
  G.color col mainP,
  G.color outlineColor $ line ptsClosed
  ]


thickLineSeg :: Float -> (Float,Float) -> (Float,Float) -> [(Float,Float)]
thickLineSeg thickness (x1,x2) (y1,y2) =
  let d = sqrt ((y1-x1)**2 + (y2-x2)**2)
      n1 = (y2-x2)/d
      n2 = (x1-y1)/d
      p1 = x1 - n1*thickness
      p2 = x2 - n2*thickness
      q1 = x1 + n1*thickness
      q2 = x2 + n2*thickness
      r1 = y1 + n1*thickness
      r2 = y2 + n2*thickness
      s1 = y1 - n1*thickness
      s2 = y2 - n2*thickness
  in [(p1,p2),(q1,q2),(r1,r2),(s1,s2)]


mythickness = 0.025

makePolygonTraced :: G.Color -> [(Float, Float)] -> Picture
makePolygonTraced col pts =
  let
    thickSegs = zipWith (thickLineSeg mythickness) pts (tail pts ++ pts)
  in Pictures (G.color (dark col) <$> Polygon <$> thickSegs)


makePolygonHatched :: G.Color -> [(Float, Float)] -> Picture
makePolygonHatched col pts =
  let
    thickSegs = zipWith (thickLineSeg mythickness) pts (tail pts ++ pts)
    mainP = Polygon pts
    ptsClosed = pts ++ [head pts]
  in Pictures $ (G.color (light col) mainP) : (G.color (dark col) <$> Polygon <$> thickSegs)





trianglePoints = [(0,0), (0.25,0), (0.125,0.25)]
squarePoints = [(0,0),(0.25,0),(0.25,0.25),(0,0.25)]
circlePoints = [(0.125+0.125*cos(t),0.125+0.125*sin(t)) | t <- [0, 0.3.. 2*pi]]


drawSelected :: Picture
drawSelected = Pictures [G.color (dark white) $ Polygon [(0,0),(1,0),(1,1),(0,1)]]

drawUnselected :: Picture
drawUnselected =
  let ptsClosed = [(0,0),(1,0),(1,1),(0,1),(0,0)] in
    Pictures [line ptsClosed]

drawBlock :: Block -> Picture
drawBlock b =
  let boundingBox = case selected b of
        False -> drawUnselected
        True  -> drawSelected
  in Pictures [boundingBox, drawCard $ card b]

drawCard :: Card -> Picture
drawCard (Card sh col pat num) =
  let pts =
          case sh of
            Circle   -> circlePoints
            Triangle -> trianglePoints
            Square   -> squarePoints
      patternHandler =
          case pat of
            Fill  -> makePolygonFilled (toGColor col)
            Hatch -> makePolygonHatched (toGColor col)
            Trace -> makePolygonTraced (toGColor col)
  in case num of
    I -> Pictures [Translate (0.5) (0.5) (patternHandler pts)]
    II -> Pictures [ Translate (0.25) 0 (patternHandler pts), Translate (0.5) (0.25) (patternHandler pts)]
    III -> Pictures [ Translate 0 0 (patternHandler pts), Translate (0.25) (0.25) $ patternHandler pts, Translate (0.5) (0.5) (patternHandler pts)]


drawBoard :: Board -> Picture
drawBoard b = go [] 0 b
  where go ac _ []           = Pictures ac
        go ac n (x0:x1:x2:t) =
          go ( Translate 0 n (drawBlock x0)
               : Translate 1 n (drawBlock x1)
               : Translate 2 n (drawBlock x2)
               : ac
             ) (n+1) t



drawWorld :: World -> IO Picture
drawWorld w = do
  return $ Pictures [upscale $ drawBoard $ (w^.board)]


main :: IO ()
main = do
  g <- getStdGen
  let world = execState dealHand $ initWorld g
  playIO displayMode bgColor fps world drawWorld eventHandler onTick
  return()
