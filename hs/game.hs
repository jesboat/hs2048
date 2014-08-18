{-# LANGUAGE
        RecordWildCards, NamedFieldPuns,
        GADTs,
        ImplicitParams,
        RankNTypes, ScopedTypeVariables
  #-}

import Prelude hiding (Right, Left, any)
import Control.Monad
import Control.Monad.State.Lazy
import Data.Function (on)
import Data.Foldable (any)
import Data.List hiding (any)
import Data.Maybe
import qualified Numeric.Probability.Distribution as Pr
import System.Random
import Utils
import Data.Char
import System.IO
import Data.IORef


gameSize = 4
initialTiles = 2
gameIndexes = [0 .. gameSize - 1]


type GameM a = State Game a


data Position = Position {
        x :: Int,
        y :: Int
    }
    deriving (Eq, Ord)
instance Show Position where
    show Position{..} = show (x,y)

data Tile = Tile {
        position :: Position,
        value :: Int,
        wasMerged :: Bool
    }
    deriving (Eq, Ord)
instance Show Tile where
    show Tile{..} = (show value) ++ "@" ++ (show position)
                    ++ if wasMerged then "[M]" else ""

data Grid = Grid {
        cells :: [(Position, Maybe Tile)]
    }
    deriving (Eq, Ord)
instance Show Grid where
    show Grid{..} = showListNice (catMaybes $ map snd $ cells)

grid'new :: Grid
grid'new = Grid cells
    where
        cells = [(Position x y, Nothing) | x <- gameIndexes, y <- gameIndexes]

grid'availableCells :: Grid -> [Position]
grid'availableCells Grid{..} = map fst $ filter (isNothing . snd) cells

grid'cellsAvailable :: Grid -> Bool
grid'cellsAvailable Grid{..} = any (isNothing . snd) cells

grid'randomAvailableCell :: Grid -> Maybe (RandomM Position)
grid'randomAvailableCell grid =
    case grid'availableCells grid of
        [] -> Nothing
        cells -> Just (Pr.uniform cells)

grid'get :: Position -> Grid -> Maybe Tile
grid'get wantPosn Grid{cells} =
    case filter (\(pos, maybeTile) -> pos == wantPosn) cells of
        [(_, maybeTile)] -> maybeTile
        _ -> error (showem "Position " wantPosn " missing from grid " cells)

grid'set :: Position -> Maybe Tile -> Grid -> Grid
grid'set wantPosn newTile oldgrid@Grid{cells} =
    if withinBounds wantPosn
    then
        Grid{cells = map (\(pos, maybeTile) ->
                            if pos == wantPosn
                            then (pos, newTile)
                            else (pos, maybeTile))
                         cells}
    else error (showem "Cannot set invalid position" wantPosn)

grid'put tile@Tile{position} grid =
    grid'set position (Just tile) grid

grid'clear position grid =
    grid'set position Nothing grid

withinBounds Position{..} =
    x >= 0 && x < gameSize && y >= 0 && y < gameSize


data Game = Game {
        grid :: Grid,
        score :: Int
    }
    deriving (Show, Eq, Ord)

game'new :: RandomM Game
game'new =
    let g0 = Game { grid = grid'new, score = 0 }
    in iterateIM initialTiles game'addRandomTile g0

game'addRandomTile :: Game -> RandomM Game
game'addRandomTile Game{..} =
    do
        position <- Pr.uniform $ grid'availableCells grid
        value <- Pr.fromFreqs [(2, 0.9), (4, 0.1)]
        let tile = Tile{position, value, wasMerged = False}
        return Game{grid = grid'put tile grid, ..}

data Direction = Up | Down | Right | Left
    deriving (Eq, Ord, Show, Bounded, Enum)
type Vector = (Int, Int)

dir2vec :: Direction -> Vector
dir2vec Up    = (0, -1)
dir2vec Right = (1, 0)
dir2vec Down  = (0, 1)
dir2vec Left  = (-1, 0)

vecplus :: Position -> Vector -> Position
vecplus Position{x,y} (dx, dy) = Position{x = x+dx, y = y+dy}

buildTraversals :: Vector -> ([Int], [Int])
buildTraversals (dx, dy) = (rx [0 .. gameSize-1], ry [0 .. gameSize-1])
    where
        rx = if dx > 0 then reverse else id
        ry = if dy > 0 then reverse else id

findFarthestPositionGeq :: Position -> Vector -> Game -> (Position, Position)
findFarthestPositionGeq start vec Game{grid} = loop start
    where loop pos =
            let next = vecplus pos vec in
            if withinBounds next && grid'get next grid == Nothing
            then loop next
            else (pos, next)

findFarthestEmpty :: Position -> Vector -> Game -> Maybe Position
findFarthestEmpty start vec Game{grid} = loop start
    where
        loop pos =
            if withinBounds pos
            then
                case loop (vecplus pos vec) of
                    Just somecell -> Just somecell
                    Nothing ->
                        if grid'get pos grid == Nothing
                        then Just pos
                        else Nothing
            else Nothing

findNextNonempty :: Position -> Vector -> Game -> Maybe Position
findNextNonempty start vec Game{grid} = loop start
    where
        loop pos =
            let next = vecplus pos vec in
            if withinBounds next
            then if grid'get next grid /= Nothing
                 then Just next
                 else loop next
            else Nothing

resetGame Game{grid, ..} = Game{grid = resetGrid grid, ..}
resetGrid Grid{cells, ..} = Grid{cells = map resetCell cells, ..}
resetCell (pos, maybeTile) = (pos, fmap resetTile maybeTile)
resetTile Tile{..} = Tile{wasMerged = False, ..}

doMoveOnAll :: Vector -> GameM ()
doMoveOnAll vec = do
    let (traversals_x, traversals_y) = buildTraversals vec
    forM_ traversals_x $ \x -> do
        forM_ traversals_y $ \y -> do
            cell <- gets (grid'get (Position x y) . grid)
            maybemeh (doMoveOnOne vec) cell
    modify resetGame

doMoveOnOne :: Vector -> Tile -> GameM ()
doMoveOnOne vec tile = do
    pos_nextEmpty <- gets (findFarthestEmpty (position tile) vec)
    pos_nextNonEmpty <- gets (findNextNonempty (position tile) vec)
    case pos_nextNonEmpty of
        Just pos_nextNonEmpty -> do
            Just nextTile <- gets (grid'get pos_nextNonEmpty . grid)
            if (value tile) == (value nextTile)
            then mergeTiles tile nextTile
            else maybeMoveTile tile pos_nextEmpty
        Nothing -> do
            maybeMoveTile tile pos_nextEmpty

modifyGrid :: (Grid -> Grid) -> GameM ()
modifyGrid f = modify (\Game{..} -> Game{grid = f grid, ..})

modifyScore :: (Int -> Int) -> GameM ()
modifyScore f = modify (\Game{..} -> Game{score = f score, ..})

mergeTiles :: Tile -> Tile -> GameM ()
mergeTiles src dst = do
    let merged = Tile{position = (position dst),
                      value = 2 * (value dst),
                      wasMerged = True}
    modifyGrid (grid'put merged)
    modifyGrid (grid'clear (position src))
    modifyScore (+ (value merged))

maybeMoveTile :: Tile -> Maybe Position -> GameM ()
maybeMoveTile _ Nothing = meh
maybeMoveTile tile (Just newpos) = do
    let newtile = tile{position = newpos}
    modifyGrid (grid'put newtile)
    modifyGrid (grid'clear (position tile))

doMove :: Direction -> Game -> (Bool, RandomM Game)
doMove direction game1 =
    let game2 = execState (doMoveOnAll (dir2vec direction)) game1 in
    if game1 /= game2
    then (True, game'addRandomTile game2)
    else (False, return game2)

allDirections :: [Direction]
allDirections = [minBound .. maxBound]

grid'matchesAvailable :: Grid -> Bool
grid'matchesAvailable grid = any hasMatch (cells grid)
    where
        hasMatch (pos, Nothing) = False
        hasMatch (pos, Just tile1) =
            (`any` allDirections) $ \dir ->
                let next = vecplus pos (dir2vec dir) in
                if withinBounds next
                then case grid'get next grid of
                        Nothing -> False
                        Just tile2 -> (value tile1) == (value tile2)
                else False

game'movesAvailable :: Game -> Bool
game'movesAvailable Game{grid} =
    grid'cellsAvailable grid || grid'matchesAvailable grid

game'won Game{grid = Grid{cells}} =
    (`any` cells) $ \ (pos, tile) ->
        case tile of
            Nothing -> False
            Just Tile{value} -> (value >= 2048)

render :: Game -> [String]
render Game{grid, score} = rows ++ ["Score: " ++ show score]
    where
        rows = [endrow] ++ (intersperse midrow $ gameRows) ++ [endrow]

        gameRows = map gameRow gameIndexes
        gameRow y = mkrow '|' ' ' $ map (\x -> cell x y) gameIndexes
        cell x y = case grid'get (Position x y) grid of
                       Just Tile{value} -> show value
                       Nothing -> ""

        endrow = mkrow '+' '-' $ map (const "") gameIndexes

        midrow = mkrow '+' '-' $ map (const "") gameIndexes

        mkrow sep padding cells =
            let seps = [sep] in concat $
            [seps] ++ (intersperse seps $ map (padWith padding) cells) ++ [seps]

        padWith join str =
            [join] ++ str ++ (take (len - length str) $ repeat join) ++ [join]
        len = maximum $ map (length . show . value) $ catMaybes $ map snd $ cells grid

data Command = Move Direction | Quit | BadCommand
    deriving (Eq, Show)

letterToMove :: Char -> Maybe Command
letterToMove c =
    case c of
        'a'           -> return $ Move Left
        's'           -> return $ Move Down
        'd'           -> return $ Move Right
        'w'           -> return $ Move Up
        'q'           -> return $ Quit
        _ | isSpace c -> Nothing
          | otherwise -> return $ BadCommand

getMoveStdin :: IO Command
getMoveStdin = do
    eof <- isEOF
    if eof
    then return Quit 
    else do
            c <- getChar
            case letterToMove c of
                Just command -> return command
                Nothing      -> getMoveStdin

data RandomPicker where
    RandomPicker :: (forall a. (Ord a, Show a) => RandomM a -> IO a) -> RandomPicker

data MoveGetter where
    MoveGetter :: IO Command -> MoveGetter

pick :: (?thePicker :: RandomPicker, Ord a, Show a) => RandomM a -> IO a
pick dist = let (RandomPicker picker) = ?thePicker in picker dist

getMove :: (?theMoveGetter :: MoveGetter) => IO Command
getMove = let (MoveGetter g) = ?theMoveGetter in g

playGame :: (?thePicker :: RandomPicker,
             ?theMoveGetter :: MoveGetter) =>
            Game -> IO Game
playGame = loop
    where
        loop :: Game -> IO Game
        loop game = do
            putStrLn ""
            mapM_ putStrLn $ render game
            if game'movesAvailable game
            then do
                    putStr "> "
                    m <- getMove
                    case m of
                        Quit ->
                            return game
                        Move direction -> do
                            let (moved, newgamedist) = doMove direction game
                            newgame <- pick newgamedist
                            loop newgame
                        BadCommand ->
                            loop game
            else do
                    putStrLn "Game over."
                    return game

play :: (?thePicker :: RandomPicker,
         ?theMoveGetter :: MoveGetter) => IO Game
play =
    do
        game0 <- pick game'new
        playGame game0

withRandom :: ((?thePicker :: RandomPicker) => r)
            -> (forall a. (Ord a, Show a) => RandomM a -> IO a)
            -> r
thunk `withRandom` rand = (let ?thePicker = RandomPicker rand in thunk)

withMoveGetter :: ((?theMoveGetter :: MoveGetter) => r)
                -> IO Command
                -> r
thunk `withMoveGetter` mover = (let ?theMoveGetter = MoveGetter mover in thunk)

main :: IO ()
main = (do { play; return () }) `withRandom` pickRandom `withMoveGetter` getMoveStdin




searchLoopB :: Maybe (Int, Game) -> [Game] -> [[Game]] -> Maybe (Int, Game)
searchLoopB bestSoFar queueThis queueNext =
    case queueThis of
        [] -> case queueNext of
                [] -> bestSoFar
                _ -> searchLoopB bestSoFar (concat queueNext) []
        (g:gs) ->
            if maybe False (\(s, _) -> score g > s) bestSoFar
            then searchLoopB bestSoFar gs queueNext
            else if game'movesAvailable g
                 then searchLoopB bestSoFar gs ((gameSuccs g) : queueNext)
                 else searchLoopB (Just (score g, g)) gs queueNext

searchLoopD :: Maybe (Int, Game) -> [Game] -> [(Int, Game)]
searchLoopD bestSoFar queue =
    case queue of
        [] -> maybeToList bestSoFar
        (g:gs) ->
            if maybe False (\(s, _) -> score g >= s) bestSoFar
            then searchLoopD bestSoFar gs
            else if game'movesAvailable g
                 then searchLoopD bestSoFar ((gameSuccs g) ++ gs)
                 else (score g, g) : (searchLoopD (Just (score g, g)) gs)

gameSuccs :: Game -> [Game]
gameSuccs g = concat $ mapMaybe moveIn allDirections
    where moveIn dir = case doMove dir g of
                        (False, _) -> Nothing
                        (True, dist) -> Just $ Pr.extract dist


moveGetterFromList :: String -> IO (IO Command)
moveGetterFromList moves0 =
    do
        mut <- newIORef moves0
        let getter = do
                        moves <- readIORef mut
                        case moves of
                            (m:rest) -> do
                                            writeIORef mut rest
                                            let (Just mov) = letterToMove m
                                            return mov
                            [] -> fail "Out of moves"
        return getter

minimalGame =
    do
        (rand :: forall a. (Ord a, Show a) => RandomM a -> IO a)
            <- makePickerFromList randoms
        move <- moveGetterFromList moves
        play `withRandom` rand `withMoveGetter` move
    where
        script = words $ concat $ intersperse " " [
                "261",
                "d 7",
                "a 14",
                "w 24",
                "d 5",
                "a 3",
                "d 16",
                "w 16",
                "d 7",
                "d 12",
                "d 4",
                "w 5",
                "a 1",
                "a 3",
                "a 1"]
        moves = concat $ filter (all isLetter) script
        randoms = map (read :: String -> Int) $ filter (all isDigit) script
