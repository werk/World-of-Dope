import Data.List (intercalate, permutations)
import System.Random
import Control.Monad.State
import Data.Array
import Data.Maybe (fromMaybe)

data Tile = Tile {
    north :: Bool,
    south :: Bool,
    east :: Bool,
    west :: Bool
    } deriving (Eq, Show)

northDirection = (0, 1)
southDirection = (0, -1)
eastDirection = (1, 0)
westDirection = (-1, 0)

directionsOf (Tile n s e w) = 
    filter (const n) [northDirection] ++
    filter (const s) [southDirection] ++
    filter (const e) [eastDirection] ++
    filter (const w) [westDirection]

(x1, y1) .+. (x2, y2) = (x1 + x2, y1 + y2)
(x, y) .* f = (x * f, y * f)

emptyTile:roadTiles = [Tile n s e w |
    n <- [False, True],
    s <- [False, True],
    e <- [False, True],
    w <- [False, True]
    ]

data City = City {
    tiles :: Array (Int, Int) (Maybe Tile),
    generator :: StdGen
    }

generate :: (StdGen -> (v, StdGen)) -> State City v
generate r = do
    state <- get
    let (v, g) = r (generator state)
    modify (\state -> state {generator = g})
    return v

branch n s e w = do
    let randomize d = liftM (flip fromMaybe d) (liftM2 (||) (generate random) (generate random))
    n' <- randomize n
    s' <- randomize s
    e' <- randomize e
    w' <- randomize w
    return (Tile n' s' e' w')

branchAt position = do
    n <- tileAt (position .+. northDirection)
    s <- tileAt (position .+. southDirection)
    e <- tileAt (position .+. eastDirection)
    w <- tileAt (position .+. westDirection)
    branch (fmap south n) (fmap north s) (fmap west e) (fmap east w)

emptyMap :: Int -> Int -> Array (Int, Int) (Maybe Tile)
emptyMap width height = array ((0, 0), (width - 1, height - 1)) [
    ((x, y), 
        if x == 0 || y == 0 || x == width - 1 || y == height - 1 
        then Just (Tile False False False False) 
        else Nothing) |
    x <- [0 .. width - 1],
    y <- [0 .. height - 1]
    ]

tileAt position = do
    state <- get
    return (tiles state ! position)

setTileAt position tile = modify (\state -> state {tiles = (tiles state) // [(position, tile)]})

fillMap position = do
    existingTile <- tileAt position
    when (existingTile == Nothing) $ do
        tile <- branchAt position
        setTileAt position (Just tile)
        forM_ (directionsOf tile) $ \direction -> do
            let position' = position .+. direction
            fillMap position'

generateMap width height = do
    g <- newStdGen
    let tiles = emptyMap width height
    let City tiles' _ = flip execState (City tiles g) $ fillMap (width `div` 2, height `div` 2)
    return tiles'

newtype CityMap = CityMap (Array (Int, Int) Bool)

generateMap' width height = do
    tiles <- generateMap width height
    return (CityMap (expandMap tiles))

expandMap tiles = 
    let (width, height) = snd (bounds tiles) .+. (1, 1) in
    let (width', height') = (width, height) .* 3 in
    array ((0, 0), (width' - 1, height' - 1)) [((x', y'), value) |
        x' <- [0 .. width' - 1],
        y' <- [0 .. height' - 1],
        let n = y' `mod` 3 == 2,
        let s = y' `mod` 3 == 0,
        let e = x' `mod` 3 == 2,
        let w = x' `mod` 3 == 0,
        let vertical = y' `mod` 3 == 1,
        let horizontal = x' `mod` 3 == 1,
        let value = case tiles ! (x' `div` 3, y' `div` 3) of
                Just (Tile n' s' e' w') -> 
                    (vertical && horizontal && (n' || s' || e' || w')) ||
                    ((vertical || horizontal) && 
                    ((n && n') || (s && s') || (e && e') || (w && w')))
                Nothing -> False
        ]

instance Show CityMap where
    show (CityMap tiles) = 
        let ((x1, y1), (x2, y2)) = bounds tiles in
        intercalate "\n" [[if tiles ! (x, y) then ':' else '#' | x <- [x1 .. x2]] | y <- [y1 .. y2]]

