import System.IO (isEOF)
import Text.Printf
import System.Environment
import Data.Graph as Graph
import Data.Array as Array
import Data.List as List

main = do
    a <- readInput
    b <- getArgs
    putStrLn $ join "\n" $ format a b

(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
(∈) = elem

-- use this to properly format your input
-- argument: the lines read from stdin
-- return:   the lines to be put to stdout
format :: [[Char]] -> [[Char]]-> [[Char]]
format input args = let
--    (taxi, bus, metro, ship) = generateGraphs(input)
    graphs = generateGraphs(input)
    start = read (args!!0) :: Int
    moves = map getMove (tail args)
    out = calcStates graphs [start] moves
    in [show (sort (nub out))]


calcStates :: (Graph, Graph, Graph, Graph) -> [Int] -> [Move] -> [Int]
calcStates graphs states [] = states
calcStates graphs states (move:moves) = calcStates graphs newStates moves where
    newStates = genStates graphs move states where
        genStates :: (Graph, Graph, Graph, Graph) -> Move -> [Int] -> [Int]
        genStates graphs move states = foldr (\state remStates -> (getSuccessors graphs move state) ++ remStates) [] states where
            getSuccessors :: (Graph, Graph, Graph, Graph) -> Move -> Int -> [Int]
            getSuccessors (taxi, bus, metro, ship) Taxi  state = (taxi  ! state)
            getSuccessors (taxi, bus, metro, ship) Bus   state = (bus   ! state)
            getSuccessors (taxi, bus, metro, ship) Metro state = (metro ! state)
            getSuccessors (taxi, bus, metro, ship) Ship  state = (ship  ! state)
            getSuccessors (taxi, bus, metro, ship) Any   state = (taxi ! state) ++ (bus ! state) ++ (metro ! state) ++ (ship ! state)

data Move = Taxi | Bus | Metro | Ship | Any

getMove :: [Char] -> Move
getMove ('t':x) = Taxi
getMove ('b':x) = Bus
getMove ('m':x) = Metro
getMove ('s':x) = Ship
getMove ('a':x) = Any

generateGraphs :: [[Char]] -> (Graph, Graph, Graph, Graph)
generateGraphs lines = (buildG (1,175) (parseGraph lines 't'), buildG (1,175) (parseGraph lines 'b'), buildG (1,175) (parseGraph lines 'm'), buildG (1,175) (parseGraph lines 's'))

parseGraph :: [[Char]] -> Char -> [Edge]
parseGraph [] char = []
parseGraph (a:lines) char =  let (pref,line) = (head a, tail a)
    in if (pref == char) then (parseLine line) ++ (parseGraph lines char) else (parseGraph lines char)

parseLine :: [Char] -> [Edge]
parseLine (' ' : chars) = foldr (\a l -> ((read x, (read a))):l ) [] (tail numList) where
   numList = split ' ' chars
   x = head numList
parseLine _ = []



trim :: [Char] -> Int -> [Char]
trim [] n = []
trim xs 0 = "..."
trim (x:xs) n = x:(trim xs (n-1))

-- startsWith a b : does b start with a?
startsWith :: [Char] -> [Char] -> Bool
startsWith [] s = True
startsWith s [] = False
startsWith (x:xs) (y:ys) = (x==y) && (startsWith xs ys)

remove :: [Char] -> [Char] -> [Char]
remove [] s = s
remove x [] = []
remove (x:xs) (y:ys) = if (x == y) then (remove xs ys) else y:(remove (x:xs) ys)

removeAll :: Char -> [Char] -> [Char]
removeAll c [] = []
removeAll c (x:xs) = if (c == x) then (removeAll c xs) else x : (removeAll c xs)

split :: Char -> [Char] -> [[Char]]
split c [] = []
split c xs = let 
    (w, rem) = nextWord c xs ([],[])
    in w:(split c rem) where
        nextWord :: Char -> [Char] -> ([Char], [Char]) -> ([Char], [Char])
        nextWord c [] (a,b) = (a,b)
        nextWord c (x:xs) (a,b)= if c == x
            then (a, xs)
            else nextWord c xs (a++[x],xs)

join :: [Char] -> [[Char]] -> [Char]
join s [] = []
join s [x] = x
join s (x:xs) = x++s++(join s xs)

-- reads stdin until EOF is reached
readInput :: IO [[Char]]
readInput = do
    done <- isEOF
    if done
            then return []
            else do
                input <- getLine
                nextLine <- readInput
                return (input : nextLine) 
