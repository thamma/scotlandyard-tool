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

format :: [[Char]] -> [[Char]]-> [[Char]]
format input args = 
    if (length input == 0)
        then ["No mapfile fed to input stream."]
    else if (length args == 0) then ["usage: ./scotlandyard [start] [transition...]", "where start is an integer and","transitions is a sequence of the characters 't', 'b', 'm' and 'a'."]
    else let
    graphs = generateGraphs(input)
    start = read (head args) :: Int
    moves = getMoves (tail args)
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

getMoves :: [[Char]] -> [Move]
getMoves input = getMove (join "" input)

getMove :: [Char] -> [Move]
getMove [] = []
getMove ('t':x) = Taxi: (getMove x)
getMove ('b':x) = Bus : (getMove x)
getMove ('m':x) = Metro : (getMove x)
getMove ('s':x) = Ship : (getMove x)
getMove ('a':x) = Any : (getMove x)

generateGraphs :: [[Char]] -> (Graph, Graph, Graph, Graph)
generateGraphs lines = let
    edgesT  = parseGraph lines 't'
    boundsT = bounds edgesT
    graphT  = buildG boundsT edgesT
    in (buildG (1,175) (parseGraph lines 't'), buildG (1,175) (parseGraph lines 'b'), buildG (1,175) (parseGraph lines 'm'), buildG (1,175) (parseGraph lines 's')) where
        bounds edges = (foldr (\(a,b) m -> min m (min a b) ) (fst $ head edges) edges, foldr (\(a,b) m -> max m (max a b) ) (fst $ head edges) edges)



parseGraph :: [[Char]] -> Char -> [Edge]
parseGraph [] char = []
parseGraph ("":lines) char = parseGraph lines char
parseGraph (('#':x):lines) char = parseGraph lines char
parseGraph (a:lines) char =  let (pref,line) = (head a, tail a)
    in if (pref == char) then (parseLine line) ++ (parseGraph lines char) else (parseGraph lines char)

parseLine :: [Char] -> [Edge]
parseLine (' ' : chars) = foldr (\a l -> ((read x, (read a))):l ) [] (tail numList) where
   numList = split ' ' chars
   x = head numList
parseLine _ = []

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
