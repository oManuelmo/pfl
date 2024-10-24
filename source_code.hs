import Data.List (nub)
import qualified Data.Set as Set
--import qualified Data.Array
--import qualified Data.Bits

-- PFL 2024/2025 Practical assignment 1

-- Uncomment the some/all of the first three lines to import the modules, do not change the code of these lines.

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]
type Visited = Set.Set City

cities :: RoadMap -> [City]
cities roadMap = nub $ concatMap (\(c1, c2, _) -> [c1, c2]) roadMap

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent [] city1 city2 = False
areAdjacent ((c1, c2, _):roadMap) city1 city2 = ((c1==city1 && c2==city2) || (c2==city1 && c1==city2)) || (areAdjacent roadMap city1 city2)
--areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing -- o Nothing é por causa do Maybe
distance ((c1, c2, dist):roadMap) city1 city2
    | (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1) = Just dist -- o Just é por causa do Maybe
    | otherwise = distance roadMap city1 city2

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] city = []
adjacent ((c1, c2, dist):roadMap) city
    | c1 == city = [(c2, dist)] ++ (adjacent roadMap city)
    | c2 == city = [(c1,dist)] ++ (adjacent roadMap city)
    | otherwise = adjacent roadMap city

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap (firstCity:secondCity:path) = do
    firstDist <- distance roadMap firstCity secondCity
    remainingDists <- pathDistance roadMap (secondCity:path)
    return (firstDist + remainingDists)

--funcao para mostrar os indegrees das cidades todas
getAlladj:: RoadMap -> [(City,Int)]
getAlladj roadMap = map (\city -> (city, length (adjacent roadMap city))) (cities roadMap)

getBiggest :: RoadMap -> [City] -> City -> [City]
getBiggest roadMap city1 city2 
    | length(adjacent roadMap (head city1)) == length(adjacent roadMap city2) = city1 ++ [city2]
    | length(adjacent roadMap (head city1)) > length(adjacent roadMap city2) = city1
    | otherwise = [city2]

rome :: RoadMap -> [City]
rome roadMap = foldl compare [] (cities roadMap)
    where 
        compare [] city = [city]  --se tiver vazio mete a primeira cidade
        compare biggestCities city = getBiggest roadMap biggestCities city --compara cidade a cidade


adjacent2 :: RoadMap -> City -> [City]
adjacent2 [] city = []
adjacent2 ((c1, c2, dist):roadMap) city
    | c1 == city = [c2] ++ (adjacent2 roadMap city)
    | c2 == city = [c1] ++ (adjacent2 roadMap city)
    | otherwise = adjacent2 roadMap city


dfs :: RoadMap -> City -> Visited -> [City]
dfs roadMap city visited
    | Set.member city visited = [] --se estiver visitada nao adiciona
    | otherwise = [city] ++ concatMap(\adjacent -> dfs roadMap adjacent nVisited) adjacents
    where  
        adjacents = adjacent2 roadMap city
        nVisited = Set.insert city visited


--temos que usar Set.size porque pode msm assim ocorrer duplicados ao fazer a funcao dfs
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap 
    | Set.size(Set.fromList (dfs roadMap (head(cities roadMap)) Set.empty)) == length(cities roadMap) = True
    | otherwise = False


shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]