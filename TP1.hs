import Data.List (minimumBy, nub, find)
import Data.Bits (shiftL, testBit, setBit, clearBit, popCount)
import Data.Array (Array, array, bounds, (!), (//), accumArray)
import Debug.Trace (trace)

-- PFL 2024/2025 Practical assignment 1

type City = String
type Path = [City]
type Distance = Int
type RoadMap = [(City,City,Distance)]

data Priority = Priority {city :: City, dist :: Distance }
    deriving (Show, Eq)

--declaracao da heap
data Heap = Empty | Node Priority  Heap  Heap
    deriving (Show, Eq)

type DPTable = Array (Int, Int) (Maybe Distance)

--merge e rebalanceia a heap
mergeHeap :: Heap -> Heap -> Heap
mergeHeap Empty heap = heap
mergeHeap heap Empty = heap
mergeHeap (Node x esq1 dir1) (Node y esq2 dir2) 
    | dist x <= dist y = Node x esq1 (mergeHeap dir1 (Node y esq2 dir2))
    | otherwise = Node y esq2 (mergeHeap (Node x esq1 dir1) dir2)

--insert na heap
insertHeap :: Priority -> Heap -> Heap
insertHeap x Empty = Node x Empty Empty
insertHeap x (Node y esq dir) = mergeHeap (Node x Empty Empty) (Node y esq dir)

--remove o primeiro elemento da heap
removeHeap :: Heap -> Heap
removeHeap Empty = Empty
removeHeap (Node _ esq dir) = mergeHeap esq dir

removeXHeap :: City -> Heap -> Heap
removeXHeap _ Empty = Empty
removeXHeap c (Node (Priority c1 d) esq dir) 
    | c == c1 = mergeHeap esq dir
    | otherwise = Node (Priority c1 d) (removeXHeap c esq) (removeXHeap c dir)

--retorna o primeiro elemento da heap
getMinHeap :: Heap -> Maybe Priority
getMinHeap Empty = Nothing
getMinHeap (Node p _ _) = Just p

--altera os valores das distancias
modifyPriority :: City -> Distance -> Heap -> Heap
modifyPriority _ _ Empty = Empty
modifyPriority city newDistance (Node (Priority c d) esq dir)
    | c == city && newDistance < d = mergeHeap (Node (Priority c newDistance) esq dir) Empty
    | otherwise = Node (Priority c d)(modifyPriority city newDistance esq) (modifyPriority city newDistance dir)

--verifica se uma dada cidade está presente na heap
findHeap :: City -> Heap -> Maybe Priority
findHeap c Empty = Nothing
findHeap c1 (Node(Priority c2 d) esq dir) 
    | c1 == c2 = Just (Priority c2 d)
    | otherwise = case findHeap c1 esq of
        Just priority -> Just priority
        Nothing -> findHeap c1 dir

updateHeap :: Priority -> Heap -> Distance -> [(City,City)] -> City -> (Heap,[(City,City)])
updateHeap (Priority c _) heap d2 oldPredecessors origin =
    case findHeap c heap of
        Just (Priority c dt) ->  --"1" 100
            if d2 < dt then 
                let updatedHeap = insertHeap (Priority c d2) (removeXHeap c heap) 
                    newPredecessors = (c, origin) : filter (\(city, _) -> city /= c) oldPredecessors  --fica apenas com os tuples que nao têm c como predecessor    
                in (updatedHeap, newPredecessors)
            else
                (heap, oldPredecessors)
        Nothing -> (heap, oldPredecessors)

-- Função para criar uma lista de todas as cidades únicas
-- RoadMap: Grafo
-- [City]: Lista de cidades
cities :: RoadMap -> [City]
cities roadMap = nub $ concatMap (\(c1, c2, _) -> [c1, c2]) roadMap

--Função para ver se duas cidades são visinhas
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent roadMap city1 city2 = any (\(c1, c2, _) -> (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1)) roadMap

--Função para calcular a distância entre duas cidades vizinhas, se não forem vizinhas, retorna Nothing
distance :: RoadMap -> City -> City -> Maybe Distance
distance [] _ _ = Nothing -- o Nothing é por causa do Maybe
distance ((c1, c2, dist):roadMap) city1 city2
    | (c1 == city1 && c2 == city2) || (c1 == city2 && c2 == city1) = Just dist -- o Just é por causa do Maybe
    | otherwise = distance roadMap city1 city2

--Função que cria uma lista das cidades vizinhas e a respetiva distância de uma determinada cidade
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] city = []
adjacent ((c1, c2, dist):roadMap) city
    | c1 == city = (c2, dist) : adjacent roadMap city
    | c2 == city = (c1,dist) : adjacent roadMap city
    | otherwise = adjacent roadMap city

--Função que calcula a distância de um dado caminho
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance _ [] = Just 0
pathDistance _ [_] = Just 0
pathDistance roadMap (firstCity:secondCity:path) = do
    firstDist <- distance roadMap firstCity secondCity
    remainingDists <- pathDistance roadMap (secondCity:path)
    return (firstDist + remainingDists)

--Função que retorna uma lista das cidades com maior vizinhança
getBiggest :: RoadMap -> [City] -> City -> [City]
getBiggest roadMap city1 city2 
    | length(adjacent roadMap (head city1)) == length(adjacent roadMap city2) = city1 ++ [city2]
    | length(adjacent roadMap (head city1)) > length(adjacent roadMap city2) = city1
    | otherwise = [city2]

--Função que retorna as cidades com maior número de cidades vizinhas
rome :: RoadMap -> [City]
rome roadMap = foldl compare [] (cities roadMap)
    where 
        compare [] city = [city]  --se tiver vazio mete a primeira cidade
        compare biggestCities city = getBiggest roadMap biggestCities city --compara cidade a cidade

--Função idêntia a adjacent, porém retorna apenas as cidades vizinhas
adjacent2 :: RoadMap -> City -> [City]
adjacent2 [] city = []
adjacent2 ((c1, c2, dist):roadMap) city
    | c1 == city = c2 : adjacent2 roadMap city
    | c2 == city = c1 : adjacent2 roadMap city
    | otherwise = adjacent2 roadMap city

--Função DFS para percorrer pelas cidades BLABLABLA
dfs :: RoadMap -> City -> [City] -> [City]                  
dfs roadMap city visited
    | city `elem` visited = [] --se estiver visitada nao adiciona
    | otherwise = city : concatMap(\adjacent -> dfs roadMap adjacent nVisited) adjacents
    where  
        adjacents = adjacent2 roadMap city
        nVisited = city : visited


isStronglyConnected :: RoadMap -> Bool
isStronglyConnected roadMap = length (nub (dfs roadMap (head (cities roadMap)) [])) == length (cities roadMap)


--constroi o caminho criado pelo dijkstra
constructPath :: [(City,City)] -> City -> Path  
constructPath predecessors end = 
    case lookup end predecessors of
        Nothing -> [end]
        Just predecessor -> constructPath predecessors predecessor ++ [end]

--algoritmo Dijkstra para encontrar os caminhos mais curtos entre cidades
dijkstra :: RoadMap -> Heap -> City -> [City] -> [(City, City)] -> [Path]
dijkstra roadMap Empty _ _ _ = []
dijkstra roadMap heap end visited predecessors
    | citi == end = trace "oi" [constructPath predecessors end] -- Found a path
    | citi `elem` visited = 
        trace ("City " ++ show citi ++ " has already been visited, updating distances.") $ 
        dijkstra roadMap updatedHeap end visited updatedPredecessors -- If already visited, update distances
    | otherwise = 
        trace ("Visiting city: " ++ show citi ++ ", current distance: " ++ show dst) $
        dijkstra roadMap newHeap end (citi : visited) newPredecessors  -- Mark as visited and update distances
    where
        Just (Priority citi dst) = getMinHeap heap -- Top of the heap
        adjacents = adjacent roadMap citi -- Adjacent cities
        -- Trace adjacent processing
        newHeap = foldl (\heap (adj, d) -> 
            trace ("Processing adjacent city: " ++ show adj) $
            if adj `notElem` visited 
            then 
                trace ("Adding adjacent city: " ++ show adj ++ " with new distance: " ++ show (d + dst)) $ 
                insertHeap (Priority adj (d + dst)) heap

            else 
                trace ("City " ++ show adj ++ " already visited.") $ 
                heap) (removeHeap heap) adjacents -- Updated heap

        (updatedHeap, updatedPredecessors) = foldl (\(h, preds) (adj, d) -> 
            trace ("Checking predecessor for city: " ++ show adj) $
            if adj `notElem` visited 
            then 
                let (nHeap, nPreds) = updateHeap (Priority adj (d + dst)) h (d + dst) preds citi 
                in 
                    trace ("Updated distance for city: " ++ show adj ++ " to " ++ show (d + dst)) $
                    (nHeap, nPreds) 
            else 
                trace ("City " ++ show adj ++ " already visited during update.") $ 
                (h, preds)) (removeHeap heap, predecessors) adjacents
        
        newPredecessors = predecessors ++ 
            [(adj, citi) | (adj, _) <- adjacents, adj `notElem` visited] -- Updated predecessors


--funcao que utiliza o algoritmo Dijkstra para calcular os paths mais curtos entre 2 cidades
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath roadMap start end = dijkstra roadMap (insertHeap (Priority start 0) Empty) end [] []


-- Função para transformar uma string em integer
-- City: cidade (ex.: "0")
cityIndex :: City -> Int -- "0" para 0
cityIndex city = read city :: Int

-- Função auxiliar para lidar com valores Maybe
-- Se o primeiro argumento for Nothing, então retorna o segundo argumento
-- Maybe a: Primeiro argumento
-- a: Segundo argumento
orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing y = y

-- Função para encontrar a distância entre duas cidades com valor padrão, se não houver conexão
-- Idêntica a fromMaybe
-- RoadMap: Grafo
-- City: Cidade 1
-- City: Cidade 2
-- Distance: distância padrão (neste caso será sempre 2147483647)
-- Distance: retorna a dsitância entre as duas cidades
distanceOrDefault :: RoadMap -> City -> City -> Distance -> Distance
distanceOrDefault roadMap city1 city2 defaultDist = 
    maybe defaultDist id (distance roadMap city1 city2)

-- Função auxiliar em Dynamic programming para resolver o TSP com memoização
-- RoadMap: Grafo
-- DPTable: Tabela de memoização que guarda subproblemass já resolvidos anteriormente (custos das cidades)
-- City: Primeira e última cidade
-- City: Cidade atual
-- Int: BitMask das cidades atualmente visitadas (ex.: 01010111)
-- Int: BitMask de todas as cidades visitadas (ex.: 11111111)
-- Distance: retorna a menor distância total necessária para visitar todas as cidades do RoadMap e retornar à cidade inicial, partindo da cidade atual e sem visitar as que já foram visitadas
tspDP :: RoadMap -> DPTable -> City -> City -> Int -> Int -> Distance
tspDP roadMap dp startCity currentCity visited allVisited
    | visited == allVisited = distanceOrDefault roadMap currentCity startCity 2147483647  -- Se tiver visitado tudo, retorna para o inicial
    | otherwise = dp ! (cityIndex currentCity, visited) `orElse` -- Se ainda não estiver no DPTable então resolve-se o subproblema recursivamente para encontrar a menor distância
                minimum [distanceOrDefault roadMap currentCity nextCity 2147483647 -- Distância da cidade atual para a próxima, se não exsitir fica o INT_MAX
                        + tspDP roadMap dp startCity nextCity (visited `setBit` nextCityIdx) allVisited -- Soma-se a menor distância da proxima cidade para a primeira cidade com a próxima cidade visitada
                        | nextCity <- cities roadMap, let nextCityIdx = cityIndex nextCity, not (testBit visited nextCityIdx)] -- Faz se isto para todas as cidades ainda não visitadas

-- Função que resolve o problema TSP e retorna o caminho mais rápido
-- RoadMap: Grafo
-- Path: Caminho do TSP
travelSales :: RoadMap -> Path
travelSales roadMap =
    let allCities = cities roadMap
        n = length allCities
        startCity = "0"  -- Começa-se pela cidade "0" 
        allVisited = (1 `shiftL` n) - 1  -- Bitmask para todas as cidades visitadas (11111101)
        dp = array ((0, 0), (n - 1, allVisited)) [((i, visited), Nothing) | i <- [0..n-1], visited <- [0..allVisited]] -- Dynamic Programming table para guardar as menores distâncias
    in buildPath roadMap dp startCity allVisited  -- Chama a função buildPath para construir o caminho para o TSP

-- Função para escolher o mínimo de uma lista com base no primeiro elemento do par, neste caso a distância
-- [(Distance, City)]: Lista de tuplos de distância e cidade
-- (Distance, City): Retorna o tuplo com menor distância
minByFst :: [(Distance, City)] -> (Distance, City)
minByFst = foldr1 (\x y -> if fst x < fst y then x else y)

-- Função que cria o caminho do problema TSP com base nos dados do RoadMap
-- RoadMap: Grafo
-- DPTable: Tabela para armazenar as menores distâncias de todas as combinações de cidades visitadas e não visitadas
-- City: Cidade onde começa e acaba
-- Int: BitMask de todas as cidades visitadas (ex.: 11111111)
-- Path: Retorna o caminho, isto é, a solução do problema
buildPath :: RoadMap -> DPTable -> City -> Int -> Path
buildPath roadMap dp startCity allVisited =
    let path = [startCity]  -- Mete a cidade inicial para a lista
        recurse currentCity visited
            | visited == allVisited = [startCity]  -- Quando todas as cidades foram visitadas, retorna à cidade inicial
            | otherwise =
                let unvisitedCities = filter (not . testBit visited . cityIndex) (cities roadMap) -- Seleciona as cidades ainda não visitadas
                    distances = [(distanceOrDefault roadMap currentCity nc 2147483647 -- distância da cidade atual para outra, senão existir então é INT_MAX
                                + tspDP roadMap dp startCity nc (visited `setBit` cityIndex nc) allVisited, nc) -- Soma-se com o resultado a função tspDP e fica (distância, nc)
                                | nc <- unvisitedCities] -- Faz-se isto para todas as cidades não visitadas
                    nextCity = snd (minByFst distances) -- A proxima cidade será então a cidade o menor custo
                in nextCity : recurse nextCity (visited `setBit` cityIndex nextCity) -- Chama.se recursivamente até todas as cidades estarem visitadas e vai adicionando-as à lista
    in path ++ recurse startCity (1 `shiftL` cityIndex startCity) -- Começa por chamar a funçao recurse com input da primeira cidade e com a primeira cidade visitada (ex.: 0001)


-- Some graphs to test your work
gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",100),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

gTest4 :: RoadMap
gTest4 = [("0","1",100), ("0", "5", 1000), ("1","2",15),("2","3",20),("3","4",55),("4","5",25), ("5", "6", 1)]