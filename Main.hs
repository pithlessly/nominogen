import Data.List (transpose)
import qualified Data.Set

type Pattern = [[Bool]]

nominoes :: Int -> [Pattern]
nominoes 1 = [[[True]]]
nominoes n = simplify $ concatMap develop $ nominoes (n - 1)
        
replace :: Int -> (a -> a) -> [a] -> [a]
replace n f xs = (take n xs) ++ [f (xs !! n)] ++ (drop (n + 1) xs)

develop :: Pattern -> [Pattern]
develop p =
  let width = length (head p)
      height = length p
      paddingRow = replicate width False
      padded = map (\row -> [False] ++ row ++ [False]) $
               [paddingRow] ++ p ++ [paddingRow]
      zz = zipWith zip
      rotations =
        (tail padded ++ [head padded]) `zz`
        ([last padded] ++ init padded) `zz`
        (map (\row -> tail row ++ [head row]) padded) `zz`
        (map (\row -> [last row] ++ init row) padded) `zz`
        padded
      candidates =
        (map.map) (\((((n, s), e), w), center) -> not center && (n || s || e || w)) rotations
  in
  [ replace y (replace x (const True)) padded
  | (y, row)   <- zip [0..] candidates,
    (x, True)  <- zip [0..] row
  ]

simplify :: [Pattern] -> [Pattern]
simplify pats =
  let dz = dropWhile (all not)
      trimmed =
        map (transpose . dz . reverse . dz . reverse .
             transpose . dz . reverse . dz . reverse) pats
      canonicalized =
        map (\p -> minimum ([id, reverse . map reverse, reverse . transpose, transpose . reverse] <*> [p])) trimmed
  in
  Data.Set.toList $ Data.Set.fromList canonicalized

allnominoes = iterate (simplify . concatMap develop) [[[True]]]
