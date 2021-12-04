module Walk where

-- indicates the number of birds on L and R
type Conf = (Int, Int)

-- indicates that e.g. 1 bird landed on R (1, R) or 2 birds left R (-2, R)
type Move = (Int, Char)

positiveSum :: Int -> Int -> Int 
positiveSum x y = if (x + y) > 0 then x + y else 0

checkBalance :: Int -> Int -> Int -> Bool
checkBalance x y z = abs (x + y - z) > 4

-- returns a new conf
moveP1 :: Move -> Conf -> Conf
moveP1 (birds, side) (left, right) = case side of 
 'L' -> if checkBalance birds left right then error "troppi uccelli" else (positiveSum left birds, right)
 'R' -> if checkBalance birds right left then error "troppi uccelli" else (left, positiveSum right birds)

performMoveP1 :: Move -> [Conf] -> [Conf]
performMoveP1 m l = l ++ [moveP1 m (last l)]
 
play1 :: [Move] -> (Conf -> [Conf])
play1 moves = \c -> tail (foldr performMoveP1 [c] (reverse moves))





checkBalanceP2 :: Move -> Conf -> Bool
checkBalanceP2 (birds, side) (left, right) = case side of
 'L' -> abs (left + birds - right) <= 4
 'R' -> abs (right + birds - left) <= 4

performMoveP2 :: Move -> Conf -> Conf
performMoveP2 (birds, side) (left, right) = case side of
 'L' -> (positiveSum left birds, right)
 'R' -> (left, positiveSum right birds)
 
performMoveOnListP2 :: Move -> Maybe [Conf] -> Maybe [Conf]
performMoveOnListP2 m Nothing = Nothing
performMoveOnListP2 m (Just l)
 | checkBalanceP2 m (last l) = Just (l ++ [performMoveP2 m (last l)])
 | otherwise = Nothing
 
play2 :: [Move] -> (Conf -> Maybe [Conf])
play2 ml = \c -> do
 cl <- foldr performMoveOnListP2 (Just [c]) (reverse ml)
 return (tail cl)
 
 
 
 
 
performMoveP3 :: Conf -> Move -> Maybe Conf
performMoveP3 (left, right) (birds, side) = case side of
 'L' -> if checkBalance birds left right then Nothing else Just (positiveSum left birds, right)
 'R' -> if checkBalance birds right left then Nothing else Just (left, positiveSum right birds)

play3 :: Maybe Conf -> [Move] -> Maybe [Conf]
play3 Nothing _ = Nothing
play3 (Just startingConfiguration) [] = Just []
play3 (Just startingConfiguration) (fm : rms) = (:)<$>appliedMove<*>(play3 appliedMove rms)
 where appliedMove = performMoveP3 startingConfiguration fm 

 
 
 