import Char (isUpper)

withinGuardsChars :: [ Char ] -> Bool
withinGuardsChars ( y ) = isUpper( y !! 0 ) && isUpper( y !! 1 )
-- ^    && isUpper( y !! 2 ) && not( isUpper( y !! 3 ) ) && isUpper( y !! 4 ) && isUpper( y !! 5 ) && isUpper( y !! 6 )

withinGuards :: [ Char ] -> [ Char ] -> [ Char ]
withinGuards ( _ ) [] = []
withinGuards ( sevenLettersList ) ( char:xs ) | length( sevenLettersList )<7 = withinGuards (char:sevenLettersList) xs
                                              | withinGuardsChars( sevenLettersList ) = sevenLettersList
                                              | otherwise = withinGuards ( char:tail( sevenLettersList ) ) xs

main :: IO ()
main = do
   src <- readFile "source.txt"
   print ( withinGuards [] src )
-- ^   frequency <- return ( foldl (charsCount) [] src )
-- ^   minimum' <- return ( minimumFrequency frequency )
-- ^   print [ ch | ( ch, num ) <- frequency, num == minimum' ]
