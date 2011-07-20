import Char (isUpper, isAlpha)

withinGuardsChars :: [ Char ] -> Bool
withinGuardsChars ( y ) = not( isUpper( y !! 0 ) )
 && isUpper( y !! 1 )
 && isUpper( y !! 2 )
 && isUpper( y !! 3 )
 && not( isUpper( y !! 4 ) ) && isAlpha( y !! 4 )
 && isUpper( y !! 5 )
 && isUpper( y !! 6 )
 && isUpper( y !! 7 )
 && not( isUpper( y !! 8 ) )

withinGuards :: [ [ Char ] ] -> [ Char ] -> [ Char ] -> [ [ Char ] ]
withinGuards results ( _ ) [] = [ [ ch | chars <- results, let ch = chars !! 4 ] ]
withinGuards results ( sevenLettersList ) ( char:xs ) | length( sevenLettersList )<9 = withinGuards results (char:sevenLettersList) xs
                                                      | withinGuardsChars( sevenLettersList ) = withinGuards ( sevenLettersList:results ) [] xs
                                                      | otherwise = withinGuards results ( char:reverse( tail( reverse( sevenLettersList ) ) ) ) xs

main :: IO ()
main = do
   src <- readFile "source.txt"
   print ( withinGuards [] [] ( reverse( src ) ) )
-- ^   frequency <- return ( foldl (charsCount) [] src )
-- ^   minimum' <- return ( minimumFrequency frequency )
-- ^   print [ ch | ( ch, num ) <- frequency, num == minimum' ]
