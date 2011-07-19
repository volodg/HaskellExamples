
charsCount :: [ ( Char, Int ) ] -> Char -> [ ( Char, Int ) ]
charsCount [] ch = [ ( ch, 1 ) ]
charsCount (( ch1, num ):xs) ch2 | ch1==ch2 = ( ch1, num + 1 ):xs
								 | otherwise = ( ch1, num ):charsCount xs ch2

minimumFrequency :: [ ( Char, Int ) ] -> Int
minimumFrequency [] = error "undefined result"
minimumFrequency (( _, num ):[]) = num
minimumFrequency (( _, num ):xs) | num < minimumFrequency( xs ) = num
						   		 | otherwise = minimumFrequency( xs )

main :: IO ()
main = do
   src <- readFile "source.txt"
   frequency <- return ( foldl (charsCount) [] src )
   minimum' <- return ( minimumFrequency frequency )
   print [ ch | ( ch, num ) <- frequency, num == minimum' ]
