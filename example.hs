
title :: String -> IO ()
title str = do
    putStrLn str
    putStrLn (replicate (length str) '-')
    putStrLn ""

main :: IO ()
main = do
    title "Hello"
    title "Goodbye"
