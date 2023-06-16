module Razb
()
where

import Le
import Pa



f :: String -> String
f s = show $ parse $ alexScanTokens s


main :: IO ()
main = do
    s <- readFile "input.txt"
    putStrLn $ f s
