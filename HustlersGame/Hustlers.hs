import System.IO  
  
main = do  
    handle <- openFile "HowToPlay.txt" ReadMode  
    contents <- hGetContents handle  
    putStrLn contents  
    hClose handle  