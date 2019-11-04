import Data.List
import System.FilePath.Posix
import System.Environment
import System.Process
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)

main :: IO ()
main = do  args <- getArgs
           if ((length args) /= 2)
             then do putStrLn "Usage: GenAsyncTypes <int> [flag]"
                     return ()
             else do let x = read (args!!1) :: Int
                         y = read (args!!0) :: Int
                         f = if length args > 1
                             then args!!1
                             else ""
                         
                     runChecker y x f
                     
mkPair :: Int -> Int -> (String, String)
mkPair y x = let sub = "rec x. "++(genSubSendSequence y)++";["++(genSubBranches x)++"]"
                 sup = "rec x. ["++(genSupSndBranches x y)++"]"
             in (sub, sup)

genSubSendSequence :: Int -> String
genSubSendSequence i = intercalate "; " $
                       map
                       (\x -> "!a"++(show x))
                       [0..i]
                    

genSubBranches :: Int -> String
genSubBranches i = intercalate ", " $
                   map
                   (\x -> "?b"++(show x)
                          ++"; x")
                   [0..i]

--

genSupRcvBranches :: Int -> String
genSupRcvBranches i = intercalate ", " $
                      map
                      (\x -> "?b"++(show x)++"; x")
                      [0..i]

genSupSndBranches :: Int -> Int -> String
genSupSndBranches j i =  intercalate ", " $
                         map
                         (\x -> "!a"++(show x)++";["++(genSupRcvBranches j)++"]")
                         [0..i]



runChecker :: Int -> Int -> String -> IO ()
runChecker y x  flag =
  let (sub, sup) = mkPair y x
      cmd = "./Checker -i "++flag++" '"++sub++"'  '"++sup++"'"
  in do writeToFile "m1.txt" sub
        writeToFile "m2.txt" sup
        -- putStrLn sub
        -- putStrLn sup
        -- start <- getCurrentTime
        -- out <- readProcess "bash" ["-c", cmd] []
        -- end <- getCurrentTime
        -- putStrLn $ (show $ diffUTCTime end start)
        -- print out
        return ()
        



writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content
