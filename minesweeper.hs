module Minesweeper(minedistirbutor) where
import System.Random

length::Int
length = 6

heigth::Int
heigth = 6

minetablesize :: Int
minetablesize = 36

mine::Int
mine = 1
nothing::Int
nothing = 0

minedistirbutor :: RandomGen a => Int ->Int -> Int -> a -> [[Int]];
minedistirbutor length heigth diff rndmmine = matrix length heigth diff (minegen length heigth diff rndmmine)
  where  

    minegen length heigth diff rndmmine = [if x == 0 then mine else nothing | x<- (take minetablesize) (randomRs (0::Int, diff::Int) rndmmine)]
    matrix length heigth diff newlst = [[newlst!!((heigth * i) + j) | j <- [0.. length-1]] | i <- [0..heigth-1]]
  
main = do

   putStrLn "Please choose difficulty. \nEasy pizzy = 1\nMedium = 2\nHard = 3\nHardcore = 4"
   diffsetter <- getLine 
   if ("1" == diffsetter) 
     then do   
             rndmmine <- getStdGen
             let table = minedistirbutor 6 6 10 rndmmine
             putStrLn "Difficulty set to easy pizzy!"  

       else if ("2" == diffsetter)
       then do 
             rndmmine <- getStdGen
             let table = minedistirbutor 6 6 6 rndmmine
             putStrLn "Difficulty set to medium!"  

       else if ("3" == diffsetter) 
       then do 
             rndmmine <- getStdGen
             let table = minedistirbutor 6 6 4 rndmmine
             putStrLn "Difficulty set to hard!"  

       else  do
             rndmmine <- getStdGen
             let table = minedistirbutor 6 6 3 rndmmine
             putStrLn "Difficulty set to hardcore!";