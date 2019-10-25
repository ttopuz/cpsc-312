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

minedistirbutor :: RandomGen a => Int ->Int -> Int -> a -> [[Int]];--random number generator to create random mines around the board.
minedistirbutor length heigth diff rndmmine = matrix length heigth diff (minegen length heigth diff rndmmine)
  where  
    minegen length heigth diff rndmmine = [if x == 0 then mine else nothing | x<- (take minetablesize) (randomRs (0::Int, diff::Int) rndmmine)] --create random numbers between 2 specified numbers, if it is 0 it is mine, if not empty 
    matrix length heigth diff newlst = [[newlst!!((heigth * i) + j) | j <- [0.. length-1]] | i <- [0..heigth-1]] --creates [[Int]] inspired from stakeflow.com

main :: IO()
main = do --program opener
   putStrLn "Wellcome to the minesweeper!"
   newgame_ask --asks if it is a new game 
   putStrLn "Please select a difficulty. \nEasy pizzy = 1\nMedium = 2\nHard = 3\nHardcore = 4"--selection of game difficulty
   diffsetter <- getLine 
   mined_table <- if ("1" == diffsetter) --registering the table generated to mined_table
       then do   
             rndmmine <- getStdGen
             let table = minedistirbutor 6 6 10 rndmmine--board generator 
             let interfacetable = usertable 6 6 0 rndmmine--empty board generator for the player 
             printtable interfacetable
             putStrLn "Difficulty set to easy pizzy!"
             putStrLn "Actual table was:"
             printtable table  

       else if ("2" == diffsetter)
       then do 
             rndmmine <- getStdGen
             let table = minedistirbutor 9 9 6 rndmmine
             let interfacetable = usertable 9 9 0 rndmmine
             printtable interfacetable
             putStrLn "Difficulty set to medium!"
             putStrLn "Actual table was:"
             printtable table  

       else if ("3" == diffsetter) 
       then do 
             rndmmine <- getStdGen
             let table = minedistirbutor 10 10 4 rndmmine
             let interfacetable = usertable 10 10 0 rndmmine
             printtable interfacetable
             putStrLn "Difficulty set to hard!" 
             putStrLn "Actual table was:"
             printtable table 
             
       else  do
             rndmmine <- getStdGen
             let table = minedistirbutor 10 10 2 rndmmine
             let interfacetable = usertable 10 10 0 rndmmine
             printtable interfacetable
             putStrLn "Difficulty set to hardcore!";
             putStrLn "Actual table was:"
             printtable table

   putStrLn "a"




  

printtable :: Show a => [[a]] -> IO ()
printtable table = putStrLn (show table) --board print function


newgame_ask = do --new game asker 
  putStrLn "Do you want to start a new game?"
  is_it_new<- getLine 
  newgame <- if  ("yes" == is_it_new) --registering the input to newgame
    then do
     let newgame = True
     return newgame
     putStrLn "New game set selected"
  else do 
     let newgame = False 
     return newgame
     putStrLn "Continue"
  return newgame


usertable :: RandomGen a => Int ->Int -> Int -> a -> [[Int]]; --function that creates player board to compare the actual board created 
usertable length heigth diff rndmmine = matrix length heigth diff (minegen length heigth diff rndmmine)
  where  
    minegen length heigth diff rndmmine = [if x == 1 then mine else nothing | x<- (take minetablesize) (randomRs (0::Int, 0::Int) rndmmine)]
    matrix length heigth diff newlst = [[newlst!!((heigth * i) + j) | j <- [0.. length-1]] | i <- [0..heigth-1]]
