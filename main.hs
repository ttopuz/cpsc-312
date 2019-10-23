module Main(main) where

    import Minesweeper
    import System.Random
    import Text.Read
    import System.Maybe
    
    main :: IO()
    main = do
       putStrLn "Wellcome to the minesweeper!"
       --newgame_ask
       (diff, size) <- dif_ask
       playInit diff size
    

    --main game loop for minesweeper, it takes difficulty, size and inputs from the user
    --this functions initiates the loop
    playInit diff size = do
        let UserAction (x,y,c) = readUA size
        grid <- makeGrid size size diff 20
        case c of
            LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
            RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
        let res = minesweeper (UserAction (x,y,c)) grid
        case res of
        EndOfGame val st -> putStrLn("End of game")
        ContinueGame st -> (play st diff size)

    --Main loop for the game, and returns at the end of the game 
    play grid diff size = do
        printGrid grid
        let UserAction (x,y,c) = readUA size
        grid <- makeGrid size size diff 20
        case c of
            LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
            RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
        let res = minesweeper (UserAction (x,y,c)) grid
        case res of
        EndOfGame val st ->  putStrLn("End of game")
        ContinueGame st -> (play st diff size)
    
    --shows the result 
    --playAgain grid val = do
    --printGrid grid
    --case val of
     --   1 -> putStrLn ("You win!")
       -- 0 -> putStrLn ("You lose!")
    
    --takes the difficulty of the game, and adjust size accordingly
    dif_ask = do
        putStrLn "Please select a difficulty. \nEasy pizzy = 1\nMedium = 2\nHard = 3\nHardcore = 4"
        diffsetter <- getLine 
        let size = diffsetter
        return (diffsetter, size)
    
    --gets the inputs from the user, x ,y cordinate and c value which is right click/left click
     -- talen from https://github.com/unoctium1/HaskellMinesweeper/blob/master/src/Minesweeper.hs
    readUA :: Int -> IO UserAction
    readUA size =
        do
            putStrLn ("  Please choose a square.  ")
            line <- getLine
            let ua = readMaybeUA line
            case ua of
                Nothing -> redo
                Just (UserAction (x,y,c)) -> if (x > size) || (y > size) || (x < 0) || (y < 0) then redo else return (UserAction (x,y,c))
                where redo = do
                    putStrLn ("  Please enter a valid coordinate")
                    readUA size
    
    -- https://github.com/unoctium1/HaskellMinesweeper/blob/master/src/Minesweeper.hs
    readMaybeUA :: String -> Maybe UserAction
    readMaybeUA [] = Nothing
    readMaybeUA str = readMaybeUA1 (splitsep (==',') str)

    readMaybeUA1 :: [String] -> Maybe UserAction
    readMaybeUA1 (a:b:c:[]) = case (x0,y0,c) of
        (Just x, Just y, ('c':[])) -> Just (UserAction(x,y,LeftClick))
        (Just x, Just y, ('f':[])) -> Just (UserAction(x,y,RightClick))
        (Nothing, _, _) -> Nothing
        (_, Nothing, _) -> Nothing
        (_, _, c) -> Nothing
        where
        x0 = (readMaybe a :: Maybe Int)
        y0 = (readMaybe b :: Maybe Int)
        readMaybeUA1 _ = Nothing

    splitsep sep [] = [[]]
    splitsep sep (h:t)
        | sep h = []: splitsep sep t
        | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t
