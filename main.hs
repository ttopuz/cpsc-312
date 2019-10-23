module Main(main) where

    import Minesweeper
    import System.Random
    import Text.Read
    
    main :: IO()
    main = do
       putStrLn "Wellcome to the minesweeper!"
       --newgame_ask
       (diff, size) <- dif_ask
       playInit diff size
    
    playInit diff size = do
        UserAction (x,y,c) = readUA
        grid <- makeDistributor size size diff 20
        case c of
            LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
            RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
        let res = minesweeper (UserAction (x,y,c)) grid
        case res of
        EndOfGame val st -> (playAgain st val)
        ContinueGame st -> (play st diff size)

    play grid diff size = do
        printGrid grid
        UserAction (x,y,c) = readUA size
        grid <- makeDistributor size size diff 20
        case c of
            LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
            RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
        let res = minesweeper (UserAction (x,y,c)) grid
        case res of
        EndOfGame val st -> (playAgain st val)
        ContinueGame st -> (play st diff size)
    

    playAgain grid val = do
    printGrid grid
    case val of
        1 -> putStrLn ("You win!")
        0 -> putStrLn ("You lose!")
    
    dif_ask = do
        putStrLn "Please select a difficulty. \nEasy pizzy = 1\nMedium = 2\nHard = 3\nHardcore = 4"
        diffsetter <- getLine 
        case diffsetter of
            1 -> size = 9
            2 -> size = 8
            3 -> size = 7
            4 -> size = 6
        return (diffsetter, size)
    
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
