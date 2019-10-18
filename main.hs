module Main(main) where

    import Minesweeper
    import Text.Read
    
    type TournamentState = (Int, Int)
    
    
    main = do
        putStrLn " MINESWEEPER"
        (s,mines) <- minedistirbutor
        playInit s mines (0,0)
    
    play :: State -> Int -> Int -> TournamentState -> IO TournamentState
    play st size mines tourn = do
        printGrid st
        putStrLn ("  Mines left: " ++ show mines)
        UserAction (x,y,c) <- readUA size
        case c of
            LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
            RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
        let newMines = if c == RightClick then (mines-1) else mines
        let res = minesweeper (UserAction (x,y,c)) st
        case res of
            EndOfGame val st -> (playAgain st val tourn)
            ContinueGame st -> (play st size newMines tourn)
    
    --1st launch of player action       
    playInit size mines tourn = do
        putStrLn ("  Mines left: " ++ show mines)
        UserAction (x,y,c) <- readUA size
        grid <- makeGridUA size mines (UserAction (x,y,c))
        case c of
            LeftClick -> putStrLn ("Checking for a mine at " ++ show x ++ " and " ++ show y)
            RightClick -> putStrLn ("Flagging space at " ++ show x ++ " and " ++ show y)
        let newMines = if c == RightClick then (mines-1) else mines
        let res = minesweeper (UserAction (x,y,c)) grid
        case res of
            EndOfGame val st -> (playAgain st val tourn)
            ContinueGame st -> (play st size newMines tourn)

    playAgain :: State -> Double -> TournamentState -> IO TournamentState
    playAgain grid val (wins,losses) = do
        printGrid grid
        case val of
            1 -> putStrLn ("You win!")
            0 -> putStrLn ("You lose!")
        let newTourn = if val == 1 then (wins+1, losses) else (wins, losses+1)
        return newTourn
    
    chooseSquare :: Int -> IO UserAction
    chooseSquare size =
        do
            putStrLn ("  Please choose a square by inputting an x coordinate, a y coordinate and click or flag.")
            line <- getLine
            let ua = readMaybeUA line
            case ua of
                Nothing -> redo
                Just (UserAction (x,y,c)) -> if (x > size) || (y > size) || (x < 0) || (y < 0) then redo else return (UserAction (x,y,c))
              where redo = do
                            putStrLn ("  Please enter a valid coordinate and command!")
                            chooseSquare size
                            
    