namespace TennisKata

module Tennis =
    type Names = {Player1Name : string; Player2Name : string}
    type Points = {Player1Score:int; Player2Score:int}
    type GameStatus = {Names:Names;Points:Points}

    let EmptyGame names = { Names=names; Points={Player1Score = 0; Player2Score = 0}; }

    let AddPointToPlayer1 game = { Names = game.Names; Points = {Player1Score = game.Points.Player1Score + 1; Player2Score = game.Points.Player2Score} }
    let AddPointToPlayer2 game = { Names = game.Names; Points = {Player1Score = game.Points.Player1Score; Player2Score = game.Points.Player2Score + 1} }

    let CountPoint game player =
        let names = game.Names
        match player with
        | x when x = names.Player1Name -> AddPointToPlayer1 game
        | x when x = names.Player2Name -> AddPointToPlayer2 game
        
    let Game names points = 
        let game = EmptyGame names
        List.fold CountPoint game points

    let PlayerScore score =
        match score with
        | 0 -> "Love"
        | 1 -> "Fifteen"
        | 2 -> "Thirty"
        | 3 -> "Fourty"

    let (|Win|_|) gameStatus =
        let points = gameStatus.Points
        if System.Math.Abs(points.Player1Score - points.Player2Score) > 1 &&
           System.Math.Max(points.Player1Score, points.Player2Score) > 3 then
            Some(Win)
        else
            None 

    let (|Advantage|_|) gameStatus =
        let points = gameStatus.Points
        if System.Math.Abs(points.Player1Score - points.Player2Score) = 1 &&
           System.Math.Max(points.Player1Score, points.Player2Score) > 3 then
            Some(Advantage)
        else
            None 

    let (|Deuce|_|) gameStatus =
        let points = gameStatus.Points
        if points.Player1Score = points.Player2Score && points.Player1Score > 3 then
            Some(Deuce)
        else
            None

    let GetLeaderName gameStatus =
       let points = gameStatus.Points
       if points.Player1Score > points.Player2Score then
            gameStatus.Names.Player1Name
        else
            gameStatus.Names.Player2Name

    let Score game = 
        match game with
        | Win -> "Win " + GetLeaderName game
        | Advantage -> "Advantage " + GetLeaderName game
        | Deuce -> "Deuce"
        | _ -> PlayerScore game.Points.Player1Score + " " + PlayerScore game.Points.Player2Score
