namespace TennisKata

open NUnit.Framework
open Tennis

[<TestFixture>]
type TennisTests() =
    let player1 = "player1"
    let player2 = "player2"

    let NamedGame points= Game {Player1Name=player1;Player2Name=player2} points

    let BuildScore (player1Points,player2Points) =
        let player1Points = List.replicate player1Points player1
        let player2Points = List.replicate player2Points player2
        List.append player1Points player2Points
        
    [<TestCase(0,0, Result="Love Love")>]
    member x.Should_score_be_Love_Love_when_no_points(player1,player2) =
        BuildScore (player1,player2) |> NamedGame |> Score

    [<TestCase(1,0, Result="Fifteen Love")>]
    [<TestCase(0,1, Result="Love Fifteen")>]
    member x.Should_score_be_Fifteen_when_one_points(player1,player2) =
        BuildScore (player1,player2) |> NamedGame |> Score
 
    [<TestCase(2,0, Result="Thirty Love")>]
    [<TestCase(0,2, Result="Love Thirty")>]
    member x.Should_score_be_Thirty_when_two_points(player1,player2) =
        BuildScore (player1,player2) |> NamedGame |> Score

    [<TestCase(3,0, Result="Fourty Love")>]
    [<TestCase(0,3, Result="Love Fourty")>]
    member x.Should_score_be_Fourty_when_three_points(player1,player2) =
        BuildScore (player1,player2) |> NamedGame |> Score

    [<TestCase(4,4, Result="Deuce")>]
    [<TestCase(5,5, Result="Deuce")>]
    member x.Should_score_be_Deuce_when_equality_and_score_at_least_4(player1,player2) =
        BuildScore (player1,player2) |> NamedGame |> Score

    [<TestCase(4,3, Result="Advantage player1")>]
    [<TestCase(5,4, Result="Advantage player1")>]
    [<TestCase(3,4, Result="Advantage player2")>]
    [<TestCase(4,5, Result="Advantage player2")>]
     member x.Should_score_be_Advantage_when_score_difference_is_1_and_a_player_scores_4_or_more(player1,player2) =
        BuildScore (player1,player2) |> NamedGame |> Score

    [<TestCase(4,2, Result="Win player1")>]
    [<TestCase(5,3, Result="Win player1")>]
    [<TestCase(2,4, Result="Win player2")>]
    [<TestCase(2,5, Result="Win player2")>]
     member x.Should_score_be_Win_when_score_difference_is_2_and_a_player_scores_4_or_more(player1,player2) =
        BuildScore (player1,player2) |> NamedGame |> Score
