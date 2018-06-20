module Main exposing (..)

import Html exposing (Html, text, span, div, h1, h2, ul, li, strong, em)
import Html.Attributes exposing (class)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Json.Decode.Extra exposing (date)
import Json.Decode.Pipeline exposing (decode, required, optional, custom, hardcoded)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http
import Http exposing (Error)
import Date exposing (Date)
import Time exposing (Time, inSeconds)


---- MODEL ----


type alias Match =
    { location : String
    , venue : String
    , datetime : Date
    , status : Status
    , home_team : Team
    , away_team : Team
    , result : Result
    , home_team_events : List MatchEvent
    , away_team_events : List MatchEvent
    }


type alias MatchEvent =
    { match_event_id : Int
    , type_of_event : String
    , player : String
    , time : String
    , team : Team
    }


type Result
    = NoResult
    | Draw
    | Winner TeamCode


type Status
    = Future
    | InProgress
    | Completed


type alias TeamCode =
    String


type alias Team =
    { country : String
    , code : TeamCode
    , goals : Maybe Int
    }


type alias Model =
    { matchList : WebData (List Match) }


init : ( Model, Cmd Msg )
init =
    ( Model Loading, fetchMatches )



---- UPDATE ----


type Msg
    = HandleMatchesResponse (WebData (List Match))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleMatchesResponse matches ->
            ( { model | matchList = matches }
            , Cmd.none
            )


fetchMatches : Cmd Msg
fetchMatches =
    let
        url =
            --"/matches.json"
            "https://worldcup.sfg.io/matches"
    in
        RemoteData.Http.get url HandleMatchesResponse matchesDecoder



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "World Cup Sweepstake" ]
        , h2 [] [ text "Fastest Goal" ]
        , viewFastestGoal model.matchList
        , h2 [] [ text "Biggest Loss" ]
        , viewBiggestLoss model.matchList
        , h2 [] [ text "Match List" ]
        , viewMatchList model.matchList
        ]


viewMatchList : WebData (List Match) -> Html Msg
viewMatchList matchList =
    case matchList of
        NotAsked ->
            displayNotAsked

        Loading ->
            displayLoading

        Failure error ->
            displayError error

        Success matches ->
            let
                sorted_matches =
                    List.sortBy dateInSeconds matches
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayMatch sorted_matches
                    ]


viewFastestGoal : WebData (List Match) -> Html Msg
viewFastestGoal matchList =
    case matchList of
        NotAsked ->
            displayNotAsked

        Loading ->
            displayLoading

        Failure error ->
            displayError error

        Success matches ->
            let
                matches_with_goals =
                    matches
                        |> List.filterMap firstGoal
                        |> List.sortBy .time
                        |> List.take 10
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayMatchEvent matches_with_goals
                    ]


viewBiggestLoss : WebData (List Match) -> Html Msg
viewBiggestLoss matchList =
    case matchList of
        NotAsked ->
            displayNotAsked

        Loading ->
            displayLoading

        Failure error ->
            displayError error

        Success matches ->
            let
                loss_margins =
                    matches
                        |> List.filterMap lossMargin
                        |> List.sortBy Tuple.first
                        |> List.reverse
                        |> List.take 10
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayLossMargin loss_margins
                    ]


dateInSeconds : Match -> Float
dateInSeconds match =
    match.datetime
        |> Date.toTime
        |> Time.inSeconds


firstGoal : Match -> Maybe MatchEvent
firstGoal match =
    let
        all_match_events =
            match.home_team_events
                ++ match.away_team_events
    in
        all_match_events
            |> List.filter isGoalEvent
            |> List.sortBy .time
            |> List.head


isGoalEvent : MatchEvent -> Bool
isGoalEvent event =
    event.type_of_event == "goal"


lossMargin : Match -> Maybe ( Int, ( Team, Match ) )
lossMargin match =
    case ( match.home_team.goals, match.away_team.goals ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Nothing, _ ) ->
            Nothing

        ( _, Nothing ) ->
            Nothing

        ( Just home, Just away ) ->
            let
                loss =
                    home - away
            in
                if (loss > 0) then
                    Just ( loss, ( match.away_team, match ) )
                else
                    Just ( -loss, ( match.home_team, match ) )


displayMatch : Match -> Html Msg
displayMatch match =
    case match.status of
        Completed ->
            li [] [ displayCompleteMatch match ]

        InProgress ->
            li [] [ displayCompleteMatch match ]

        Future ->
            li [] [ displayFutureMatch match ]


displayCompleteMatch : Match -> Html Msg
displayCompleteMatch match =
    span []
        [ displayTeamName match.result match.home_team
        , text " "
        , displayGoals match.home_team.goals
        , text " - "
        , displayGoals match.away_team.goals
        , text " "
        , displayTeamName match.result match.away_team
        ]


displayGoals : Maybe Int -> Html Msg
displayGoals goals =
    case goals of
        Just goals_scored ->
            span []
                [ text (toString goals_scored) ]

        Nothing ->
            text ""


displayTeamName : Result -> Team -> Html Msg
displayTeamName result team =
    case result of
        NoResult ->
            span [] [ text team.country ]

        Draw ->
            span [ class "draw" ] [ text team.country ]

        Winner winning_team_code ->
            case winning_team_code == team.code of
                True ->
                    span [ class "winner" ] [ text team.country ]

                False ->
                    span [] [ text team.country ]


displayFutureMatch : Match -> Html Msg
displayFutureMatch match =
    span []
        [ span [] [ text match.home_team.country ]
        , span [] [ text " - " ]
        , span [] [ text match.away_team.country ]
        ]


keyedDisplayMatch : Match -> ( String, Html Msg )
keyedDisplayMatch match =
    ( toString match.datetime, lazy displayMatch match )


keyedDisplayMatchEvent : MatchEvent -> ( String, Html Msg )
keyedDisplayMatchEvent matchEvent =
    ( toString matchEvent.match_event_id, lazy displayMatchEvent matchEvent )


keyedDisplayLossMargin : ( Int, ( Team, Match ) ) -> ( String, Html Msg )
keyedDisplayLossMargin ( loss, ( loser, match ) ) =
    ( toString match.datetime, lazy displayLossMargin ( loss, ( loser, match ) ) )


displayLossMargin : ( Int, ( Team, Match ) ) -> Html Msg
displayLossMargin ( loss, ( loser, match ) ) =
    li []
        [ text (toString loss ++ " ")
        , text (loser.country ++ " ")
        , text " ["
        , displayCompleteMatch match
        , text "]"
        ]


displayMatchEvent : MatchEvent -> Html Msg
displayMatchEvent matchEvent =
    li []
        [ span [] [ text matchEvent.time ]
        , text " "
        , span [] [ text matchEvent.player ]
        , span [] [ text (" (" ++ matchEvent.team.country ++ ")") ]
        ]


displayError : Http.Error -> Html msg
displayError error =
    text ("Failed with error: " ++ toString error)


displayLoading : Html msg
displayLoading =
    text "Loading..."


displayNotAsked : Html msg
displayNotAsked =
    text "Not asked..."



---- DECODERS ----


matchesDecoder : Decode.Decoder (List Match)
matchesDecoder =
    Decode.list matchDecoder


matchDecoder : Decode.Decoder Match
matchDecoder =
    decode Match
        |> required "venue" Decode.string
        |> required "location" Decode.string
        |> required "datetime" Json.Decode.Extra.date
        |> required "status" (Decode.string |> Decode.andThen statusStringToStatusDecode)
        |> required "home_team" teamDecode
        |> required "away_team" teamDecode
        |> optional "winner_code" resultDecode NoResult
        |> custom
            (Decode.field "home_team" teamDecode
                |> Decode.andThen (\team -> Decode.field "home_team_events" (Decode.list (matchEventDecode team)))
            )
        |> custom
            (Decode.field "away_team" teamDecode
                |> Decode.andThen (\team -> Decode.field "away_team_events" (Decode.list (matchEventDecode team)))
            )


statusStringToStatusDecode : String -> Decode.Decoder Status
statusStringToStatusDecode str =
    case str of
        "future" ->
            Decode.succeed Future

        "in progress" ->
            Decode.succeed InProgress

        "completed" ->
            Decode.succeed Completed

        somethingElse ->
            Decode.fail <| "Unknown status: " ++ somethingElse


teamDecode : Decode.Decoder Team
teamDecode =
    decode Team
        |> required "country" Decode.string
        |> required "code" Decode.string
        |> optional "goals" (Decode.map Just Decode.int) Nothing


resultDecode : Decode.Decoder Result
resultDecode =
    Decode.string
        |> Decode.andThen resultStringToResultDecode


resultStringToResultDecode : String -> Decode.Decoder Result
resultStringToResultDecode str =
    case str of
        "Draw" ->
            Decode.succeed Draw

        teamCode ->
            Decode.succeed (Winner teamCode)


matchEventDecode : Team -> Decode.Decoder MatchEvent
matchEventDecode team =
    Decode.map5 MatchEvent
        (Decode.field "id" Decode.int)
        (Decode.field "type_of_event" Decode.string)
        (Decode.field "player" Decode.string)
        (Decode.field "time" Decode.string)
        (Decode.succeed team)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
