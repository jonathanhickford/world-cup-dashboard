module Main exposing (..)

import Html exposing (Html, text, span, div, h1, h2, ul, li, strong, em, p, a)
import Html.Attributes exposing (class, href)
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
import Dict exposing (Dict)
import Round


---- MODEL ----


type alias Populaton =
    { country : String
    , population : Int
    }


type alias TeamSummaryStats =
    { country : String
    , goals_for : Int
    , population : Maybe Int
    }


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
    , home_team_stats : Maybe TeamStats
    , away_team_stats : Maybe TeamStats
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


type alias TeamStats =
    { yellow_cards : Int
    , red_cards : Int
    , points : Int
    }


type alias Model =
    { matchList : WebData (List Match)
    , populationList : WebData (List Populaton)
    , summaryList : WebData (List TeamSummaryStats)
    }


init : ( Model, Cmd Msg )
init =
    ( Model Loading Loading Loading, Cmd.batch [ fetchMatches, fetchPopulation, fetchSummary ] )



---- UPDATE ----


type Msg
    = HandleMatchesResponse (WebData (List Match))
    | HandlePopulationResponse (WebData (List Populaton))
    | HandleSummaryResponse (WebData (List TeamSummaryStats))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleMatchesResponse matches ->
            ( { model | matchList = matches }
            , Cmd.none
            )

        HandlePopulationResponse population ->
            ( { model | populationList = population }
            , Cmd.none
            )

        HandleSummaryResponse summary ->
            ( { model | summaryList = summary }
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


fetchSummary : Cmd Msg
fetchSummary =
    let
        url =
            --"/matches.json"
            "https://worldcup.sfg.io/teams/results"
    in
        RemoteData.Http.get url HandleSummaryResponse summariesDecode


fetchPopulation : Cmd Msg
fetchPopulation =
    let
        url =
            "%PUBLIC_URL%/population.json"
    in
        RemoteData.Http.get url HandlePopulationResponse populationsDecode



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "World Cup Sweepstake" ]
        , h2 [] [ text "Fastest Goal" ]
        , viewFastestGoal model.matchList
        , h2 [] [ text "Biggest Loss" ]
        , viewBiggestLoss model.matchList
        , h2 [] [ text "Dirtiest Team" ]
        , viewDirtiestTeam model.matchList
        , h2 [] [ text "Goals per Capita (Millions)" ]
        , p []
            [ text "Sources: "
            , a [ href "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)" ]
                [ text "Wikipedia UN data"
                ]
            , text " and "
            , a [ href "https://en.wikipedia.org/wiki/Demography_of_the_United_Kingdom#Population" ]
                [ text "Wikipedia UK/England data" ]
            ]
        , viewGoalsPerCapita model.summaryList model.populationList
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


viewDirtiestTeam : WebData (List Match) -> Html Msg
viewDirtiestTeam matchList =
    case matchList of
        NotAsked ->
            displayNotAsked

        Loading ->
            displayLoading

        Failure error ->
            displayError error

        Success matches ->
            let
                card_count =
                    matches
                        |> List.foldl cardCount Dict.empty
                        |> Dict.toList
                        |> List.sortBy sortByPoints
                        |> List.reverse
                        |> List.take 10
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayDirtyTeam card_count
                    ]


viewGoalsPerCapita : WebData (List TeamSummaryStats) -> WebData (List Populaton) -> Html Msg
viewGoalsPerCapita summary population =
    case ( summary, population ) of
        ( NotAsked, _ ) ->
            displayNotAsked

        ( _, NotAsked ) ->
            displayNotAsked

        ( Loading, _ ) ->
            displayLoading

        ( _, Loading ) ->
            displayLoading

        ( Failure error1, Failure error2 ) ->
            displayErrors error1 error2

        ( Failure error, _ ) ->
            displayError error

        ( _, Failure error ) ->
            displayError error

        ( Success summary, Success population ) ->
            let
                summaries_with_population =
                    summary
                        |> List.map (\team -> { team | population = (findbyCountry team.country population) })
                        |> List.map (\team -> calulateGoalsPerCapita team)
                        |> List.sort
                        |> List.reverse
                        |> List.take 10
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayPerCapita summaries_with_population
                    ]


calulateGoalsPerCapita : TeamSummaryStats -> ( Float, Int, Int, String )
calulateGoalsPerCapita stats =
    case stats.population of
        Nothing ->
            ( 0.0, 0, stats.goals_for, stats.country )

        Just p ->
            ( (toFloat stats.goals_for) / (toFloat p) * 1000000, p, stats.goals_for, stats.country )


dateInSeconds : Match -> Float
dateInSeconds match =
    match.datetime
        |> Date.toTime
        |> Time.inSeconds


sortByPoints : ( String, TeamStats ) -> Int
sortByPoints ( country, stats ) =
    stats.points


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


cardCount : Match -> Dict String TeamStats -> Dict String TeamStats
cardCount match store =
    let
        updates =
            [ ( match.home_team, match.home_team_stats ), ( match.away_team, match.away_team_stats ) ]
                |> List.map
                    (\( team, team_match_stats ) ->
                        case team_match_stats of
                            Nothing ->
                                ( team.country, Nothing )

                            Just match_stats ->
                                case Dict.get team.country store of
                                    Nothing ->
                                        ( team.country
                                        , Just (TeamStats match_stats.yellow_cards match_stats.red_cards match_stats.points)
                                        )

                                    Just stored_stats ->
                                        ( team.country
                                        , Just
                                            (TeamStats
                                                (stored_stats.yellow_cards + match_stats.yellow_cards)
                                                (stored_stats.red_cards + match_stats.red_cards)
                                                (stored_stats.points + match_stats.points)
                                            )
                                        )
                    )

        update_action item store =
            case Tuple.second item of
                Nothing ->
                    store

                Just stats ->
                    Dict.insert (Tuple.first item) stats store
    in
        updates
            |> List.foldl update_action store


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
            li []
                [ displayCompleteMatch match
                , span [] [ text " (in progress)" ]
                ]

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


keyedDisplayPerCapita : ( Float, Int, Int, String ) -> ( String, Html Msg )
keyedDisplayPerCapita ( per_capita, population, goals, country ) =
    ( country, lazy displayPerCapita ( per_capita, population, goals, country ) )


displayPerCapita : ( Float, Int, Int, String ) -> Html Msg
displayPerCapita ( per_capita, population, goals, country ) =
    li []
        [ text ((Round.round 2 per_capita) ++ " ")
        , text (country ++ " ")
        , text ("(goals: " ++ toString goals ++ ", ")
        , text ("population: " ++ toString population ++ ")")
        ]


keyedDisplayDirtyTeam : ( String, TeamStats ) -> ( String, Html Msg )
keyedDisplayDirtyTeam ( country, stats ) =
    ( toString country, lazy displayDirtyTeam ( country, stats ) )


displayDirtyTeam : ( String, TeamStats ) -> Html Msg
displayDirtyTeam ( country, stats ) =
    li []
        [ text (toString stats.points)
        , text " "
        , text country
        , text (" (reds: " ++ toString stats.red_cards ++ ", yellows: " ++ toString stats.yellow_cards ++ ")")
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


displayErrors : Http.Error -> Http.Error -> Html msg
displayErrors error1 error2 =
    text ("Failed with errors: " ++ (toString error1) ++ (toString error2))


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
        |> required "home_team_statistics" (Decode.nullable statsDecode)
        |> required "away_team_statistics" (Decode.nullable statsDecode)


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


statsDecode : Decode.Decoder TeamStats
statsDecode =
    decode TeamStats
        |> required "yellow_cards" Decode.int
        |> required "red_cards" Decode.int
        |> custom
            (Decode.field "yellow_cards" Decode.int
                |> Decode.andThen
                    (\yellow ->
                        Decode.field "red_cards" Decode.int
                            |> Decode.andThen
                                (\red ->
                                    Decode.succeed (2 * red + yellow)
                                )
                    )
            )


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


populationsDecode : Decode.Decoder (List Populaton)
populationsDecode =
    Decode.list populationDecode


populationDecode : Decode.Decoder Populaton
populationDecode =
    Decode.map2 Populaton
        (Decode.field "Country" Decode.string)
        (Decode.field "Population" Decode.int)


summariesDecode : Decode.Decoder (List TeamSummaryStats)
summariesDecode =
    Decode.list summaryDecode


summaryDecode : Decode.Decoder TeamSummaryStats
summaryDecode =
    Decode.map3 TeamSummaryStats
        (Decode.field "country" Decode.string)
        (Decode.field "goals_for" Decode.int)
        (Decode.succeed Nothing)


findbyCountry : String -> List Populaton -> Maybe Int
findbyCountry country list =
    let
        population =
            list
                |> List.filter (\item -> item.country == country)
                |> List.head
    in
        case population of
            Nothing ->
                Nothing

            Just population ->
                Just population.population



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
