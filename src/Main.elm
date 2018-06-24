module Main exposing (..)

import Html exposing (Html, text, span, div, h1, h2, ul, li, p, a, pre, strong)
import Html.Attributes exposing (href)
import Html.Keyed
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


type alias Population =
    { country : String
    , population : Int
    }


type alias TeamSummaryStats =
    { country : String
    , code : TeamCode
    , goals_for : Int
    }


type alias TeamSummaryStatsWithPopulation =
    { country : String
    , code : TeamCode
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
    }


type alias MatchEvent =
    { match_event_id : Int
    , type_of_event : String
    , player : String
    , time : String
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
    , events : Maybe (List MatchEvent)
    , stats : Maybe TeamMatchStats
    }


type alias TeamMatchStats =
    { goals : Int
    , yellow_cards : Int
    , red_cards : Int
    , points : Int
    }


type alias Model =
    { matchList : WebData (List Match)
    , populationList : WebData (List Population)
    , summaryList : WebData (List TeamSummaryStats)
    , mergedPopulationAndStats : WebData (List TeamSummaryStatsWithPopulation)
    }


init : ( Model, Cmd Msg )
init =
    ( Model Loading Loading Loading Loading, Cmd.batch [ fetchMatches, fetchPopulation, fetchSummary ] )



---- UPDATE ----


type Msg
    = HandleMatchesResponse (WebData (List Match))
    | HandlePopulationResponse (WebData (List Population))
    | HandleSummaryResponse (WebData (List TeamSummaryStats))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleMatchesResponse matches ->
            ( { model | matchList = matches }
            , Cmd.none
            )

        HandlePopulationResponse population ->
            let
                mergedPopulationAndStats =
                    attemptToMergePopulationAndSummaryStats population model.summaryList
            in
                ( { model | populationList = population, mergedPopulationAndStats = mergedPopulationAndStats }
                , Cmd.none
                )

        HandleSummaryResponse summary ->
            let
                mergedPopulationAndStats =
                    attemptToMergePopulationAndSummaryStats model.populationList summary
            in
                ( { model | summaryList = summary, mergedPopulationAndStats = mergedPopulationAndStats }
                , Cmd.none
                )


fetchMatches : Cmd Msg
fetchMatches =
    let
        url =
            --"%PUBLIC_URL%/matches.json"
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


attemptToMergePopulationAndSummaryStats : WebData (List Population) -> WebData (List TeamSummaryStats) -> WebData (List TeamSummaryStatsWithPopulation)
attemptToMergePopulationAndSummaryStats population stats =
    case ( population, stats ) of
        ( Success p, Success s ) ->
            Success (mergePopulationandStats p s)

        ( _, _ ) ->
            Loading


mergePopulationandStats : List Population -> List TeamSummaryStats -> List TeamSummaryStatsWithPopulation
mergePopulationandStats population stats =
    stats
        |> List.map
            (\team ->
                TeamSummaryStatsWithPopulation
                    team.country
                    team.code
                    team.goals_for
                    (populationbyCountry team.country population)
            )



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
        , viewGoalsPerCapita model.mergedPopulationAndStats
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
                        |> List.sortBy time
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
                        |> List.sortBy teamPoints
                        |> List.reverse
                        |> List.take 10
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayDirtyTeam card_count
                    ]


viewGoalsPerCapita : WebData (List TeamSummaryStatsWithPopulation) -> Html Msg
viewGoalsPerCapita summary =
    case summary of
        NotAsked ->
            displayNotAsked

        Loading ->
            displayLoading

        Failure error ->
            displayError error

        Success summary ->
            let
                summaries_with_population =
                    summary
                        |> List.map (\team -> calulateGoalsPerCapita team)
                        |> List.sortBy Tuple.first
                        |> List.reverse
                        |> List.take 10
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayPerCapita summaries_with_population
                    ]


calulateGoalsPerCapita : TeamSummaryStatsWithPopulation -> ( Float, TeamSummaryStatsWithPopulation )
calulateGoalsPerCapita stats =
    case stats.population of
        Nothing ->
            ( 0.0, stats )

        Just p ->
            ( (toFloat stats.goals_for) / (toFloat p) * 1000000, stats )


dateInSeconds : Match -> Float
dateInSeconds match =
    match.datetime
        |> Date.toTime
        |> Time.inSeconds


teamPoints : ( String, TeamMatchStats ) -> Int
teamPoints ( _, stats ) =
    stats.points


firstGoal : Match -> Maybe ( String, MatchEvent )
firstGoal match =
    let
        all_match_events =
            case ( match, match.home_team.events, match.away_team.events ) of
                ( match, Just h, Just a ) ->
                    (h
                        |> List.map (\event -> ( match.home_team.country, event ))
                    )
                        ++ (a
                                |> List.map (\event -> ( match.away_team.country, event ))
                           )

                ( _, _, _ ) ->
                    []
    in
        all_match_events
            |> List.filter isGoalEvent
            |> List.sortBy time
            |> List.head


time : ( String, MatchEvent ) -> String
time ( _, event ) =
    event.time


cardCount : Match -> Dict String TeamMatchStats -> Dict String TeamMatchStats
cardCount match store =
    let
        updates =
            [ ( match.home_team, match.home_team.stats ), ( match.away_team, match.away_team.stats ) ]
                |> List.map
                    (\( team, team_match_stats ) ->
                        case team_match_stats of
                            Nothing ->
                                ( team.country, Nothing )

                            Just match_stats ->
                                case Dict.get team.country store of
                                    Nothing ->
                                        ( team.country
                                        , Just
                                            (TeamMatchStats
                                                match_stats.goals
                                                match_stats.yellow_cards
                                                match_stats.red_cards
                                                match_stats.points
                                            )
                                        )

                                    Just stored_stats ->
                                        ( team.country
                                        , Just
                                            (TeamMatchStats
                                                (stored_stats.goals + match_stats.goals)
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


isGoalEvent : ( String, MatchEvent ) -> Bool
isGoalEvent ( _, event ) =
    event.type_of_event == "goal"


lossMargin : Match -> Maybe ( Int, ( Team, Match ) )
lossMargin match =
    case ( match.home_team.stats, match.away_team.stats ) of
        ( Just home_stats, Just away_stats ) ->
            let
                loss =
                    home_stats.goals - away_stats.goals
            in
                if (loss > 0) then
                    Just ( loss, ( match.away_team, match ) )
                else
                    Just ( -loss, ( match.home_team, match ) )

        ( _, _ ) ->
            Nothing


displayMatch : Match -> Html Msg
displayMatch match =
    case match.status of
        Completed ->
            li [] [ displayCompleteMatch match ]

        InProgress ->
            li []
                [ displayCompleteMatch match
                , text " (in progress)"
                ]

        Future ->
            li [] [ displayFutureMatch match ]


displayCompleteMatch : Match -> Html Msg
displayCompleteMatch match =
    let
        goal_text =
            case ( match.home_team.stats, match.away_team.stats ) of
                ( Just home_stats, Just away_stats ) ->
                    text
                        (toString home_stats.goals ++ " - " ++ toString away_stats.goals)

                ( _, _ ) ->
                    text ""
    in
        span []
            [ displayTeamName match.result match.home_team
            , text " "
            , goal_text
            , text " "
            , displayTeamName match.result match.away_team
            ]


displayGoals : Maybe Int -> Html Msg
displayGoals goals =
    case goals of
        Just goals_scored ->
            text (toString goals_scored)

        Nothing ->
            text ""


displayTeamName : Result -> Team -> Html Msg
displayTeamName result team =
    case result of
        NoResult ->
            text team.country

        Draw ->
            strong [] [ text team.country ]

        Winner winning_team_code ->
            case winning_team_code == team.code of
                True ->
                    strong [] [ text team.country ]

                False ->
                    text team.country


displayFutureMatch : Match -> Html Msg
displayFutureMatch match =
    text (match.home_team.country ++ " - " ++ match.away_team.country)


keyedDisplayMatch : Match -> ( String, Html Msg )
keyedDisplayMatch match =
    ( toString match.datetime, displayMatch match )


keyedDisplayMatchEvent : ( String, MatchEvent ) -> ( String, Html Msg )
keyedDisplayMatchEvent ( country, matchEvent ) =
    ( toString matchEvent.match_event_id, (displayMatchEvent country matchEvent) )


keyedDisplayLossMargin : ( Int, ( Team, Match ) ) -> ( String, Html Msg )
keyedDisplayLossMargin ( loss, ( loser, match ) ) =
    ( toString match.datetime, displayLossMargin ( loss, ( loser, match ) ) )


displayLossMargin : ( Int, ( Team, Match ) ) -> Html Msg
displayLossMargin ( loss, ( loser, match ) ) =
    li []
        [ text (toString loss ++ " ")
        , text (loser.country ++ " ")
        , text " ["
        , displayCompleteMatch match
        , text "]"
        ]


keyedDisplayPerCapita : ( Float, TeamSummaryStatsWithPopulation ) -> ( String, Html Msg )
keyedDisplayPerCapita ( per_capita, stats ) =
    ( stats.country, displayPerCapita ( per_capita, stats ) )


displayPerCapita : ( Float, TeamSummaryStatsWithPopulation ) -> Html Msg
displayPerCapita ( per_capita, stats ) =
    let
        displayPopulation =
            case stats.population of
                Nothing ->
                    text "population: unknown)"

                Just p ->
                    text ("population: " ++ toString p ++ ")")
    in
        li []
            [ text ((Round.round 2 per_capita) ++ " ")
            , text (stats.country ++ " ")
            , text ("(goals: " ++ toString stats.goals_for ++ ", ")
            , displayPopulation
            ]


keyedDisplayDirtyTeam : ( String, TeamMatchStats ) -> ( String, Html Msg )
keyedDisplayDirtyTeam ( country, stats ) =
    ( country, displayDirtyTeam ( country, stats ) )


displayDirtyTeam : ( String, TeamMatchStats ) -> Html Msg
displayDirtyTeam ( country, stats ) =
    li []
        [ text (toString stats.points)
        , text " "
        , text country
        , text (" (reds: " ++ toString stats.red_cards ++ ", yellows: " ++ toString stats.yellow_cards ++ ")")
        ]


displayMatchEvent : String -> MatchEvent -> Html Msg
displayMatchEvent country matchEvent =
    li []
        [ span [] [ text matchEvent.time ]
        , text " "
        , span [] [ text matchEvent.player ]
        , span [] [ text (" (" ++ country ++ ")") ]
        ]


displayError : Http.Error -> Html msg
displayError error =
    div []
        [ p []
            [ text "Failed with error: " ]
        , pre []
            [ text (toString error) ]
        ]


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
        |> required "location" Decode.string
        |> required "venue" Decode.string
        |> required "datetime" Json.Decode.Extra.date
        |> required "status" (Decode.string |> Decode.andThen statusStringToStatusDecode)
        |> custom
            (teamMonster "home_team" "home_team_events" "home_team_statistics")
        |> custom
            (teamMonster "away_team" "away_team_events" "away_team_statistics")
        |> optional "winner_code" resultDecode NoResult


teamMonster : String -> String -> String -> Decode.Decoder Team
teamMonster team_field events_field stats_field =
    Decode.maybe (Decode.at [ team_field, "goals" ] Decode.int)
        |> Decode.andThen
            (\goals ->
                Decode.field events_field (Decode.nullable (Decode.list matchEventDecode))
                    |> Decode.andThen
                        (\events ->
                            Decode.field stats_field (Decode.nullable (statsDecode goals))
                                |> Decode.andThen
                                    (\stats ->
                                        Decode.field team_field (teamDecode events stats)
                                    )
                        )
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


statsDecode : Maybe Int -> Decode.Decoder TeamMatchStats
statsDecode goals =
    let
        g =
            Maybe.withDefault 0 goals
    in
        decode TeamMatchStats
            |> hardcoded g
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


teamDecode : Maybe (List MatchEvent) -> Maybe TeamMatchStats -> Decode.Decoder Team
teamDecode events stats =
    decode Team
        |> required "country" Decode.string
        |> required "code" Decode.string
        |> hardcoded events
        |> hardcoded stats


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


matchEventDecode : Decode.Decoder MatchEvent
matchEventDecode =
    Decode.map4 MatchEvent
        (Decode.field "id" Decode.int)
        (Decode.field "type_of_event" Decode.string)
        (Decode.field "player" Decode.string)
        (Decode.field "time" Decode.string)


populationsDecode : Decode.Decoder (List Population)
populationsDecode =
    Decode.list populationDecode


populationDecode : Decode.Decoder Population
populationDecode =
    Decode.map2 Population
        (Decode.field "Country" Decode.string)
        (Decode.field "Population" Decode.int)


summariesDecode : Decode.Decoder (List TeamSummaryStats)
summariesDecode =
    Decode.list summaryDecode


summaryDecode : Decode.Decoder TeamSummaryStats
summaryDecode =
    Decode.map3 TeamSummaryStats
        (Decode.field "country" Decode.string)
        (Decode.field "fifa_code" Decode.string)
        (Decode.field "goals_for" Decode.int)


populationbyCountry : String -> List Population -> Maybe Int
populationbyCountry country list =
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
