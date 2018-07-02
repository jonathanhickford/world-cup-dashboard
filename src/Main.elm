module Main exposing (..)

import Html exposing (Html, text, span, div, h1, h2, h3, ul, li, p, a, pre, strong, em)
import Html.Attributes exposing (href, id)
import Html.Events exposing (onClick)
import Html.Events.Extra exposing (onClickPreventDefault)
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
import List.Extra
import DateFormat
import Char
import Ports


---- MODEL ----


type alias Population =
    { country : String
    , population : Int
    }


type alias TeamSummaryStats =
    { country : String
    , code : TeamCode
    , goals_for : Int
    , group : Group
    }


type Group
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type alias TeamSummaryStatsWithPopulation =
    { country : String
    , code : TeamCode
    , goals_for : Int
    , population : Maybe Int
    }


type alias Match =
    { location : String
    , venue : String
    , stage_name : String
    , datetime : Date
    , status : Status
    , home_team : Team
    , away_team : Team
    , result : MatchResult
    }


type alias MatchEvent =
    { match_event_id : Int
    , type_of_event : String
    , player : String
    , time : Int
    }


type MatchResult
    = NoResult
    | Draw
    | Winner TeamCode


type Status
    = Future
    | InProgress
    | Completed
    | PendingCorrection


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
    , penalties : Int
    , yellow_cards : Int
    , red_cards : Int
    , points : Int
    }


type alias Model =
    { matchList : WebData (List Match)
    , populationList : WebData (List Population)
    , summaryList : WebData (List TeamSummaryStats)
    , mergedPopulationAndStats : WebData (List TeamSummaryStatsWithPopulation)
    , showGroupMatches : Bool
    , showAll : Bool
    , newServiceWorkerAvailable : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model Loading Loading Loading Loading False False False, Cmd.batch [ fetchMatches, fetchPopulation, fetchSummary ] )



---- UPDATE ----


type Msg
    = HandleMatchesResponse (WebData (List Match))
    | HandlePopulationResponse (WebData (List Population))
    | HandleSummaryResponse (WebData (List TeamSummaryStats))
    | ToggleShowGroupMatches
    | ToggleShowAll String
    | NewServiceWorkerAvailable Bool
    | RefreshPage


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

        ToggleShowGroupMatches ->
            ( { model | showGroupMatches = not model.showGroupMatches }, Cmd.none )

        ToggleShowAll id ->
            ( { model | showAll = not model.showAll }, Ports.scrollIntoView id )

        NewServiceWorkerAvailable boolean_value ->
            ( { model | newServiceWorkerAvailable = boolean_value }, Cmd.none )

        RefreshPage ->
            ( model, Ports.refreshPage () )


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


displayNewServiceWorkerAvailable : Bool -> Html Msg
displayNewServiceWorkerAvailable userShouldReload =
    case userShouldReload of
        True ->
            Html.button [ onClick RefreshPage ] [ text "A new version is avaliable, please refresh this page" ]

        False ->
            text ""


view : Model -> Html Msg
view model =
    div []
        [ displayNewServiceWorkerAvailable model.newServiceWorkerAvailable
        , h1 [] [ text "World Cup Sweepstake" ]
        , h2 [ id "fastest_goal" ] [ text "Fastest Goal" ]
        , viewFastestGoal model.matchList model.showAll
        , h2 [ id "biggest_loss" ] [ text "Biggest Loss" ]
        , viewBiggestLoss model.matchList model.showAll
        , h2 [ id "dirtiest_team" ] [ text "Dirtiest Team" ]
        , viewDirtiestTeam model.matchList model.showAll
        , h2 [ id "goals_per_capita" ] [ text "Goals per Capita (Millions)" ]
        , p []
            [ text "Sources: "
            , a [ href "https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)" ]
                [ text "Wikipedia UN data"
                ]
            , text " and "
            , a [ href "https://en.wikipedia.org/wiki/Demography_of_the_United_Kingdom#Population" ]
                [ text "Wikipedia UK/England data" ]
            ]
        , viewGoalsPerCapita model.mergedPopulationAndStats model.showAll
        , h2 [ id "match_list" ] [ text "Match List" ]
        , viewMatchList model.matchList model.showGroupMatches
        ]


viewMatchList : WebData (List Match) -> Bool -> Html Msg
viewMatchList matchList showGroupMatches =
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
                    case showGroupMatches of
                        True ->
                            List.sortBy dateInSeconds matches

                        False ->
                            List.sortBy dateInSeconds matches
                                |> List.filter (\match -> match.stage_name /= "First stage")

                grouped_matches =
                    List.Extra.groupWhile sameDateDays sorted_matches

                show_hide_label =
                    case showGroupMatches of
                        True ->
                            "[–] Hide Group Matches"

                        False ->
                            "[+] Show Group Matches"
            in
                div []
                    [ p []
                        [ a [ onClickPreventDefault ToggleShowGroupMatches, href "#" ] [ text show_hide_label ]
                        ]
                    , Html.div
                        []
                      <|
                        (grouped_matches
                            |> List.map
                                (\days_matches ->
                                    let
                                        first_match =
                                            List.head days_matches

                                        first_match_date =
                                            case first_match of
                                                Nothing ->
                                                    "Nothing"

                                                Just m ->
                                                    dateFormatter m.datetime
                                    in
                                        div []
                                            [ p [] [ text first_match_date ]
                                            , Html.Keyed.ul [] <|
                                                List.map keyedDisplayMatch days_matches
                                            ]
                                )
                        )
                    ]



{-
   Html.Keyed.ul [] <|
       List.map keyedDisplayMatch days_matches
-}


takeTenIfLimited : Bool -> List a -> List a
takeTenIfLimited showAll list =
    case showAll of
        True ->
            list

        False ->
            list |> List.take 10


viewFastestGoal : WebData (List Match) -> Bool -> Html Msg
viewFastestGoal matchList showAll =
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
                        |> takeTenIfLimited showAll

                show_hide_link =
                    showHideLink showAll "fastest_goal"
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayMatchEvent matches_with_goals
                    , p [] [ show_hide_link ]
                    ]


viewBiggestLoss : WebData (List Match) -> Bool -> Html Msg
viewBiggestLoss matchList showAll =
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
                        |> takeTenIfLimited showAll

                show_hide_link =
                    showHideLink showAll "biggest_loss"
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayLossMargin loss_margins
                    , p [] [ show_hide_link ]
                    ]


viewDirtiestTeam : WebData (List Match) -> Bool -> Html Msg
viewDirtiestTeam matchList showAll =
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
                        |> takeTenIfLimited showAll

                show_hide_link =
                    showHideLink showAll "dirtiest_team"
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayDirtyTeam card_count
                    , p [] [ show_hide_link ]
                    ]


viewGoalsPerCapita : WebData (List TeamSummaryStatsWithPopulation) -> Bool -> Html Msg
viewGoalsPerCapita summary showAll =
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
                        |> takeTenIfLimited showAll

                show_hide_link =
                    showHideLink showAll "goals_per_capita"
            in
                div []
                    [ Html.Keyed.ul [] <|
                        List.map keyedDisplayPerCapita summaries_with_population
                    , p [] [ show_hide_link ]
                    ]


showHideLink : Bool -> String -> Html Msg
showHideLink showAll id =
    case showAll of
        True ->
            a [ onClick (ToggleShowAll id), href ("#" ++ id) ] [ text "[–] Show top 10" ]

        False ->
            a [ onClickPreventDefault (ToggleShowAll id), href "#" ] [ text "[+] Show full list" ]


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


dateDay : Match -> Int
dateDay match =
    match.datetime
        |> Date.day


sameDateDays : Match -> Match -> Bool
sameDateDays a b =
    (dateDay a) == (dateDay b)


dateFormatter : Date -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.dayOfWeekNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text " "
        , DateFormat.monthNameFull
        ]


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


time : ( a, MatchEvent ) -> Int
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
                                                match_stats.penalties
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
                                                (stored_stats.penalties + match_stats.penalties)
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


isGoalEvent : ( a, MatchEvent ) -> Bool
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

        PendingCorrection ->
            li []
                [ displayCompleteMatch match
                , text " (pending correction)"
                ]

        Future ->
            li [] [ displayFutureMatch match ]


displayCompleteMatch : Match -> Html Msg
displayCompleteMatch match =
    let
        goal_text =
            case ( match.home_team.stats, match.away_team.stats ) of
                ( Just home_stats, Just away_stats ) ->
                    let
                        draw =
                            home_stats.goals == away_stats.goals

                        some_penalties =
                            (home_stats.penalties > 0) || (away_stats.penalties > 0)
                    in
                        case ( draw, some_penalties ) of
                            ( True, True ) ->
                                text
                                    (toString home_stats.goals
                                        ++ " ("
                                        ++ toString home_stats.penalties
                                        ++ ") - ("
                                        ++ toString away_stats.penalties
                                        ++ ") "
                                        ++ toString away_stats.goals
                                    )

                            ( _, _ ) ->
                                text (toString home_stats.goals ++ " - " ++ toString away_stats.goals)

                ( _, _ ) ->
                    text ""
    in
        span []
            [ em [] [ text match.stage_name ]
            , text ": "
            , displayTeamName match.result match.home_team
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


displayTeamName : MatchResult -> Team -> Html Msg
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
    span []
        [ em [] [ text match.stage_name ]
        , text
            (": " ++ match.home_team.country ++ " - " ++ match.away_team.country)
        ]


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
        [ span [] [ text (toString matchEvent.time) ]
        , text "' "
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
        |> required "stage_name" Decode.string
        |> required "datetime" Json.Decode.Extra.date
        |> required "status" (Decode.string |> Decode.andThen statusStringToStatusDecode)
        |> custom
            (teamMonster { team_field = "home_team", events_field = "home_team_events", stats_field = "home_team_statistics" })
        |> custom
            (teamMonster { team_field = "away_team", events_field = "away_team_events", stats_field = "away_team_statistics" })
        |> optional "winner_code" resultDecode NoResult


teamMonster : { team_field : String, events_field : String, stats_field : String } -> Decode.Decoder Team
teamMonster { team_field, events_field, stats_field } =
    Decode.maybe (Decode.at [ team_field, "goals" ] Decode.int)
        |> Decode.andThen
            (\goals ->
                Decode.maybe (Decode.at [ team_field, "penalties" ] Decode.int)
                    |> Decode.andThen
                        (\penalties ->
                            Decode.maybe (Decode.field events_field (Decode.list matchEventDecode))
                                |> Decode.andThen
                                    (\events ->
                                        Decode.maybe (Decode.field stats_field (statsDecode goals penalties))
                                            |> Decode.andThen
                                                (\stats ->
                                                    Decode.field team_field (teamDecode events stats)
                                                )
                                    )
                        )
            )


statusStringToStatusDecode : String -> Decode.Decoder Status
statusStringToStatusDecode str =
    case (String.toLower str) of
        "future" ->
            Decode.succeed Future

        "in progress" ->
            Decode.succeed InProgress

        "completed" ->
            Decode.succeed Completed

        "pending_correction" ->
            Decode.succeed PendingCorrection

        somethingElse ->
            Decode.fail <| "Unknown status: " ++ somethingElse


groupStringtoGroup : String -> Decode.Decoder Group
groupStringtoGroup str =
    case (String.toLower str) of
        "a" ->
            Decode.succeed A

        "b" ->
            Decode.succeed B

        "c" ->
            Decode.succeed C

        "d" ->
            Decode.succeed D

        "e" ->
            Decode.succeed E

        "f" ->
            Decode.succeed F

        "g" ->
            Decode.succeed G

        "h" ->
            Decode.succeed H

        somethingElse ->
            Decode.fail <| "Unknown group: " ++ somethingElse


statsDecode : Maybe Int -> Maybe Int -> Decode.Decoder TeamMatchStats
statsDecode goals penalties =
    let
        g =
            Maybe.withDefault 0 goals

        p =
            Maybe.withDefault 0 penalties

        reds =
            Decode.field "red_cards" Decode.int

        yellows =
            Decode.field "yellow_cards" Decode.int
    in
        Decode.map5 TeamMatchStats
            (Decode.succeed g)
            (Decode.succeed p)
            yellows
            reds
            (Decode.map2 pointsFromCards yellows reds)


pointsFromCards : Int -> Int -> Int
pointsFromCards yellows reds =
    2 * reds + yellows


teamDecode : Maybe (List MatchEvent) -> Maybe TeamMatchStats -> Decode.Decoder Team
teamDecode events stats =
    Decode.map4 Team
        (Decode.field "country" Decode.string)
        (Decode.field "code" Decode.string)
        (Decode.succeed events)
        (Decode.succeed stats)


resultDecode : Decode.Decoder MatchResult
resultDecode =
    Decode.string
        |> Decode.andThen resultStringToResultDecode


resultStringToResultDecode : String -> Decode.Decoder MatchResult
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
        (Decode.field "time" Decode.string |> Decode.andThen timeStringToInt)


timeStringToInt : String -> Decode.Decoder Int
timeStringToInt str =
    let
        time =
            String.split "+" str
                |> List.map (String.filter Char.isDigit)
                |> List.map String.toInt
                |> List.map (Result.withDefault 0)
                |> List.sum
    in
        Decode.succeed time


populationsDecode : Decode.Decoder (List Population)
populationsDecode =
    Decode.list
        (Decode.map2
            Population
            (Decode.field "Country" Decode.string)
            (Decode.field "Population" Decode.int)
        )


summariesDecode : Decode.Decoder (List TeamSummaryStats)
summariesDecode =
    Decode.list
        (Decode.map4 TeamSummaryStats
            (Decode.field "country" Decode.string)
            (Decode.field "fifa_code" Decode.string)
            (Decode.field "goals_for" Decode.int)
            (Decode.field "group_letter" (Decode.string |> Decode.andThen groupStringtoGroup))
        )


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


groupbyCountry : String -> List TeamSummaryStats -> Maybe Group
groupbyCountry country list =
    let
        group =
            list
                |> List.filter (\item -> item.country == country)
                |> List.head
    in
        case group of
            Nothing ->
                Nothing

            Just group ->
                Just group.group



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.newServiceWorkerAvailable NewServiceWorkerAvailable



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
