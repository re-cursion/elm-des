module Demo exposing (main)

import Browser
import Des.Scenario as Scenario
import Html exposing (Html, div, h2, option, select, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)
import ScenarioConfig
import Scenarios


type alias Model =
    { scenario : Scenario.Model
    , selected : String
    }


type Msg
    = ScenarioMsg Scenario.Msg
    | SelectScenario String


loadScenario : String -> ( Scenario.Model, Cmd Scenario.Msg )
loadScenario key =
    let
        json =
            case key of
                "two-parallel"    -> Scenarios.twoParallel
                "three-pipeline"  -> Scenarios.threePipeline
                "belter-shipyard" -> Scenarios.belterShipyard
                _                 -> Scenarios.singleWorker
    in
    case ScenarioConfig.decode json of
        Ok cfg ->
            Scenario.init cfg

        Err _ ->
            Scenario.init
                (ScenarioConfig.decode Scenarios.singleWorker
                    |> Result.withDefault dummyCfg
                )


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( sm, sc ) = loadScenario "single-worker"
    in
    ( { scenario = sm, selected = "single-worker" }
    , Cmd.map ScenarioMsg sc
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScenarioMsg smsg ->
            let
                ( sm, sc ) = Scenario.update smsg model.scenario
            in
            ( { model | scenario = sm }, Cmd.map ScenarioMsg sc )

        SelectScenario key ->
            let
                ( sm, sc ) = loadScenario key
            in
            ( { model | scenario = sm, selected = key }, Cmd.map ScenarioMsg sc )


view : Model -> Html Msg
view model =
    div [ style "padding" "1rem", style "font-family" "monospace" ]
        [ div [ style "margin-bottom" "0.75rem" ]
            [ h2 [ style "margin" "0 0 0.5rem" ] [ text model.scenario.config.meta.title ]
            , select [ onInput SelectScenario, style "margin-right" "0.5rem" ]
                [ option [ value "single-worker"   ] [ text "M/M/1 Single Worker" ]
                , option [ value "two-parallel"    ] [ text "M/M/2 Two Parallel" ]
                , option [ value "three-pipeline"  ] [ text "Three-stage Pipeline" ]
                , option [ value "belter-shipyard" ] [ text "Belter Shipyard (expanse)" ]
                ]
            ]
        , Html.map ScenarioMsg (Scenario.view model.scenario)
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ScenarioMsg (Scenario.subscriptions model.scenario)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


dummyCfg : ScenarioConfig.ScenarioConfig
dummyCfg =
    { meta            = { id = "err", title = "Error", theme = "plain", seed = 0, speed = 1 }
    , nodes           = []
    , queues          = []
    , edges           = []
    , locks           = []
    , scheduledEvents = []
    }
