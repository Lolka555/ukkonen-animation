port module UkkonenVisualization exposing (main)

import Array exposing (Array)
import Browser
import Dict
import Html exposing (Html, button, div, h1, h2, input, li, span, text, ul)
import Html.Attributes exposing (class, disabled, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Markdown
import String
import UkkonenAlgorithm exposing (ActivePoint, UkkonenState, initialState, steps)
import UkkonenTree exposing (ClosingIndex(..), UkkonenEdge, UkkonenNode, UkkonenTree, getNode)


port tree : Encode.Value -> Cmd msg


type alias Model =
    { input : String
    , builtString : String
    , steps : Array UkkonenState
    , currentStep : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { input = ""
      , builtString = ""
      , steps = Array.empty
      , currentStep = 0
      }
    , Cmd.none
    )


type Msg
    = UpdateInput String
    | Build
    | Back
    | Forward


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateInput s ->
            ( { model | input = s }, Cmd.none )

        Build ->
            let
                raw =
                    String.trim model.input
            in
            if raw == "" then
                ( model, Cmd.none )

            else
                let
                    terminated =
                        raw ++ "$"

                    allSteps =
                        Array.fromList (initialState :: steps terminated)

                    newModel =
                        { model
                            | builtString = terminated
                            , steps = allSteps
                            , currentStep = 0
                        }
                in
                ( newModel, treeCmd newModel )

        Back ->
            let
                newModel =
                    { model | currentStep = max (model.currentStep - 1) 0 }
            in
            ( newModel, treeCmd newModel )

        Forward ->
            let
                last =
                    max 0 (Array.length model.steps - 1)

                newModel =
                    { model | currentStep = min (model.currentStep + 1) last }
            in
            ( newModel, treeCmd newModel )


treeCmd : Model -> Cmd Msg
treeCmd model =
    case Array.get model.currentStep model.steps of
        Just state ->
            let
                prefix =
                    String.left state.charsAdded model.builtString
            in
            tree (treeJson state.tree state.activePoint prefix)

        Nothing ->
            Cmd.none



-- VIEW

introText : Html msg
introText =
    Markdown.toHtml []
        """
[Ukkonen's algorithm](https://en.wikipedia.org/wiki/Ukkonen%27s_algorithm) is a method of constructing the [suffix tree](https://en.wikipedia.org/wiki/Suffix_tree) of a string in linear time. Suffix trees are useful because they can efficiently answer many questions about a string, such as how many times a given substring occurs within the string. Enter an input string below and you'll be able to watch step-by-step as Ukkonen's algorithm builds a suffix tree.

I was inspired to build this visualization after reading [this great explanation](http://stackoverflow.com/a/9513423) of Ukkonen's algorithm. I'd recommend first reading that for an overview of how the algorithm works and then playing around with this visualization. Also quite helpful is the explanation given in [this video](https://www.youtube.com/watch?v=aPRqocoBsFQ).
"""


view : Model -> Html Msg
view model =
    let
        leftEnabled =
            model.currentStep > 0

        rightEnabled =
            model.currentStep < (Array.length model.steps - 1)
    in
    div [ id "visualization" ]
        [ div [ id "heading" ]
            [ h1 [] [ text "Visualization of Ukkonen's Algorithm" ]
            , introText
            , div [ id "input-string" ]
                [ input
                    [ type_ "text"
                    , placeholder "input string..."
                    , value model.input
                    , onInput UpdateInput
                    ]
                    []
                , span [ id "input-button-wrapper" ]
                    [ button [ onClick Build ] [ text "build suffix tree" ] ]
                ]
            ]
        , div [ id "steps-wrapper" ]
            [ div [ id "side-box" ]
                [ h2 []
                    [ text <|
                        "Step "
                            ++ String.fromInt (model.currentStep + 1)
                            ++ " of "
                            ++ String.fromInt (Array.length model.steps)
                    ]
                , div [ id "navigation" ]
                    [ span [ id "left-button-wrapper" ] [ leftButton leftEnabled ]
                    , span [ id "right-button-wrapper" ] [ rightButton rightEnabled ]
                    ]
                , algorithmStateView model
                ]
            , div [ id "letter-blocks" ] (letterBlocks model.builtString model.currentStep model.steps)
            ]
        ]


leftButton : Bool -> Html Msg
leftButton enabled =
    button [ onClick Back, disabled (not enabled) ] [ text "< prev" ]


rightButton : Bool -> Html Msg
rightButton enabled =
    button [ onClick Forward, disabled (not enabled) ] [ text "next >" ]


algorithmStateView : Model -> Html Msg
algorithmStateView model =
    case Array.get model.currentStep model.steps of
        Nothing ->
            text ""

        Just state ->
            let
                activeNodeString =
                    String.fromInt state.activePoint.nodeId

                activeEdgeString =
                    case state.activePoint.edge of
                        Just ( edgeChar, _ ) ->
                            String.fromChar edgeChar

                        Nothing ->
                            "none"

                activeLengthString =
                    case state.activePoint.edge of
                        Just ( _, edgeSteps ) ->
                            String.fromInt edgeSteps

                        Nothing ->
                            "0"

                remainderString =
                    String.fromInt (state.remainder - 1)
            in
            ul [ id "algorithm-state" ]
                [ li []
                    [ span [] [ text "active_node:" ]
                    , span [ id "var-active-node" ] [ text activeNodeString ]
                    ]
                , li []
                    [ span [] [ text "active_edge:" ]
                    , span [ id "var-active-edge" ] [ text activeEdgeString ]
                    ]
                , li []
                    [ span [] [ text "active_length:" ]
                    , span [ id "var-active-length" ] [ text activeLengthString ]
                    ]
                , li []
                    [ span [] [ text "remainder:" ]
                    , span [ id "var-remainder" ] [ text remainderString ]
                    ]
                ]


letterBlocks : String -> Int -> Array UkkonenState -> List (Html Msg)
letterBlocks str currentStep stepsArr =
    case Array.get currentStep stepsArr of
        Just state ->
            List.indexedMap
                (\i c ->
                    let
                        charsAdded =
                            state.charsAdded

                        added =
                            if i < charsAdded then
                                [ class "added" ]

                            else
                                []

                        remainder =
                            if i > charsAdded - state.remainder && i < charsAdded then
                                [ class "remainder" ]

                            else
                                []
                    in
                    div (added ++ remainder) [ text (String.fromChar c) ]
                )
                (String.toList str)

        Nothing ->
            []



-- TREE JSON (for D3)

treeJson : UkkonenTree -> ActivePoint -> String -> Encode.Value
treeJson tree_ activePoint_ str =
    treeJsonHelp 0 tree_ activePoint_ str


treeJsonHelp : Int -> UkkonenTree -> ActivePoint -> String -> Encode.Value
treeJsonHelp rootId tree_ activePoint_ str =
    let
        root : UkkonenNode
        root =
            getNode rootId tree_

        isActivePoint =
            activePoint_.nodeId == rootId
    in
    Encode.object
        [ ( "id", Encode.int rootId )
        , ( "suffixLink"
          , case root.suffixLink of
                Just n ->
                    Encode.int n

                Nothing ->
                    Encode.null
          )
        , ( "isActivePoint", Encode.bool isActivePoint )
        , ( "children"
          , Encode.object
                (List.map
                    (\( c, edge ) ->
                        let
                            labelEnd =
                                case edge.labelEnd of
                                    Definite l ->
                                        l

                                    EndOfString ->
                                        String.length str
                        in
                        ( String.fromChar c
                        , Encode.object
                            [ ( "label", Encode.string (String.slice edge.labelStart labelEnd str) )
                            , ( "pointingTo", treeJsonHelp edge.pointingTo tree_ activePoint_ str )
                            , ( "edgeSteps"
                              , case activePoint_.edge of
                                    Just ( ac, edgeSteps ) ->
                                        if isActivePoint && c == ac then
                                            Encode.int edgeSteps

                                        else
                                            Encode.null

                                    Nothing ->
                                        Encode.null
                              )
                            ]
                        )
                    )
                    (Dict.toList root.edges)
                )
          )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
