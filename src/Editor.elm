module Editor exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode



-- MODEL


type Node
    = NumNode Int -- Numeric literal node: e.g. '4'
    | FnNode Operator (List Node) -- Function node: e.g. a '+' or '-' node.


type Operator
    = Add
    | Subtract
    | Divide
    | Multiply


type alias Model =
    { -- Abstract Syntax Tree
      ast : Node
    }



-- MSG


type Msg
    = DisplayValue Node


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initialAST =
            FnNode Add
                [ FnNode Multiply [ NumNode 3, NumNode 2 ]
                , NumNode 10
                ]
    in
    ( { ast = initialAST }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }



-- Subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplayValue targetNode ->
            ( model, displayNodeEvaluation targetNode )


view : Model -> Html Msg
view model =
    Html.div [] [ viewNode model.ast ]


viewNode : Node -> Html Msg
viewNode node =
    let
        baseStyles =
            [ style "margin" "5px", style "padding-left" "2px", style "display" "inline-block", style "border-radius" "5px" ]
    in
    case node of
        FnNode operator children ->
            let
                operatorSpan =
                    Html.span [] [ Html.text <| renderOperator operator ]

                styles =
                    style "background-color" "rgba(44, 178, 218, 0.2)" :: baseStyles

                onClickHandler =
                    -- We are stacking multiple spans with event handlers all on top of each other;
                    -- hence, we need to stop propagation in our event handler. Unfortunately, this
                    -- requries us to use a more complex API than Html.Events.onClick.
                    Html.Events.stopPropagationOn "click" <| Json.Decode.succeed ( DisplayValue node, True )
            in
            Html.span
                ([ onClickHandler, class "fn-node node" ] ++ styles)
                ([ operatorSpan ] ++ List.map viewNode children)

        NumNode content ->
            Html.span
                ([ class "num-node node" ] ++ baseStyles)
                [ Html.span [] [ Html.text <| String.fromInt content ] ]


renderOperator : Operator -> String
renderOperator operator =
    case operator of
        Add ->
            "➕"

        Subtract ->
            "➖"

        Divide ->
            "➗"

        Multiply ->
            "✖"


displayNodeEvaluation : Node -> Cmd Msg
displayNodeEvaluation node =
    let
        _ =
            Debug.log "hi" (String.fromInt <| evaluateNode node)
    in
    Cmd.none


evaluateNode : Node -> Int
evaluateNode node =
    case node of
        NumNode content ->
            content

        FnNode operator children ->
            case operator of
                Add ->
                    List.sum <| List.map evaluateNode children

                Subtract ->
                    -- TODO
                    0

                Multiply ->
                    List.product <| List.map evaluateNode children

                Divide ->
                    -- TODO
                    1
