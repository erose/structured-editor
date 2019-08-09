module Editor exposing (..)

import Browser
import Html exposing (Html)
import Html.Attributes
import Html.Events



-- MODEL


type Node
    = NumNode Int
    | FnNode Operator (List Node)


type Operator
    = Add
    | Subtract
    | Divide
    | Multiply


type alias Model =
    { ast : Node }



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
        style =
            Html.Attributes.style

        baseStyles =
            [ style "margin" "5px", style "display" "inline-block", style "border-radius" "5px" ]
    in
    case node of
        FnNode operator children ->
            let
                operatorSpan =
                    Html.span [] [ Html.text <| renderOperator operator ]
            in
            Html.span
                ([ Html.Events.onClick <| DisplayValue node
                 , Html.Attributes.class "fn-node node"
                 ]
                    ++ (style "background-color" "rgba(44, 178, 218, 0.2)" :: baseStyles)
                )
                ([ operatorSpan ] ++ List.map viewNode children)

        NumNode content ->
            Html.span
                ([ Html.Attributes.class "num-node node" ] ++ baseStyles)
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
            "✖️"


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
                    0

                Multiply ->
                    List.product <| List.map evaluateNode children

                Divide ->
                    1
