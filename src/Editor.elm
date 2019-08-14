module Editor exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode



-- TODOS
-- show results
-- un-highlight
-- user input
-- subtraction and division
-- arithmetic -> lisp
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
    , selectedNode : Maybe Node
    }



-- MSG


type Msg
    = DisplayValue Node


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initialAST =
            FnNode Add
                [ FnNode Multiply [ NumNode 6245, FnNode Multiply [ FnNode Add [ NumNode 123, NumNode 456 ], NumNode 5478 ] ]
                , NumNode 537853970
                ]
    in
    ( { ast = initialAST, selectedNode = Nothing }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }



-- Subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplayValue targetNode ->
            ( { model | selectedNode = Just targetNode }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        result =
            case model.selectedNode of
                Just node ->
                    String.fromInt <| evaluateNode node

                Nothing ->
                    ""
    in
    Html.div [ style "display" "flex", style "align-items" "center", style "font-family" "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'" ]
        [ viewNode model.ast model.selectedNode 0
        , Html.span
            [ style "padding" "10px"
            , style "margin-left" "10px"
            , style "background-color" orange
            , style "border-radius" "5px"
            , style "font-size" "20px"
            , style "min-width" "20px"
            , style "min-height" "20px"
            , style "text-align" "center"
            ]
            [ Html.text result ]
        ]


orange =
    "rgba(255, 165, 0, 0.5)"


blues : Array String
blues =
    Array.fromList
        [ "rgba(196,220,255,1)"
        , "rgba(155,194,251,1)"
        , "rgba(206, 228, 255, 1)"
        ]


viewNode : Node -> Maybe Node -> Int -> Html Msg
viewNode node selectedNode depth =
    let
        baseStyles =
            [ style "font-size" "30px", style "margin" "8px", style "padding-left" "5px", style "display" "inline-block", style "border-radius" "5px" ]

        viewChild childNode =
            viewNode childNode selectedNode (depth + 1)
    in
    case node of
        FnNode operator children ->
            let
                operatorSpan =
                    Html.span [] [ Html.text <| renderOperator operator ]

                blue =
                    -- Default case should never happen, as we are modding by the length of the
                    -- blues array.
                    Maybe.withDefault "rgb( 0,0,0,)" <| Array.get (remainderBy (Array.length blues) depth) blues

                backgroundColor =
                    case selectedNode of
                        Just n ->
                            if node == n then
                                orange

                            else
                                blue

                        Nothing ->
                            blue

                stylesForNodesWithChildren =
                    [ style "background-color" backgroundColor, style "border" "thin solid rgb(179, 213, 255)" ]

                styles =
                    stylesForNodesWithChildren ++ baseStyles

                onClickHandler =
                    -- We are stacking multiple spans with event handlers all on top of each other;
                    -- hence, we need to stop propagation in our event handler. Unfortunately, this
                    -- requries us to use a more complex API than Html.Events.onClick.
                    Html.Events.stopPropagationOn "mouseover" <| Json.Decode.succeed ( DisplayValue node, True )
            in
            Html.span
                ([ onClickHandler, class "fn-node node" ] ++ styles)
                ([ operatorSpan ] ++ List.map viewChild children)

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
