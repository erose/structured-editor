module Editor exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Json.Decode
import Parser exposing ((|.), (|=), Parser)



-- TODOS
-- arithmetic -> lisp
-- MODEL


type Node
    = NumNode Int -- Numeric literal node: e.g. '4'
    | FnNode Operator Node Node -- Function node: e.g. a '+' or '-' node.


type Operator
    = Add
    | Subtract
    | Divide
    | Multiply


type alias Model =
    { -- Abstract Syntax Tree
      selectedNode : Maybe Node
    , currentInput : String
    }



-- MSG


type Msg
    = DisplayValue Node
    | StopDisplayingValue
    | ChangeInput String


init : flags -> ( Model, Cmd Msg )
init _ =
    let
        initialInput =
            "(+ 3 (* 8 6))"
    in
    ( { currentInput = initialInput, selectedNode = Nothing }, Cmd.none )


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = always Sub.none }



-- Subscriptions


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplayValue targetNode ->
            ( { model | selectedNode = Just targetNode }, Cmd.none )

        StopDisplayingValue ->
            ( { model | selectedNode = Nothing }, Cmd.none )

        ChangeInput newValue ->
            ( { model | currentInput = newValue, selectedNode = Nothing }, Cmd.none )



-- Simple LISP parsing function.


parseToAST : String -> Node
parseToAST string =
    case Parser.run nodeParser string of
        Result.Ok value ->
            value

        Result.Err _ ->
            NumNode 0


nodeParser : Parser Node
nodeParser =
    Parser.oneOf [ fnNodeParser, numNodeParser ]


fnNodeParser : Parser Node
fnNodeParser =
    Parser.succeed FnNode
        |. Parser.symbol "("
        |. Parser.spaces
        |= operatorParser
        |. Parser.spaces
        -- We need to use Parser.lazy here because nodeParser recursively calls fnNodeParser.
        |= Parser.lazy (\_ -> nodeParser)
        |. Parser.spaces
        |= Parser.lazy (\_ -> nodeParser)
        |. Parser.spaces
        |. Parser.symbol ")"


operatorParser : Parser Operator
operatorParser =
    Parser.oneOf
        [ Parser.map (\_ -> Add) <| Parser.keyword "+"
        , Parser.map (\_ -> Subtract) <| Parser.keyword "-"
        , Parser.map (\_ -> Divide) <| Parser.keyword "/"
        , Parser.map (\_ -> Multiply) <| Parser.keyword "*"
        ]


numNodeParser : Parser Node
numNodeParser =
    Parser.succeed NumNode
        |= Parser.int


view : Model -> Html Msg
view model =
    let
        result =
            case model.selectedNode of
                Just node ->
                    String.fromInt <| evaluateNode node

                Nothing ->
                    ""

        inputDiv =
            Html.div []
                [ Html.input
                    [ Html.Attributes.attribute "value" model.currentInput
                    , Html.Events.onInput (\newValue -> ChangeInput newValue)
                    ]
                    []
                ]

        ast =
            parseToAST model.currentInput

        expressionDiv =
            Html.div [ style "display" "flex", style "align-items" "center", style "font-family" "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol'" ]
                [ viewNode ast model.selectedNode 0
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
    in
    Html.div [] [ inputDiv, expressionDiv ]


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
        FnNode operator left right ->
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

                -- We are stacking multiple spans with event handlers all on top of each other;
                -- hence, we need to stop propagation in our event handler. Unfortunately, this
                -- requries us to use a more complex API than Html.Events.onClick in both these
                -- handlers.
                mouseOverHandler =
                    Html.Events.stopPropagationOn "mouseover" <| Json.Decode.succeed ( DisplayValue node, True )

                mouseOutHandler =
                    Html.Events.stopPropagationOn "mouseout" <| Json.Decode.succeed ( StopDisplayingValue, True )
            in
            Html.span
                ([ mouseOverHandler, mouseOutHandler, class "fn-node node" ] ++ styles)
                [ operatorSpan, viewChild left, viewChild right ]

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


evaluateNode : Node -> Int
evaluateNode node =
    case node of
        NumNode content ->
            content

        FnNode operator left right ->
            case operator of
                Add ->
                    evaluateNode left + evaluateNode right

                Subtract ->
                    evaluateNode left - evaluateNode right

                Multiply ->
                    evaluateNode left * evaluateNode right

                Divide ->
                    evaluateNode left // evaluateNode right
