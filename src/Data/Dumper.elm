module Data.Dumper exposing (dumpRecord, dumpUnion, dumpNestedRecord, dumpString, dumpInt, dumpFloat, dumpBool)

{-| Provides facilities to dump data structures in HTML.

@docs dumpRecord
@docs dumpNestedRecord
@docs dumpUnion
@docs dumpString
@docs dumpInt
@docs dumpFloat
@docs dumpBool
-}

import Html exposing (Html)
import Html.Attributes


{-| Dump a record in HTML. -}
dumpRecord : String -> List (String, a -> Html msg) -> a -> Html msg
dumpRecord name extractors record =
  extractors
    |> List.map (toTupleRecord record)
    |> dumpTupleRecords (Just name) 2
    |> inDiv

{-| Dump a union in HTML. -}
dumpUnion : String -> (String, Html msg) -> Html msg
dumpUnion name (type_, content) =
  [ colorText "#C376DA" "type "
  , colorText "#D19A66" (name ++ " =\n")
  , colorText "#C376DA" "  "
  , Html.span [ Html.Attributes.style [ ("color", "#D19A66"), ("display", "inline-block") ] ] [ Html.text (type_ ++ " \n") ]
  , content
  ]
    |> inDiv

{-| Dump a nested record in HTML. It is a hack: you have to provide the depth of the record. -}
dumpNestedRecord : Int -> List ( String, a -> Html msg ) -> a -> Html msg
dumpNestedRecord depth extractors record =
  extractors
    |> List.map (toTupleRecord record)
    |> dumpTupleRecords Nothing (2 * (depth + 1))
    |> Html.span []

{-| Dump a String in HTML. -}
dumpString : String -> Html msg
dumpString string =
  colorText "#98C379" ("\"" ++ string ++ "\"")

{-| Dump an Int in HTML. -}
dumpInt : Int -> Html msg
dumpInt int =
  colorText "#D19A66" (toString int)

{-| Dump a Float in HTML. -}
dumpFloat : Float -> Html msg
dumpFloat float =
  colorText "#D19A66" (toString float)

{-| Dump a Boolean in HTML. -}
dumpBool : Bool -> Html msg
dumpBool bool =
  colorText "#D19A66" (toString bool)


inDiv : List (Html msg) -> Html msg
inDiv =
  Html.div
    [ Html.Attributes.style
      [ ("white-space", "pre")
      , ("font-family", "monospace")
      , ("background-color", "#F6F8FA")
      , ("padding", "12px")
      , ("box-sizing", "border-box")
      ]
    ]

toTupleRecord : a -> (String, a -> Html msg) -> (String, Html msg)
toTupleRecord record (name, function) =
  (name, function record)

dumpTupleRecords : Maybe String -> Int -> List (String, Html msg) -> List (Html msg)
dumpTupleRecords name spaces fields =
  let spaces_ = String.repeat spaces " " in
  case fields of
    (fieldName, content) :: tail ->
      List.concat
        [ case name of
          Just name_ ->
            [ colorText "#C376DA" "type alias "
            , colorText "#D19A66" (name_ ++ " =\n")
            ]
          Nothing ->
            [ Html.text "\n" ]
        , [ colorText "#C376DA" (spaces_ ++ "{ ")
          , colorText "#5DA6E2" fieldName
          , colorText "#C376DA" " : "
          , content
          , if List.isEmpty tail then
              Html.text ""
            else
              colorText "#98C379" "\n"
          ]
        , (List.map (dumpField spaces) tail)
        , [ colorText "#C376DA" (spacesIfPresent tail spaces_ ++ "}") ]
        ]

    [] ->
      []

spacesIfPresent : List a -> String -> String
spacesIfPresent tail spaces =
  if List.isEmpty tail then
    " "
  else
    spaces

colorText : String -> String -> Html msg
colorText color_ text_ =
  Html.span
    [ Html.Attributes.style
      [ ("color", color_) ]
    ]
    [ Html.text text_ ]

dumpField : Int -> (String, Html msg) -> Html msg
dumpField spaces (name, field) =
  let spaces_ = String.repeat spaces " " in
  Html.span []
    [ colorText "#C376DA" (spaces_ ++ ", ")
    , colorText "#5DA6E2" name
    , colorText "#C376DA" " : "
    , field
    , Html.text "\n"
    ]
