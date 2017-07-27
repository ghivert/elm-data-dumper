module Data.Dumper exposing (..)

import Html exposing (Html)
import Html.Attributes





type alias Location =
  { host : String
  , hostname : String
  , protocol : String
  }

type Locations
  = Nowhere
  | Loc Location

dumpLocation : Location -> Html msg
dumpLocation =
  dumpRecord "Location"
    [ ("host", .host >> dumpString)
    , ("hostname", .hostname >> dumpString)
    , ("protocol", .protocol >> dumpString)
    ]

dumpNestedLocation : Location -> Html msg
dumpNestedLocation =
  dumpNestedRecord 1
    [ ("host", .host >> dumpString)
    , ("hostname", .hostname >> dumpString)
    , ("protocol", .protocol >> dumpString)
    ]

dumpLocations : Locations -> Html msg
dumpLocations locations =
  dumpUnion "Locations" <|
    case locations of
      Nowhere ->
        ("Nowhere", Html.text "")
      Loc location ->
        ("Loc", dumpNestedLocation location)

main : Html msg
main =
  Html.div []
    [ dumpLocation
      (Location "host_" "hostname_" "protocol_")
    , dumpLocations
        Nowhere
    ,dumpLocations
      (Loc (Location "host_" "hostname_" "protocol_"))
    ]





dumpRecord : String -> List (String, a -> Html msg) -> a -> Html msg
dumpRecord name extractors record =
  extractors
    |> List.map (toTupleRecord record)
    |> dumpTupleRecords (Just name) 2
    |> Html.div
      [ Html.Attributes.style
        [ ("white-space", "pre")
        , ("font-family", "monospace")
        , ("background-color", "#F6F8FA")
        , ("padding", "12px")
        , ("box-sizing", "border-box")
        ]
      ]

dumpUnion : String -> (String, Html msg) -> Html msg
dumpUnion name (type_, content) =
  [ colorText "#C376DA" "type "
  , colorText "#D19A66" (name ++ " =\n")
  , colorText "#C376DA" "  "
  , Html.span [ Html.Attributes.style [ ("color", "#D19A66"), ("display", "inline-block") ] ] [ Html.text (type_ ++ " \n") ]
  , content
  ]
    |> Html.div
      [ Html.Attributes.style
        [ ("white-space", "pre")
        , ("font-family", "monospace")
        , ("background-color", "#F6F8FA")
        , ("padding", "12px")
        , ("box-sizing", "border-box")
        ]
      ]

dumpNestedRecord : Int -> List ( String, a -> Html msg ) -> a -> Html msg
dumpNestedRecord depth extractors record =
  extractors
    |> List.map (toTupleRecord record)
    |> dumpTupleRecords Nothing (2 * (depth + 1))
    |> Html.span []

dumpString : String -> Html msg
dumpString string =
  colorText "#98C379" ("\"" ++ string ++ "\"")

dumpInt : Int -> Html msg
dumpInt int =
  colorText "#D19A66" (toString int)

dumpFloat : Float -> Html msg
dumpFloat float =
  colorText "#D19A66" (toString float)



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
