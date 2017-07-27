module Data.Dumper exposing (..)

import Html exposing (Html)
import Html.Attributes



type alias Location =
  { host : String
  , hostname : String
  , protocol : String
  , testInt : Int
  , testFloat : Float
  , testRecord :
    { testInside1 : String
    , testInside2 : String
    }
  }

dumpLocation : Location -> Html msg
dumpLocation =
  dumpRecord
    [ ("host", .host >> dumpString)
    , ("hostname", .hostname >> dumpString)
    , ("protocol", .protocol >> dumpString)
    , ("testInt", .testInt >> dumpInt)
    , ("testFloat", .testFloat >> dumpFloat)
    , ("testRecord", .testRecord >> dumpInsideRecord)
    ]
    "Location"

dumpInsideRecord : { testInside1 : String, testInside2 : String } -> Html msg
dumpInsideRecord =
  dumpNestedRecord
    [ ("testInside1", .testInside1 >> dumpString)
    , ("testInside2", .testInside2 >> dumpString)
    ]



dumpRecord : List (String, a -> Html msg) -> String -> a -> Html msg
dumpRecord extractors name record =
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

dumpNestedRecord : List ( String, a -> Html msg ) -> a -> Html msg
dumpNestedRecord extractors record =
  extractors
    |> List.map (toTupleRecord record)
    |> dumpTupleRecords Nothing 4
    |> Html.div
      [ Html.Attributes.style
        [ ("display", "inline") ]
      ]

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
          , colorText "#98C379" "\n"
          ]
        , (List.map (dumpField spaces) tail)
        , [ colorText "#C376DA" (spaces_ ++ "}") ]
        ]

    [] ->
      []

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
  Html.div [ Html.Attributes.style [ ("diplay", "inline") ] ]
    [ colorText "#C376DA" (spaces_ ++ ", ")
    , colorText "#5DA6E2" name
    , colorText "#C376DA" " : "
    , field
    , Html.text "\n"
    ]


main : Html msg
main =
  dumpLocation
    (Location "host_" "hostname_" "protocol_" 0 1.5 { testInside1 = "testInside1_", testInside2 = "testInside2_" })
