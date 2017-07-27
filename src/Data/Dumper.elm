module Data.Dumper exposing (..)

import Html exposing (Html)
import Html.Attributes



type alias Location =
  { host : String
  , hostname : String
  , protocol : String
  , origin : String
  , port_ : String
  , pathname : String
  , search : String
  , hash : String
  , test : Int
  }

dumpLocation : Location -> Html msg
dumpLocation =
  dumpRecord
    [ ("host", .host >> dumpString)
    , ("hostname", .hostname >> dumpString)
    , ("protocol", .protocol >> dumpString)
    , ("origin", .origin >> dumpString)
    , ("port_", .port_ >> dumpString)
    , ("pathname", .pathname >> dumpString)
    , ("search", .search >> dumpString)
    , ("hash", .hash >> dumpString)
    , ("test", .test >> dumpInt)
    ]
    "Location"



dumpRecord : List (String, a -> Html msg) -> String -> a -> Html msg
dumpRecord extractors name record =
  extractors
    |> List.map (toTupleRecord record)
    |> dumpTupleRecords name
    |> Html.div
      [ Html.Attributes.style
        [ ("white-space", "pre")
        , ("font-family", "monospace")
        ]
      ]

dumpString : String -> Html msg
dumpString string =
  colorText "#98C379" ("\"" ++ string ++ "\"")

dumpInt : Int -> Html msg
dumpInt int =
  colorText "#98C379" (toString int)

dumpFloat : Float -> Html msg
dumpFloat float =
  colorText "#98C379" (toString float)



toTupleRecord : a -> (String, a -> Html msg) -> (String, Html msg)
toTupleRecord record (name, function) =
  (name, function record)

dumpTupleRecords : String -> List (String, Html msg) -> List (Html msg)
dumpTupleRecords name fields =
  case fields of
    (name, content) :: tail ->
      List.concat
        [ [ colorText "#C376DA" "type alias "
          , colorText "#D19A66" (name ++ " =\n")
          , colorText "#C376DA" "  { "
          , colorText "#5DA6E2" "page "
          , colorText "#C376DA" ": "
          , content
          , colorText "#98C379" "\n"
          ]
        , (List.map dumpField tail)
        , [ colorText "#C376DA" "  }" ]
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

dumpField : (String, Html msg) -> Html msg
dumpField (name, field) =
  Html.div []
    [ colorText "#C376DA" "  , "
    , colorText "#5DA6E2" (name ++ " ")
    , colorText "#C376DA" ":"
    , field
    , Html.text "\n"
    ]
