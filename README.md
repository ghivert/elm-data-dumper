```elm
import Html exposing (Html)

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
  let example = Location "host_" "hostname_" "protocol_" in
  Html.div []
    [ dumpLocation example
    , dumpLocations Nowhere
    , dumpLocations <| Loc example
    ]
```
