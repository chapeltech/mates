module DavXml exposing (Node, Response, contains, property, responses)

import XmlParser


type alias Node =
    { namespace : String, name : String, content : String, children : List Child }


type Child = Child Node


childNodes : Node -> List Node
childNodes node = List.map (\(Child child) -> child) node.children


type alias Response =
    { href : String, properties : List Node }


responses : String -> Result String (List Response)
responses body =
    XmlParser.parse body
        |> Result.mapError (\_ -> "Invalid DAV XML response")
        |> Result.andThen
            (\document ->
                if document.docType /= Nothing then
                    Err "DAV responses must not declare a DTD"

                else
                    let
                        root = resolve [] document.root
                    in
                    if root.namespace /= "DAV:" || root.name /= "multistatus" then
                        Err "Expected a DAV multistatus response"

                    else
                        Ok (List.filterMap response (childNodes root))
            )


response : Node -> Maybe Response
response node =
    if node.namespace /= "DAV:" || node.name /= "response" then
        Nothing

    else
        property "DAV:" "href" (childNodes node)
            |> Maybe.map
                (\href ->
                    { href = String.trim href.content
                    , properties =
                        childNodes node
                            |> List.filter (\child -> child.namespace == "DAV:" && child.name == "propstat")
                            |> List.filter (\child -> property "DAV:" "status" (childNodes child) |> Maybe.map (.content >> String.contains " 200 ") |> Maybe.withDefault False)
                            |> List.concatMap (\child -> property "DAV:" "prop" (childNodes child) |> Maybe.map childNodes |> Maybe.withDefault [])
                    }
                )


property : String -> String -> List Node -> Maybe Node
property namespace local nodes =
    List.filter (\node -> node.namespace == namespace && node.name == local) nodes |> List.head


contains : String -> String -> Node -> Bool
contains namespace local node =
    (node.namespace == namespace && node.name == local) || List.any (contains namespace local) (childNodes node)


resolve : List ( String, String ) -> XmlParser.Node -> Node
resolve inherited raw =
    case raw of
        XmlParser.Text content ->
            { namespace = "", name = "", content = content, children = [] }

        XmlParser.Element rawName attributes rawChildren ->
            let
                declarations =
                    attributes
                        |> List.filterMap
                            (\attribute ->
                                if attribute.name == "xmlns" then
                                    Just ( "", attribute.value )

                                else if String.startsWith "xmlns:" attribute.name then
                                    Just ( String.dropLeft 6 attribute.name, attribute.value )

                                else
                                    Nothing
                            )

                namespaces = declarations ++ inherited
                ( prefix, local ) =
                    case String.split ":" rawName of
                        [ p, n ] -> ( p, n )
                        _ -> ( "", rawName )
                children = List.map (resolve namespaces) rawChildren
            in
            { namespace = List.filter (Tuple.first >> (==) prefix) namespaces |> List.head |> Maybe.map Tuple.second |> Maybe.withDefault ""
            , name = local
            , content = String.concat (List.map .content children)
            , children = List.map Child children
            }
