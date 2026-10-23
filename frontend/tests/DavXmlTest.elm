module DavXmlTest exposing (tests)

import DavXml
import Expect
import Test exposing (Test, describe, test)


tests : Test
tests =
    describe "DAV XML interoperability"
        [ test "resolves arbitrary namespace prefixes and decodes entity text" <|
            \_ ->
                DavXml.responses "<m:multistatus xmlns:m='DAV:'><m:response><m:href>/dav/contacts/a.vcf</m:href><m:propstat><m:prop><v:address-data xmlns:v='urn:ietf:params:xml:ns:carddav'>FN:A &amp; B</v:address-data><m:getetag>&quot;tag&quot;</m:getetag></m:prop><m:status>HTTP/1.1 200 OK</m:status></m:propstat></m:response></m:multistatus>"
                    |> Result.map (List.head >> Maybe.andThen (\r -> DavXml.property "urn:ietf:params:xml:ns:carddav" "address-data" r.properties) >> Maybe.map .content)
                    |> Expect.equal (Ok (Just "FN:A & B"))
        , test "accepts a default namespace and retains privilege descendants" <|
            \_ ->
                DavXml.responses "<multistatus xmlns='DAV:'><response><href>/dav/contacts/</href><propstat><prop><current-user-privilege-set><privilege><read/></privilege></current-user-privilege-set></prop><status>HTTP/1.1 200 OK</status></propstat></response></multistatus>"
                    |> Result.map (List.head >> Maybe.andThen (\r -> DavXml.property "DAV:" "current-user-privilege-set" r.properties) >> Maybe.map (DavXml.contains "DAV:" "write"))
                    |> Expect.equal (Ok (Just False))
        , test "does not mistake a failed property for successful address data" <|
            \_ ->
                DavXml.responses "<multistatus xmlns='DAV:'><response><href>/missing</href><propstat><prop><address-data xmlns='urn:ietf:params:xml:ns:carddav'>hidden</address-data></prop><status>HTTP/1.1 404 Not Found</status></propstat></response></multistatus>"
                    |> Result.map (List.head >> Maybe.map .properties)
                    |> Expect.equal (Ok (Just []))
        , test "keeps calendar data in CDATA" <|
            \_ ->
                DavXml.responses "<multistatus xmlns='DAV:'><response><href>/a.ics</href><propstat><prop><calendar-data xmlns='urn:ietf:params:xml:ns:caldav'><![CDATA[SUMMARY:A & B]]></calendar-data></prop><status>HTTP/1.1 200 OK</status></propstat></response></multistatus>"
                    |> Result.map (List.head >> Maybe.andThen (\r -> DavXml.property "urn:ietf:params:xml:ns:caldav" "calendar-data" r.properties) >> Maybe.map .content)
                    |> Expect.equal (Ok (Just "SUMMARY:A & B"))
        , test "rejects malformed XML" <|
            \_ ->
                case DavXml.responses "<multistatus>" of
                    Err _ -> Expect.pass
                    Ok _ -> Expect.fail "Malformed XML was accepted"
        , test "rejects an unrelated namespace" <|
            \_ ->
                case DavXml.responses "<multistatus xmlns='urn:unrelated'/>" of
                    Err _ -> Expect.pass
                    Ok _ -> Expect.fail "Non-DAV XML was accepted"
        ]
