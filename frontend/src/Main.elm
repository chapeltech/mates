module Main exposing (main)

import DavXml
import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h1
        , h2
        , header
        , img
        , input
        , label
        , li
        , main_
        , p
        , section
        , select
        , option
        , span
        , text
        , textarea
        , ul
        )
import Html.Attributes
    exposing
        ( attribute
        , autocomplete
        , alt
        , class
        , disabled
        , id
        , name
        , placeholder
        , src
        , selected
        , style
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import String
import Task
import Time


type Status
    = Loading
    | Loaded
    | Failed String


type Page
    = ContactsPage
    | CalendarPage


type CalendarView
    = AgendaView
    | DayView
    | WeekView
    | MonthView


type alias Email =
    { address : String
    , types : List String
    }


type alias ContactField =
    { name : String
    , label : Maybe String
    , value : String
    }


type alias ContactPhoto =
    { contentType : String
    , source : String
    }


type alias DavData =
    { etag : String
    , href : String
    , body : String
    }


type alias CardProperty =
    { group : Maybe String
    , name : String
    , params : List ( String, String )
    , value : String
    }


type alias Contact =
    { etag : String
    , href : String
    , id : String
    , displayName : String
    , emails : List Email
    , fields : List ContactField
    , photo : Maybe ContactPhoto
    , rawBody : String
    , isFullDetail : Bool
    }


type alias ContactEditDraft =
    { etag : String
    , contactId : String
    , href : String
    , rawBody : String
    , displayName : String
    , email : String
    , emailTypes : String
    , phone : String
    , birthday : String
    , deathDate : String
    , birthPlace : String
    , deathPlace : String
    , note : String
    , relationships : List RelationshipDraft
    }


type alias RelationshipDraft =
    { originalLines : List String
    , changed : Bool
    , target : Maybe String
    , label : String
    , name : String
    }


type alias FamilyRelation =
    { label : String
    , name : String
    }


type alias FamilyGroups =
    { parents : List FamilyRelation
    , partners : List FamilyRelation
    , siblings : List FamilyRelation
    , children : List FamilyRelation
    , other : List FamilyRelation
    }


type FamilyBucket
    = ParentBucket
    | PartnerBucket
    | SiblingBucket
    | ChildBucket
    | OtherBucket


type alias CalendarEntry =
    { id : String
    , calendarName : String
    , title : String
    , startsAt : String
    , endsAt : Maybe String
    , location : Maybe String
    , description : Maybe String
    , allDay : Bool
    }


type alias WeekDay =
    { isoDate : String
    , weekday : String
    , label : String
    }


type alias Date =
    { year : Int
    , month : Int
    , day : Int
    }


type alias Model =
    { origin : String
    , contactCollection : Maybe String
    , contactsWritable : Bool
    , calendarPending : Int
    , page : Page
    , contacts : List Contact
    , contactQuery : String
    , selectedContact : Maybe String
    , calendarEntries : List CalendarEntry
    , calendarQuery : String
    , selectedCalendarEntry : Maybe String
    , calendarView : CalendarView
    , todayDate : Date
    , calendarCursor : Date
    , status : Status
    , creatingContact : Bool
    , editDraft : Maybe ContactEditDraft
    , pendingEditContact : Maybe String
    , draftName : String
    , draftEmail : String
    , draftEmailTypes : String
    , savingContact : Bool
    , savingContactEdit : Bool
    }


type Msg
    = CollectionsLoaded Bool (Result Http.Error String)
    | ContactsLoaded (Result Http.Error String)
    | ContactDetailLoaded String (Result Http.Error String)
    | CalendarEntriesLoaded (Result Http.Error String)
    | CurrentDateLoaded Date
    | ChangePage Page
    | ChangeCalendarView CalendarView
    | NavigateCalendar Int
    | ContactQueryChanged String
    | CalendarQueryChanged String
    | SelectContact String
    | SelectCalendarEntry String
    | Refresh
    | StartCreatingContact
    | CancelCreatingContact
    | StartEditingContact
    | CancelEditingContact
    | DraftNameChanged String
    | DraftEmailChanged String
    | DraftEmailTypesChanged String
    | EditNameChanged String
    | EditEmailChanged String
    | EditEmailTypesChanged String
    | EditPhoneChanged String
    | EditBirthdayChanged String
    | EditDeathDateChanged String
    | EditBirthPlaceChanged String
    | EditDeathPlaceChanged String
    | EditNoteChanged String
    | AddEditRelationship
    | RemoveEditRelationship Int
    | EditRelationshipLabelChanged Int String
    | EditRelationshipTargetChanged Int String
    | EditRelationshipNameChanged Int String
    | SubmitContact
    | SubmitContactAt Time.Posix
    | SubmitContactEdit
    | ContactCreated String (Result Http.Error String)
    | ContactUpdated String (Result Http.Error String)


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : String -> ( Model, Cmd Msg )
init origin =
    ( { origin = origin
      , contactCollection = Nothing
      , contactsWritable = False
      , calendarPending = 0
      , page = ContactsPage
      , contacts = []
      , contactQuery = ""
      , selectedContact = Nothing
      , calendarEntries = []
      , calendarQuery = ""
      , selectedCalendarEntry = Nothing
      , calendarView = AgendaView
      , todayDate = defaultDate
      , calendarCursor = defaultDate
      , status = Loading
      , creatingContact = False
      , editDraft = Nothing
      , pendingEditContact = Nothing
      , draftName = ""
      , draftEmail = ""
      , draftEmailTypes = "INTERNET"
      , savingContact = False
      , savingContactEdit = False
      }
    , Cmd.batch [ fetchAll, loadCurrentDate ]
    )


loadCurrentDate : Cmd Msg
loadCurrentDate =
    Task.map2 dateFromPosix Time.here Time.now
        |> Task.perform CurrentDateLoaded


fetchAll : Cmd Msg
fetchAll =
    Cmd.batch [ discoverCollections False "/dav/", discoverCollections True "/dav/calendars/" ]


fetchContacts : Cmd Msg
fetchContacts =
    Http.request
        { method = "REPORT"
        , headers = davXmlHeaders
        , url = "/dav/contacts/"
        , body = Http.stringBody "application/xml; charset=utf-8" addressBookQueryBody
        , expect = Http.expectString ContactsLoaded
        , timeout = Nothing
        , tracker = Nothing
        }


discoverCollections : Bool -> String -> Cmd Msg
discoverCollections calendars href =
    Http.request
        { method = "PROPFIND"
        , headers = davXmlHeaders
        , url = href
        , body = Http.stringBody "application/xml" "<D:propfind xmlns:D=\"DAV:\"><D:prop><D:resourcetype/><D:current-user-privilege-set/></D:prop></D:propfind>"
        , expect = Http.expectString (CollectionsLoaded calendars)
        , timeout = Nothing
        , tracker = Nothing
        }


fetchCalendarCollection : String -> Cmd Msg
fetchCalendarCollection href =
    Http.request
        { method = "REPORT"
        , headers = davXmlHeaders
        , url = href
        , body = Http.stringBody "application/xml; charset=utf-8" calendarQueryBody
        , expect = Http.expectString CalendarEntriesLoaded
        , timeout = Nothing
        , tracker = Nothing
        }


davXmlHeaders : List Http.Header
davXmlHeaders =
    [ Http.header "Depth" "1"
    , Http.header "Content-Type" "application/xml; charset=utf-8"
    ]


addressBookQueryBody : String
addressBookQueryBody =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        ++ "<C:addressbook-query xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
        ++ "<D:prop><D:getetag/><C:address-data><C:prop name=\"UID\"/><C:prop name=\"FN\"/><C:prop name=\"EMAIL\"/></C:address-data></D:prop>"
        ++ "</C:addressbook-query>"


addressBookMultigetBody : String -> String
addressBookMultigetBody href =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        ++ "<C:addressbook-multiget xmlns:D=\"DAV:\" xmlns:C=\"urn:ietf:params:xml:ns:carddav\">"
        ++ "<D:prop><D:getetag/><C:address-data/></D:prop>"
        ++ "<D:href>"
        ++ escapeXmlText href
        ++ "</D:href>"
        ++ "</C:addressbook-multiget>"


fetchContactDetail : Contact -> Cmd Msg
fetchContactDetail contact =
    Http.request
        { method = "REPORT"
        , headers = davXmlHeaders
        , url = "/dav/contacts/"
        , body = Http.stringBody "application/xml; charset=utf-8" (addressBookMultigetBody contact.href)
        , expect = Http.expectString (ContactDetailLoaded contact.id)
        , timeout = Nothing
        , tracker = Nothing
        }


calendarQueryBody : String
calendarQueryBody =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>"
        ++ "<CAL:calendar-query xmlns:D=\"DAV:\" xmlns:CAL=\"urn:ietf:params:xml:ns:caldav\">"
        ++ "<D:prop><D:getetag/><CAL:calendar-data/></D:prop>"
        ++ "<CAL:filter><CAL:comp-filter name=\"VCALENDAR\"><CAL:comp-filter name=\"VEVENT\"/></CAL:comp-filter></CAL:filter>"
        ++ "</CAL:calendar-query>"


createContact : Model -> Time.Posix -> Cmd Msg
createContact model now =
    let
        uid =
            "web-" ++ String.fromInt (Time.posixToMillis now)

        href =
            "/dav/contacts/" ++ uid ++ ".vcf"
    in
    Http.request
        { method = "PUT"
        , headers =
            [ Http.header "If-None-Match" "*"
            ]
        , url = href
        , body = Http.stringBody "text/vcard; charset=utf-8" (contactCreateVCard uid model)
        , expect = Http.expectString (ContactCreated uid)
        , timeout = Nothing
        , tracker = Nothing
        }


contactCreateVCard : String -> Model -> String
contactCreateVCard uid model =
    let
        name =
            escapeVCardText (String.trim model.draftName)

        email =
            String.trim model.draftEmail

        emailLine =
            if email == "" then
                ""

            else
                "EMAIL;TYPE=" ++ String.join "," (parseEmailTypes model.draftEmailTypes) ++ ":" ++ escapeVCardText email ++ "\r\n"
    in
    "BEGIN:VCARD\r\n"
        ++ "VERSION:4.0\r\n"
        ++ "UID:"
        ++ escapeVCardText uid
        ++ "\r\n"
        ++ "FN:"
        ++ name
        ++ "\r\n"
        ++ emailLine
        ++ "END:VCARD\r\n"


updateContact : ContactEditDraft -> Cmd Msg
updateContact draft =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "If-Match" draft.etag ]
        , url = draft.href
        , body = Http.stringBody "text/vcard; charset=utf-8" (contactEditVCard draft)
        , expect = Http.expectString (ContactUpdated draft.contactId)
        , timeout = Nothing
        , tracker = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CollectionsLoaded calendars (Ok body) ->
            case DavXml.responses body of
                Err problem -> ( { model | status = Failed problem }, Cmd.none )
                Ok responses ->
                    let
                        namespace = if calendars then "urn:ietf:params:xml:ns:caldav" else "urn:ietf:params:xml:ns:carddav"
                        kind = if calendars then "calendar" else "addressbook"
                        collections = List.filter (\resource -> DavXml.property "DAV:" "resourcetype" resource.properties |> Maybe.map (DavXml.contains namespace kind) |> Maybe.withDefault False) responses
                    in
                    if calendars then
                        ( { model | calendarEntries = [], calendarPending = List.length collections, status = Loaded }, Cmd.batch (List.map (.href >> fetchCalendarCollection) collections) )
                    else
                        case List.head collections of
                            Nothing -> ( { model | contacts = [], contactCollection = Nothing, contactsWritable = False, status = Loaded }, Cmd.none )
                            Just collection ->
                                ( { model | contactCollection = Just collection.href, contactsWritable = DavXml.property "DAV:" "current-user-privilege-set" collection.properties |> Maybe.map (DavXml.contains "DAV:" "write") |> Maybe.withDefault False }
                                , fetchContacts
                                )

        CollectionsLoaded _ (Err error) -> ( { model | status = Failed (httpErrorToString error) }, Cmd.none )

        ContactsLoaded (Ok body) ->
            case parseContactsDav False body of
                Ok contacts ->
                    let
                        selected =
                            selectedContactAfterLoad model.selectedContact contacts

                        detailCommand =
                            selected
                                |> selectedContactFrom contacts
                                |> Maybe.map fetchContactDetail
                                |> Maybe.withDefault Cmd.none
                    in
                    ( { model
                        | contacts = contacts
                        , selectedContact = selected
                        , status = Loaded
                      }
                    , detailCommand
                    )

                Err problem ->
                    ( { model | status = Failed problem }, Cmd.none )

        ContactsLoaded (Err error) ->
            ( { model | status = Failed (httpErrorToString error) }, Cmd.none )

        ContactDetailLoaded _ (Ok body) ->
            case parseContactsDav True body of
                Ok (contact :: _) ->
                    let
                        shouldStartEdit =
                            model.pendingEditContact == Just contact.id
                    in
                    ( { model
                        | contacts = replaceContact contact model.contacts
                        , status = Loaded
                        , editDraft =
                            if shouldStartEdit then
                                Just (editDraftFromContact contact)

                            else
                                model.editDraft
                        , pendingEditContact =
                            if shouldStartEdit then
                                Nothing

                            else
                                model.pendingEditContact
                      }
                    , Cmd.none
                    )

                Ok [] ->
                    ( model, Cmd.none )

                Err problem ->
                    ( { model | status = Failed problem }, Cmd.none )

        ContactDetailLoaded _ (Err error) ->
            ( { model | status = Failed (httpErrorToString error) }, Cmd.none )

        CalendarEntriesLoaded (Ok body) ->
            case parseCalendarDav body of
                Ok entries ->
                    ( { model
                        | calendarEntries = model.calendarEntries ++ entries
                        , calendarPending = max 0 (model.calendarPending - 1)
                        , selectedCalendarEntry =
                            selectedCalendarAfterLoad model.selectedCalendarEntry entries
                        , status = Loaded
                      }
                    , Cmd.none
                    )

                Err problem ->
                    ( { model | status = Failed problem }, Cmd.none )

        CalendarEntriesLoaded (Err error) ->
            ( { model | status = Failed (httpErrorToString error) }, Cmd.none )

        CurrentDateLoaded todayDate ->
            ( { model
                | todayDate = todayDate
                , calendarCursor =
                    if model.calendarCursor == defaultDate then
                        todayDate

                    else
                        model.calendarCursor
              }
            , Cmd.none
            )

        ChangePage page ->
            ( { model
                | page = page
                , creatingContact =
                    if page == ContactsPage then
                        model.creatingContact

                    else
                        False
                , editDraft =
                    if page == ContactsPage then
                        model.editDraft

                    else
                        Nothing
                , pendingEditContact =
                    if page == ContactsPage then
                        model.pendingEditContact

                    else
                        Nothing
              }
            , Cmd.none
            )

        ChangeCalendarView calendarView ->
            ( { model | calendarView = calendarView }, Cmd.none )

        NavigateCalendar delta ->
            ( { model
                | calendarCursor =
                    shiftCalendarCursor model.calendarView delta model.calendarCursor
              }
            , Cmd.none
            )

        ContactQueryChanged query ->
            let
                filtered =
                    filterContacts query model.contacts
            in
            ( { model
                | contactQuery = query
                , selectedContact = selectedContactAfterFilter model.selectedContact filtered
              }
            , Cmd.none
            )

        CalendarQueryChanged query ->
            let
                filtered =
                    filterCalendarEntries query model.calendarEntries
            in
            ( { model
                | calendarQuery = query
                , selectedCalendarEntry =
                    selectedCalendarAfterFilter model.selectedCalendarEntry filtered
              }
            , Cmd.none
            )

        SelectContact contactId ->
            let
                command =
                    selectedContactFrom model.contacts (Just contactId)
                        |> Maybe.map fetchContactDetail
                        |> Maybe.withDefault Cmd.none
            in
            ( { model
                | selectedContact = Just contactId
                , creatingContact = False
                , editDraft = Nothing
                , pendingEditContact = Nothing
              }
            , command
            )

        SelectCalendarEntry entryId ->
            ( { model | selectedCalendarEntry = Just entryId }, Cmd.none )

        Refresh ->
            ( { model
                | status = Loading
                , editDraft = Nothing
                , pendingEditContact = Nothing
              }
            , fetchAll
            )

        StartCreatingContact ->
            ( { model
                | creatingContact = True
                , selectedContact = Nothing
                , editDraft = Nothing
                , pendingEditContact = Nothing
              }
            , Cmd.none
            )

        CancelCreatingContact ->
            ( { model
                | creatingContact = False
                , draftName = ""
                , draftEmail = ""
                , draftEmailTypes = "INTERNET"
              }
            , Cmd.none
            )

        StartEditingContact ->
            case selectedContactFrom model.contacts model.selectedContact of
                Just contact ->
                    if contact.isFullDetail then
                        ( { model
                            | editDraft = Just (editDraftFromContact contact)
                            , creatingContact = False
                            , pendingEditContact = Nothing
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | status = Loading
                            , pendingEditContact = Just contact.id
                            , creatingContact = False
                          }
                        , fetchContactDetail contact
                        )

                Nothing ->
                    ( model, Cmd.none )

        CancelEditingContact ->
            ( { model
                | editDraft = Nothing
                , pendingEditContact = Nothing
                , savingContactEdit = False
              }
            , Cmd.none
            )

        DraftNameChanged value ->
            ( { model | draftName = value }, Cmd.none )

        DraftEmailChanged value ->
            ( { model | draftEmail = value }, Cmd.none )

        DraftEmailTypesChanged value ->
            ( { model | draftEmailTypes = value }, Cmd.none )

        EditNameChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | displayName = value }) model.editDraft }, Cmd.none )

        EditEmailChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | email = value }) model.editDraft }, Cmd.none )

        EditEmailTypesChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | emailTypes = value }) model.editDraft }, Cmd.none )

        EditPhoneChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | phone = value }) model.editDraft }, Cmd.none )

        EditBirthdayChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | birthday = value }) model.editDraft }, Cmd.none )

        EditDeathDateChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | deathDate = value }) model.editDraft }, Cmd.none )

        EditBirthPlaceChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | birthPlace = value }) model.editDraft }, Cmd.none )

        EditDeathPlaceChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | deathPlace = value }) model.editDraft }, Cmd.none )

        EditNoteChanged value ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | note = value }) model.editDraft }, Cmd.none )

        AddEditRelationship ->
            ( { model
                | editDraft =
                    Maybe.map
                        (\draft ->
                            { draft
                                | relationships =
                                    draft.relationships
                                        ++ [ { label = "related", name = "", target = Nothing, originalLines = [], changed = True } ]
                            }
                        )
                        model.editDraft
              }
            , Cmd.none
            )

        RemoveEditRelationship index ->
            ( { model
                | editDraft =
                    Maybe.map
                        (\draft ->
                            { draft | relationships = removeAt index draft.relationships }
                        )
                        model.editDraft
              }
            , Cmd.none
            )

        EditRelationshipLabelChanged index value ->
            ( { model
                | editDraft =
                    Maybe.map
                        (\draft ->
                            { draft
                                | relationships =
                                    updateAt index
                                        (\relationship -> { relationship | label = value, changed = True })
                                        draft.relationships
                            }
                        )
                        model.editDraft
              }
            , Cmd.none
            )

        EditRelationshipTargetChanged index target ->
            ( { model | editDraft = Maybe.map (\draft -> { draft | relationships = updateAt index (\relationship -> { relationship | changed = True, target = if target == "" then Nothing else Just target, name = List.filter (\contact -> model.origin ++ contact.href == target) model.contacts |> List.head |> Maybe.map .displayName |> Maybe.withDefault relationship.name }) draft.relationships }) model.editDraft }, Cmd.none )

        EditRelationshipNameChanged index value ->
            ( { model
                | editDraft =
                    Maybe.map
                        (\draft ->
                            { draft
                                | relationships =
                                    updateAt index
                                        (\relationship -> { relationship | name = value, target = Nothing, changed = True })
                                        draft.relationships
                            }
                        )
                        model.editDraft
              }
            , Cmd.none
            )

        SubmitContact ->
            if String.trim model.draftName == "" || model.savingContact then
                ( { model | status = Failed "Name is required" }, Cmd.none )

            else
                ( { model | savingContact = True, status = Loading }
                , Task.perform SubmitContactAt Time.now
                )

        SubmitContactAt now ->
            ( model, createContact model now )

        ContactCreated uid (Ok _) ->
            ( { model
                | contactQuery = ""
                , selectedContact = Just uid
                , status = Loading
                , creatingContact = False
                , draftName = ""
                , draftEmail = ""
                , draftEmailTypes = "INTERNET"
                , savingContact = False
              }
            , fetchContacts
            )

        ContactCreated _ (Err error) ->
            ( { model
                | status = Failed (httpErrorToString error)
                , savingContact = False
              }
            , Cmd.none
            )

        SubmitContactEdit ->
            case model.editDraft of
                Just draft ->
                    if String.trim draft.displayName == "" || model.savingContactEdit then
                        ( { model | status = Failed "Name is required" }, Cmd.none )

                    else
                        ( { model | savingContactEdit = True, status = Loading }
                        , updateContact draft
                        )

                Nothing ->
                    ( model, Cmd.none )

        ContactUpdated contactId (Ok _) ->
            ( { model
                | selectedContact = Just contactId
                , status = Loading
                , editDraft = Nothing
                , pendingEditContact = Nothing
                , savingContactEdit = False
              }
            , fetchContacts
            )

        ContactUpdated _ (Err error) ->
            ( { model
                | status = Failed (httpErrorToString error)
                , savingContactEdit = False
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "app-shell" ]
        ( [ viewHeader model
          , viewToolbar model
          ]
            ++ viewPage model
        )


viewHeader : Model -> Html Msg
viewHeader model =
    header [ class "topbar" ]
        [ div []
            [ h1 [ class "product-name" ] [ text "mates" ]
            , p [ class "product-subtitle" ] [ text (pageTitle model.page) ]
            ]
        , span [ class ("status-badge " ++ statusClass model.status) ]
            [ text (statusText model.status) ]
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    section [ class "toolbar", attribute "aria-label" "View controls" ]
        ([ div [ class "view-tabs" ]
            [ viewTab model.page ContactsPage "Contacts"
            , viewTab model.page CalendarPage "Calendar"
            ]
         , input
            [ class "search-input"
            , type_ "search"
            , placeholder (searchPlaceholder model.page)
            , value (searchValue model)
            , onInput (searchMessage model.page)
            ]
            []
         ]
            ++ contactsToolbarActions model
            ++ [ button
                    [ class "icon-button refresh-button"
                    , onClick Refresh
                    , disabled (model.status == Loading)
                    , attribute "aria-label" "Refresh"
                    , attribute "title" "Refresh"
                    ]
                    [ text "↻" ]
               ]
        )


contactsToolbarActions : Model -> List (Html Msg)
contactsToolbarActions model =
    if model.page == ContactsPage && model.contactsWritable then
        [ button
            [ class
                ("icon-button add-contact-button"
                    ++ (if model.creatingContact then
                            " is-active"

                        else
                            ""
                       )
                )
            , attribute "aria-label" "Add contact"
            , attribute "title" "Add contact"
            , onClick StartCreatingContact
            , disabled model.savingContact
            ]
            [ text "+" ]
        ]

    else
        []


viewTab : Page -> Page -> String -> Html Msg
viewTab current target labelText =
    button
        [ class
            ("tab-button"
                ++ (if current == target then
                        " is-active"

                    else
                        ""
                   )
            )
        , onClick (ChangePage target)
        ]
        [ text labelText ]


viewPage : Model -> List (Html Msg)
viewPage model =
    case model.page of
        ContactsPage ->
            viewContactsPage model

        CalendarPage ->
            viewCalendarPage model


viewContactsPage : Model -> List (Html Msg)
viewContactsPage model =
    let
        filtered =
            filterContacts model.contactQuery model.contacts

        selected =
            selectedContactFrom filtered model.selectedContact
    in
    [ main_ [ class "workspace" ]
        [ viewContactList filtered model.selectedContact
        , viewContactDetailPanel model selected
        ]
    ]


viewCalendarPage : Model -> List (Html Msg)
viewCalendarPage model =
    let
        filtered =
            filterCalendarEntries model.calendarQuery model.calendarEntries

        selected =
            selectedCalendarFrom filtered model.selectedCalendarEntry
    in
    [ viewCalendarViewbar model.calendarView model.calendarCursor
    , case model.calendarView of
        AgendaView ->
            main_ [ class "calendar-workspace" ]
                [ viewCalendarList
                    (upcomingAgendaEntries model.todayDate filtered)
                    model.selectedCalendarEntry
                , viewCalendarDetailPanel selected
                ]

        DayView ->
            main_ [ class "calendar-grid-workspace" ]
                [ viewDayCalendar
                    model.calendarCursor
                    (entriesOnDate model.calendarCursor filtered)
                    model.selectedCalendarEntry
                , viewCalendarDetailPanel selected
                ]

        WeekView ->
            main_ [ class "calendar-grid-workspace" ]
                [ viewWeekCalendar model.calendarCursor filtered model.selectedCalendarEntry
                , viewCalendarDetailPanel selected
                ]

        MonthView ->
            main_ [ class "calendar-grid-workspace" ]
                [ viewMonthCalendar model.calendarCursor filtered model.selectedCalendarEntry
                , viewCalendarDetailPanel selected
                ]
    ]


viewCalendarViewbar : CalendarView -> Date -> Html Msg
viewCalendarViewbar calendarView cursorDate =
    section [ class "calendar-viewbar", attribute "aria-label" "Calendar view" ]
        [ div [ class "view-tabs" ]
            [ viewCalendarModeTab calendarView AgendaView "Agenda"
            , viewCalendarModeTab calendarView DayView "Day"
            , viewCalendarModeTab calendarView WeekView "Week"
            , viewCalendarModeTab calendarView MonthView "Month"
            ]
        , viewCalendarNavigation calendarView cursorDate
        ]


viewCalendarNavigation : CalendarView -> Date -> Html Msg
viewCalendarNavigation calendarView cursorDate =
    case calendarView of
        AgendaView ->
            span [ class "calendar-range" ] [ text "Next upcoming" ]

        _ ->
            div [ class "calendar-navigation" ]
                [ button
                    [ class "calendar-nav-button"
                    , attribute "aria-label" "Previous"
                    , onClick (NavigateCalendar -1)
                    ]
                    [ text "<" ]
                , span [ class "calendar-range" ]
                    [ text (calendarRangeLabel calendarView cursorDate) ]
                , button
                    [ class "calendar-nav-button"
                    , attribute "aria-label" "Next"
                    , onClick (NavigateCalendar 1)
                    ]
                    [ text ">" ]
                ]


viewCalendarModeTab : CalendarView -> CalendarView -> String -> Html Msg
viewCalendarModeTab current target labelText =
    button
        [ class
            ("tab-button"
                ++ (if current == target then
                        " is-active"

                    else
                        ""
                   )
            )
        , onClick (ChangeCalendarView target)
        ]
        [ text labelText ]


viewCreateContactForm : Model -> Html Msg
viewCreateContactForm model =
    div [ class "create-contact-detail", attribute "aria-label" "Add contact" ]
        [ h2 [ class "detail-name" ] [ text "New contact" ]
        , form [ class "create-contact-form", onSubmit SubmitContact ]
            [ label [ class "field", attribute "for" "contact-name" ]
                [ span [ class "field-label" ] [ text "Name" ]
                , input
                    [ class "text-input"
                    , id "contact-name"
                    , name "displayName"
                    , type_ "text"
                    , autocomplete True
                    , placeholder "Ada Lovelace"
                    , value model.draftName
                    , onInput DraftNameChanged
                    ]
                    []
                ]
            , label [ class "field", attribute "for" "contact-email" ]
                [ span [ class "field-label" ] [ text "Email" ]
                , input
                    [ class "text-input"
                    , id "contact-email"
                    , name "email"
                    , type_ "email"
                    , autocomplete True
                    , placeholder "ada@example.test"
                    , value model.draftEmail
                    , onInput DraftEmailChanged
                    ]
                    []
                ]
            , label [ class "field", attribute "for" "contact-email-types" ]
                [ span [ class "field-label" ] [ text "Email type" ]
                , input
                    [ class "text-input"
                    , id "contact-email-types"
                    , name "emailTypes"
                    , type_ "text"
                    , placeholder "INTERNET, HOME"
                    , value model.draftEmailTypes
                    , onInput DraftEmailTypesChanged
                    ]
                    []
                ]
            , div [ class "create-actions" ]
                [ button
                    [ class "secondary-button"
                    , type_ "button"
                    , onClick CancelCreatingContact
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "create-button"
                    , type_ "submit"
                    , disabled (not (canSubmitContact model))
                    ]
                    [ text
                        (if model.savingContact then
                            "Adding"

                         else
                            "Add contact"
                        )
                    ]
                ]
            ]
        ]


viewEditContactForm : Model -> ContactEditDraft -> Html Msg
viewEditContactForm model draft =
    div [ class "edit-contact-detail", attribute "aria-label" "Edit contact" ]
        [ h2 [ class "detail-name" ] [ text "Edit contact" ]
        , form [ class "edit-contact-form", onSubmit SubmitContactEdit ]
            [ div [ class "edit-form-grid" ]
                [ label [ class "field", attribute "for" "edit-contact-name" ]
                    [ span [ class "field-label" ] [ text "Name" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-name"
                        , name "displayName"
                        , type_ "text"
                        , autocomplete True
                        , value draft.displayName
                        , onInput EditNameChanged
                        ]
                        []
                    ]
                , label [ class "field", attribute "for" "edit-contact-email" ]
                    [ span [ class "field-label" ] [ text "Primary email" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-email"
                        , name "email"
                        , type_ "email"
                        , autocomplete True
                        , value draft.email
                        , onInput EditEmailChanged
                        ]
                        []
                    ]
                , label [ class "field", attribute "for" "edit-contact-email-types" ]
                    [ span [ class "field-label" ] [ text "Email type" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-email-types"
                        , name "emailTypes"
                        , type_ "text"
                        , value draft.emailTypes
                        , onInput EditEmailTypesChanged
                        ]
                        []
                    ]
                , label [ class "field", attribute "for" "edit-contact-phone" ]
                    [ span [ class "field-label" ] [ text "Primary phone" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-phone"
                        , name "phone"
                        , type_ "tel"
                        , autocomplete True
                        , value draft.phone
                        , onInput EditPhoneChanged
                        ]
                        []
                    ]
                , label [ class "field", attribute "for" "edit-contact-birthday" ]
                    [ span [ class "field-label" ] [ text "Birthday" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-birthday"
                        , name "birthday"
                        , type_ "text"
                        , placeholder "YYYY-MM-DD"
                        , value draft.birthday
                        , onInput EditBirthdayChanged
                        ]
                        []
                    ]
                , label [ class "field", attribute "for" "edit-contact-death-date" ]
                    [ span [ class "field-label" ] [ text "Death date" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-death-date"
                        , name "deathDate"
                        , type_ "text"
                        , placeholder "YYYY-MM-DD"
                        , value draft.deathDate
                        , onInput EditDeathDateChanged
                        ]
                        []
                    ]
                , label [ class "field", attribute "for" "edit-contact-birth-place" ]
                    [ span [ class "field-label" ] [ text "Birth place" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-birth-place"
                        , name "birthPlace"
                        , type_ "text"
                        , value draft.birthPlace
                        , onInput EditBirthPlaceChanged
                        ]
                        []
                    ]
                , label [ class "field", attribute "for" "edit-contact-death-place" ]
                    [ span [ class "field-label" ] [ text "Death place" ]
                    , input
                        [ class "text-input"
                        , id "edit-contact-death-place"
                        , name "deathPlace"
                        , type_ "text"
                        , value draft.deathPlace
                        , onInput EditDeathPlaceChanged
                        ]
                        []
                    ]
                ]
            , label [ class "field", attribute "for" "edit-contact-note" ]
                [ span [ class "field-label" ] [ text "Note" ]
                , textarea
                    [ class "text-input text-area"
                    , id "edit-contact-note"
                    , name "note"
                    , attribute "rows" "4"
                    , value draft.note
                    , onInput EditNoteChanged
                    ]
                    []
                ]
            , div [ class "relationship-editor" ]
                [ div [ class "relationship-editor-heading" ]
                    [ span [ class "field-label" ] [ text "Relationships" ]
                    , button
                        [ class "secondary-button compact-button"
                        , type_ "button"
                        , onClick AddEditRelationship
                        ]
                        [ text "Add relation" ]
                    ]
                , div [ class "relationship-list" ]
                    (if List.isEmpty draft.relationships then
                        [ div [ class "muted" ] [ text "No relationships." ] ]

                     else
                        List.indexedMap (viewRelationshipDraftRow model) draft.relationships
                    )
                ]
            , div [ class "create-actions" ]
                [ button
                    [ class "secondary-button"
                    , type_ "button"
                    , onClick CancelEditingContact
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "create-button"
                    , type_ "submit"
                    , disabled (not (canSubmitContactEdit model draft))
                    ]
                    [ text
                        (if model.savingContactEdit then
                            "Saving"

                         else
                            "Save"
                        )
                    ]
                ]
            ]
        ]


viewRelationshipDraftRow : Model -> Int -> RelationshipDraft -> Html Msg
viewRelationshipDraftRow model index relationship =
    div [ class "relationship-row" ]
        [ label [ class "field relationship-kind-field" ]
            [ span [ class "field-label" ] [ text "Type" ]
            , input
                [ class "text-input"
                , type_ "text"
                , value relationship.label
                , placeholder "mother"
                , onInput (EditRelationshipLabelChanged index)
                ]
                []
            ]
        , label [ class "field relationship-name-field" ]
            [ span [ class "field-label" ] [ text "Name" ]
            , input
                [ class "text-input"
                , type_ "text"
                , value relationship.name
                , placeholder "Annabella Milbanke"
                , onInput (EditRelationshipNameChanged index)
                ]
                []
            ]
        , label [ class "field" ]
            [ span [ class "field-label" ] [ text "Link to contact" ]
            , select [ class "text-input", onInput (EditRelationshipTargetChanged index) ]
                (option [ value "", selected (relationship.target == Nothing) ] [ text "Keep name only" ]
                    :: List.map (\contact -> let target = model.origin ++ contact.href in option [ value target, selected (relationship.target == Just target) ] [ text (contact.displayName ++ " · " ++ contact.id) ]) (List.filter (\contact -> Just contact.id /= model.selectedContact) model.contacts))
            ]
        , button
            [ class "icon-button relationship-remove-button"
            , type_ "button"
            , attribute "aria-label" "Remove relationship"
            , attribute "title" "Remove relationship"
            , onClick (RemoveEditRelationship index)
            ]
            [ text "-" ]
        ]


viewContactList : List Contact -> Maybe String -> Html Msg
viewContactList contacts selected =
    section [ class "contact-list-panel", attribute "aria-label" "Contacts" ]
        [ div [ class "panel-heading" ]
            [ h2 [] [ text "Contacts" ]
            , span [ class "result-count" ] [ text (String.fromInt (List.length contacts)) ]
            ]
        , if List.isEmpty contacts then
            div [ class "empty-state" ] [ text "No contacts found." ]

          else
            ul [ class "contact-list" ] (List.map (viewContactRow selected) contacts)
        ]


viewContactRow : Maybe String -> Contact -> Html Msg
viewContactRow selected contact =
    let
        selectedClass =
            case selected of
                Just contactId ->
                    if contactId == contact.id then
                        " is-selected"

                    else
                        ""

                Nothing ->
                    ""
    in
    li []
        [ button
            [ class ("contact-row" ++ selectedClass)
            , onClick (SelectContact contact.id)
            ]
            [ span [ class "contact-name" ] [ text contact.displayName ]
            , span [ class "contact-email" ] [ text (primaryContactLine contact) ]
            ]
        ]


viewContactDetailPanel : Model -> Maybe Contact -> Html Msg
viewContactDetailPanel model selectedContact =
    section [ class "detail-panel", attribute "aria-label" "Contact details" ]
        [ div [ class "detail-surface" ]
            [ case ( model.creatingContact, model.editDraft, selectedContact ) of
                ( True, _, _ ) ->
                    viewCreateContactForm model

                ( _, Just draft, _ ) ->
                    viewEditContactForm model draft

                ( _, Nothing, Just contact ) ->
                    viewContactDetail model.contactsWritable contact

                _ ->
                    div [ class "empty-state detail-empty" ]
                        [ text "No contact selected." ]
            ]
        ]


viewContactDetail : Bool -> Contact -> Html Msg
viewContactDetail writable contact =
    div []
        (viewContactPhoto contact
            ++ (div [ class "detail-title-row" ]
                    [ h2 [ class "detail-name" ] [ text contact.displayName ]
                    , if writable then button
                        [ class "secondary-button compact-button"
                        , type_ "button"
                        , onClick StartEditingContact
                        ]
                        [ text "Edit" ] else text ""
                    ]
                    :: (viewFamilyTree contact ++ viewContactFieldSections contact)
               )
        )


viewContactPhoto : Contact -> List (Html Msg)
viewContactPhoto contact =
    case contact.photo of
        Just photo ->
            [ div [ class "contact-photo-shell" ]
                [ img
                    [ class "contact-photo"
                    , src photo.source
                    , alt (contact.displayName ++ " photo")
                    ]
                    []
                ]
            ]

        Nothing ->
            []


viewFamilyTree : Contact -> List (Html Msg)
viewFamilyTree contact =
    let
        groups =
            familyGroups contact
    in
    if familyGroupsEmpty groups then
        []

    else
        [ div [ class "detail-section family-tree-section" ]
            [ span [ class "section-label" ] [ text "Family tree" ]
            , div [ class "family-tree" ]
                (viewFamilyGeneration "Parents" groups.parents
                    ++ [ div [ class "family-middle-row" ]
                            [ viewFamilySide "Siblings" groups.siblings
                            , div [ class "family-self-node" ]
                                [ span [ class "family-self-label" ] [ text "Person" ]
                                , span [ class "family-self-name" ] [ text contact.displayName ]
                                ]
                            , viewFamilySide "Partners" groups.partners
                            ]
                       ]
                    ++ viewFamilyGeneration "Children" groups.children
                    ++ viewFamilyGeneration "Other relations" groups.other
                )
            ]
        ]


viewFamilyGeneration : String -> List FamilyRelation -> List (Html Msg)
viewFamilyGeneration labelText relations =
    if List.isEmpty relations then
        []

    else
        [ div [ class "family-generation" ]
            [ span [ class "family-generation-label" ] [ text labelText ]
            , ul [ class "family-node-list" ] (List.map viewFamilyNode relations)
            ]
        ]


viewFamilySide : String -> List FamilyRelation -> Html Msg
viewFamilySide labelText relations =
    if List.isEmpty relations then
        div [ class "family-side is-empty" ] []

    else
        div [ class "family-side" ]
            [ span [ class "family-generation-label" ] [ text labelText ]
            , ul [ class "family-node-list" ] (List.map viewFamilyNode relations)
            ]


viewFamilyNode : FamilyRelation -> Html Msg
viewFamilyNode relation =
    li [ class "family-node" ]
        [ span [ class "family-node-name" ] [ text relation.name ]
        , span [ class "family-node-label" ] [ text relation.label ]
        ]


familyGroups : Contact -> FamilyGroups
familyGroups contact =
    contact.fields
        |> List.filter (\field -> field.name == "Related")
        |> List.foldl addFamilyRelation emptyFamilyGroups


emptyFamilyGroups : FamilyGroups
emptyFamilyGroups =
    { parents = []
    , partners = []
    , siblings = []
    , children = []
    , other = []
    }


addFamilyRelation : ContactField -> FamilyGroups -> FamilyGroups
addFamilyRelation field groups =
    let
        relation =
            { label = relationshipDisplayLabel field
            , name = field.value
            }
    in
    case relationshipBucket relation.label of
        ParentBucket ->
            { groups | parents = groups.parents ++ [ relation ] }

        PartnerBucket ->
            { groups | partners = groups.partners ++ [ relation ] }

        SiblingBucket ->
            { groups | siblings = groups.siblings ++ [ relation ] }

        ChildBucket ->
            { groups | children = groups.children ++ [ relation ] }

        OtherBucket ->
            { groups | other = groups.other ++ [ relation ] }


relationshipDisplayLabel : ContactField -> String
relationshipDisplayLabel field =
    case field.label of
        Just label ->
            label

        Nothing ->
            "related"


relationshipBucket : String -> FamilyBucket
relationshipBucket labelText =
    case normalizedRelationshipLabel labelText of
        "parent" ->
            ParentBucket

        "mother" ->
            ParentBucket

        "father" ->
            ParentBucket

        "spouse" ->
            PartnerBucket

        "partner" ->
            PartnerBucket

        "husband" ->
            PartnerBucket

        "wife" ->
            PartnerBucket

        "sibling" ->
            SiblingBucket

        "brother" ->
            SiblingBucket

        "sister" ->
            SiblingBucket

        "child" ->
            ChildBucket

        "daughter" ->
            ChildBucket

        "son" ->
            ChildBucket

        _ ->
            OtherBucket


normalizedRelationshipLabel : String -> String
normalizedRelationshipLabel labelText =
    labelText
        |> String.trim
        |> String.toLower
        |> String.replace "_" "-"
        |> String.replace " " "-"


familyGroupsEmpty : FamilyGroups -> Bool
familyGroupsEmpty groups =
    List.isEmpty groups.parents
        && List.isEmpty groups.partners
        && List.isEmpty groups.siblings
        && List.isEmpty groups.children
        && List.isEmpty groups.other


viewContactFieldSections : Contact -> List (Html Msg)
viewContactFieldSections contact =
    let
        fields =
            contactFieldsForDetail contact

        sections =
            List.concatMap (viewContactFieldSection fields) contactFieldOrder
    in
    if List.isEmpty sections then
        [ div [ class "detail-section" ]
            [ p [ class "muted" ] [ text "No contact details." ] ]
        ]

    else
        sections


contactFieldsForDetail : Contact -> List ContactField
contactFieldsForDetail contact =
    case contact.photo of
        Just _ ->
            List.filter (\field -> field.name /= "Image") contact.fields

        Nothing ->
            contact.fields


contactFieldOrder : List String
contactFieldOrder =
    [ "Email"
    , "Phone"
    , "Address"
    , "URL"
    , "Organization"
    , "Title"
    , "Role"
    , "Nickname"
    , "Birthday"
    , "Death date"
    , "Anniversary"
    , "Birth place"
    , "Death place"
    , "Related"
    , "Image"
    , "Note"
    ]


viewContactFieldSection : List ContactField -> String -> List (Html Msg)
viewContactFieldSection fields sectionName =
    let
        sectionFields =
            List.filter (\field -> field.name == sectionName) fields
    in
    if List.isEmpty sectionFields then
        []

    else
        [ div [ class "detail-section" ]
            [ span [ class "section-label" ] [ text sectionName ]
            , ul [ class "contact-field-list" ]
                (List.map viewContactField sectionFields)
            ]
        ]


viewContactField : ContactField -> Html Msg
viewContactField field =
    li [ class "contact-field-row" ]
        [ span [ class "contact-field-value" ] [ text field.value ]
        , span [ class "contact-field-label" ] [ text (fieldLabelText field) ]
        ]


fieldLabelText : ContactField -> String
fieldLabelText field =
    case field.label of
        Just label ->
            label

        Nothing ->
            ""


viewCalendarList : List CalendarEntry -> Maybe String -> Html Msg
viewCalendarList entries selected =
    section [ class "agenda-panel", attribute "aria-label" "Calendar entries" ]
        [ div [ class "panel-heading" ]
            [ h2 [] [ text "Agenda" ]
            , span [ class "result-count" ] [ text (String.fromInt (List.length entries)) ]
            ]
        , if List.isEmpty entries then
            div [ class "empty-state" ] [ text "No calendar entries found." ]

          else
            ul [ class "agenda-list" ] (List.map (viewCalendarRow selected) entries)
        ]


viewCalendarRow : Maybe String -> CalendarEntry -> Html Msg
viewCalendarRow selected entry =
    let
        selectedClass =
            case selected of
                Just entryId ->
                    if entryId == entry.id then
                        " is-selected"

                    else
                        ""

                Nothing ->
                    ""
    in
    li []
        [ button
            [ class ("agenda-row" ++ selectedClass)
            , onClick (SelectCalendarEntry entry.id)
            ]
            [ span [ class "agenda-date" ] [ text (shortDate entry.startsAt) ]
            , span [ class "agenda-copy" ]
                [ span [ class "agenda-title" ] [ text entry.title ]
                , span [ class "agenda-meta" ]
                    [ text (timeRange entry ++ " | " ++ entry.calendarName) ]
                ]
            ]
        ]


viewCalendarDetailPanel : Maybe CalendarEntry -> Html Msg
viewCalendarDetailPanel selectedEntry =
    section [ class "detail-panel", attribute "aria-label" "Calendar entry details" ]
        [ div [ class "detail-surface" ]
            [ case selectedEntry of
                Just entry ->
                    viewCalendarDetail entry

                Nothing ->
                    div [ class "empty-state detail-empty" ]
                        [ text "No calendar entry selected." ]
            ]
        ]


viewCalendarDetail : CalendarEntry -> Html Msg
viewCalendarDetail entry =
    div []
        ([ h2 [ class "detail-name" ] [ text entry.title ]
         , div [ class "detail-section detail-grid" ]
            [ viewDetailItem "Calendar" entry.calendarName
            , viewDetailItem "Date" (prettyDate entry.startsAt)
            , viewDetailItem "Time" (timeRange entry)
            ]
         ]
            ++ optionalDetailItems entry
        )


viewDayCalendar : Date -> List CalendarEntry -> Maybe String -> Html Msg
viewDayCalendar day entries selected =
    section [ class "day-panel", attribute "aria-label" "Day calendar" ]
        [ div [ class "panel-heading" ]
            [ h2 [] [ text "Day" ]
            , span [ class "result-count" ] [ text (String.fromInt (List.length entries)) ]
            ]
        , div [ class "day-calendar" ]
            [ viewWeekTimeAxis
            , viewWeekDayColumn selected entries (weekDayFromDate day)
            ]
        ]


viewWeekCalendar : Date -> List CalendarEntry -> Maybe String -> Html Msg
viewWeekCalendar cursorDate entries selected =
    let
        days =
            weekDates cursorDate
    in
    section [ class "week-panel", attribute "aria-label" "Week calendar" ]
        [ div [ class "panel-heading" ]
            [ h2 [] [ text "Week" ]
            , span [ class "result-count" ]
                [ text (String.fromInt (List.length (entriesInDates days entries))) ]
            ]
        , div [ class "week-calendar" ]
            ( [ div [ class "week-header-spacer" ] []
              ]
                ++ List.map viewWeekHeader (List.map weekDayFromDate days)
                ++ [ viewWeekTimeAxis ]
                ++ List.map
                    (viewWeekDayColumn selected entries << weekDayFromDate)
                    days
            )
        ]


viewMonthCalendar : Date -> List CalendarEntry -> Maybe String -> Html Msg
viewMonthCalendar cursorDate entries selected =
    let
        days =
            monthGridDates cursorDate
    in
    section [ class "month-panel", attribute "aria-label" "Month calendar" ]
        [ div [ class "panel-heading" ]
            [ h2 [] [ text "Month" ]
            , span [ class "result-count" ]
                [ text (String.fromInt (List.length (entriesInMonth cursorDate entries))) ]
            ]
        , div [ class "month-calendar" ]
            (List.map viewMonthWeekday weekdayNames
                ++ List.map
                    (viewMonthDay selected cursorDate entries)
                    days
            )
        ]


viewMonthWeekday : String -> Html Msg
viewMonthWeekday weekday =
    div [ class "month-weekday-header" ] [ text weekday ]


viewMonthDay : Maybe String -> Date -> List CalendarEntry -> Date -> Html Msg
viewMonthDay selected cursorDate entries day =
    let
        dayEntries =
            entriesOnDate day entries

        outsideClass =
            if day.month == cursorDate.month then
                ""

            else
                " is-outside"
    in
    div [ class ("month-day-cell" ++ outsideClass) ]
        [ span [ class "month-day-number" ] [ text (String.fromInt day.day) ]
        , div [ class "month-events" ]
            (List.map (viewMonthEntry selected) dayEntries)
        ]


viewMonthEntry : Maybe String -> CalendarEntry -> Html Msg
viewMonthEntry selected entry =
    button
        [ class ("month-event" ++ selectedEventClass selected entry.id)
        , onClick (SelectCalendarEntry entry.id)
        ]
        [ span [ class "month-event-title" ] [ text entry.title ]
        , span [ class "month-event-time" ] [ text (monthEventTime entry) ]
        ]


monthEventTime : CalendarEntry -> String
monthEventTime entry =
    if entry.allDay then
        "All day"

    else
        timePart entry.startsAt


viewWeekHeader : WeekDay -> Html Msg
viewWeekHeader day =
    div [ class "week-day-header" ]
        [ span [ class "week-day-name" ] [ text day.weekday ]
        , span [ class "week-day-date" ] [ text day.label ]
        ]


viewWeekTimeAxis : Html Msg
viewWeekTimeAxis =
    div [ class "week-time-axis" ]
        (div [ class "week-all-day-label" ] [ text "All day" ]
            :: List.map viewTimeAxisLabel weekHourLabels
        )


viewTimeAxisLabel : Int -> Html Msg
viewTimeAxisLabel hour =
    div [ class "week-time-label" ] [ text (hourLabel hour) ]


viewWeekDayColumn : Maybe String -> List CalendarEntry -> WeekDay -> Html Msg
viewWeekDayColumn selected entries day =
    let
        dayEntries =
            List.filter (\entry -> datePart entry.startsAt == day.isoDate) entries

        allDayEntries =
            List.filter .allDay dayEntries

        timedEntries =
            List.filter (\entry -> not entry.allDay) dayEntries
    in
    div [ class "week-day-column" ]
        [ div [ class "week-all-day-row" ]
            (if List.isEmpty allDayEntries then
                [ span [ class "week-empty-all-day" ] [] ]

             else
                List.map (viewWeekAllDayEntry selected) allDayEntries
            )
        , div [ class "week-time-column" ]
            (List.map viewWeekSlot (List.range 1 weekSlotCount)
                ++ List.map (viewWeekTimedEntry selected) timedEntries
            )
        ]


viewWeekSlot : Int -> Html Msg
viewWeekSlot slotIndex =
    div
        [ class
            ("week-slot"
                ++ (if modBy 2 slotIndex == 1 then
                        " is-hour"

                    else
                        ""
                   )
            )
        , style "grid-row" (String.fromInt slotIndex)
        ]
        []


viewWeekAllDayEntry : Maybe String -> CalendarEntry -> Html Msg
viewWeekAllDayEntry selected entry =
    button
        [ class ("week-event week-all-day-event" ++ selectedEventClass selected entry.id)
        , onClick (SelectCalendarEntry entry.id)
        ]
        [ span [ class "week-event-title" ] [ text entry.title ]
        , span [ class "week-event-meta" ] [ text entry.calendarName ]
        ]


viewWeekTimedEntry : Maybe String -> CalendarEntry -> Html Msg
viewWeekTimedEntry selected entry =
    button
        [ class ("week-event week-timed-event" ++ selectedEventClass selected entry.id)
        , style "grid-row" (eventGridStart entry ++ " / span " ++ eventGridSpan entry)
        , onClick (SelectCalendarEntry entry.id)
        ]
        [ span [ class "week-event-title" ] [ text entry.title ]
        , span [ class "week-event-meta" ] [ text (timeRange entry) ]
        ]


selectedEventClass : Maybe String -> String -> String
selectedEventClass selected entryId =
    case selected of
        Just selectedId ->
            if selectedId == entryId then
                " is-selected"

            else
                ""

        Nothing ->
            ""


viewDetailItem : String -> String -> Html Msg
viewDetailItem labelText valueText =
    div [ class "detail-item" ]
        [ span [ class "section-label" ] [ text labelText ]
        , p [] [ text valueText ]
        ]


optionalDetailItems : CalendarEntry -> List (Html Msg)
optionalDetailItems entry =
    List.filterMap identity
        [ optionalDetailItem "Location" entry.location
        , optionalDetailItem "Description" entry.description
        ]


optionalDetailItem : String -> Maybe String -> Maybe (Html Msg)
optionalDetailItem labelText maybeValue =
    case maybeValue of
        Just valueText ->
            if String.trim valueText == "" then
                Nothing

            else
                Just (viewDetailItem labelText valueText)

        Nothing ->
            Nothing


parseContactsDav : Bool -> String -> Result String (List Contact)
parseContactsDav isFullDetail body =
    DavXml.responses body |> Result.map (\_ ->
        (body
            |> extractDavData "address-data"
            |> List.filterMap (contactFromDavData isFullDetail)
            |> List.sortBy (.displayName >> String.toLower)
        ))


contactFromDavData : Bool -> DavData -> Maybe Contact
contactFromDavData isFullDetail davData =
    let
        properties =
            parseProperties davData.body

        displayName =
            firstPropertyValue "FN" properties
                |> Maybe.withDefault (firstPropertyValue "N" properties |> Maybe.withDefault "Unnamed contact")

        uid =
            firstPropertyValue "UID" properties
                |> Maybe.withDefault (idFromHref davData.href)

        emails =
            properties
                |> propertiesNamed "EMAIL"
                |> List.map emailFromProperty

        fields =
            List.concat
                [ List.map emailFieldFromProperty (propertiesNamed "EMAIL" properties)
                , List.map (fieldFromProperty "Phone") (propertiesNamed "TEL" properties)
                , List.map addressFieldFromProperty (propertiesNamed "ADR" properties)
                , List.map (fieldFromProperty "URL") (propertiesNamed "URL" properties)
                , List.map organizationFieldFromProperty (propertiesNamed "ORG" properties)
                , List.map (fieldFromProperty "Title") (propertiesNamed "TITLE" properties)
                , List.map (fieldFromProperty "Role") (propertiesNamed "ROLE" properties)
                , List.map (fieldFromProperty "Nickname") (propertiesNamed "NICKNAME" properties)
                , List.map (fieldFromProperty "Birthday") (propertiesNamed "BDAY" properties)
                , List.map (fieldFromProperty "Death date") (propertiesNamed "DEATHDATE" properties)
                , List.map (fieldFromProperty "Anniversary") (propertiesNamed "ANNIVERSARY" properties)
                , List.map (fieldFromProperty "Birth place") (propertiesNamed "BIRTHPLACE" properties)
                , List.map (fieldFromProperty "Death place") (propertiesNamed "DEATHPLACE" properties)
                , relatedFields properties
                , List.map (fieldFromProperty "Note") (propertiesNamed "NOTE" properties)
                ]
    in
    Just
        { href = davData.href
        , id = uid
        , displayName = unescapeCardText displayName
        , emails = emails
        , fields = fields
        , photo = contactPhotoFromProperties properties
        , etag = davData.etag
        , rawBody = davData.body
        , isFullDetail = isFullDetail
        }


emailFromProperty : CardProperty -> Email
emailFromProperty property =
    { address = unescapeCardText property.value
    , types = typeValues property
    }


emailFieldFromProperty : CardProperty -> ContactField
emailFieldFromProperty property =
    { name = "Email"
    , label = typeLabel property
    , value = unescapeCardText property.value
    }


fieldFromProperty : String -> CardProperty -> ContactField
fieldFromProperty fieldName property =
    { name = fieldName
    , label = typeLabel property
    , value = unescapeCardText property.value
    }


addressFieldFromProperty : CardProperty -> ContactField
addressFieldFromProperty property =
    { name = "Address"
    , label = typeLabel property
    , value =
        property.value
            |> String.split ";"
            |> List.map (unescapeCardText >> String.trim)
            |> List.filter (\part -> part /= "")
            |> String.join ", "
    }


organizationFieldFromProperty : CardProperty -> ContactField
organizationFieldFromProperty property =
    { name = "Organization"
    , label = typeLabel property
    , value =
        property.value
            |> String.split ";"
            |> List.map (unescapeCardText >> String.trim)
            |> List.filter (\part -> part /= "")
            |> String.join ", "
    }


relatedFields : List CardProperty -> List ContactField
relatedFields properties =
    properties
        |> List.filter (\property -> property.name == "RELATED" || property.name == "X-ABRELATEDNAMES")
        |> List.map (relatedField properties)


relatedField : List CardProperty -> CardProperty -> ContactField
relatedField properties property =
    { name = "Related"
    , label = Just (relatedLabel properties property)
    , value = unescapeCardText property.value
    }


relatedLabel : List CardProperty -> CardProperty -> String
relatedLabel properties property =
    case property.group of
        Just groupName ->
            case
                properties
                    |> List.filter (\candidate -> candidate.name == "X-ABLABEL" && candidate.group == Just groupName)
                    |> List.head
            of
                Just labelProperty ->
                    cleanAppleLabel (unescapeCardText labelProperty.value)

                Nothing ->
                    typeLabel property |> Maybe.withDefault "related"

        Nothing ->
            typeLabel property |> Maybe.withDefault "related"


cleanAppleLabel : String -> String
cleanAppleLabel label =
    if String.startsWith "_$!<" label && String.endsWith ">!$_" label then
        label
            |> String.dropLeft 4
            |> String.dropRight 4

    else
        label


contactPhotoFromProperties : List CardProperty -> Maybe ContactPhoto
contactPhotoFromProperties properties =
    properties
        |> List.filter (\property -> property.name == "PHOTO" || property.name == "LOGO")
        |> List.head
        |> Maybe.map photoFromProperty


photoFromProperty : CardProperty -> ContactPhoto
photoFromProperty property =
    let
        contentType =
            photoContentType property

        cleanedValue =
            unescapeCardText property.value
    in
    { contentType = contentType
    , source =
        if String.startsWith "http://" cleanedValue || String.startsWith "https://" cleanedValue || String.startsWith "data:" cleanedValue then
            cleanedValue

        else
            "data:" ++ contentType ++ ";base64," ++ cleanedValue
    }


photoContentType : CardProperty -> String
photoContentType property =
    case typeValues property of
        value :: _ ->
            if String.contains "/" value then
                String.toLower value

            else
                "image/" ++ String.toLower value

        [] ->
            "image/jpeg"


parseCalendarDav : String -> Result String (List CalendarEntry)
parseCalendarDav body =
    DavXml.responses body |> Result.map (\_ ->
        (body
            |> extractDavData "calendar-data"
            |> List.concatMap calendarEntriesFromDavData
            |> List.sortBy .startsAt
        ))


calendarEntriesFromDavData : DavData -> List CalendarEntry
calendarEntriesFromDavData davData =
    davData.body
        |> unfoldedLines
        |> componentBlocks "VEVENT"
        |> List.filterMap (calendarEntryFromBlock davData.href)


calendarEntryFromBlock : String -> List String -> Maybe CalendarEntry
calendarEntryFromBlock href block =
    let
        properties =
            List.filterMap parseProperty block

        maybeStart =
            firstProperty "DTSTART" properties

        calendarName =
            calendarNameFromHref href
    in
    case maybeStart of
        Just startProperty ->
            let
                uid =
                    firstPropertyValue "UID" properties
                        |> Maybe.withDefault (idFromHref href)

                startsAt =
                    formatICalDateTime startProperty.value
            in
            Just
                { id = calendarName ++ ":" ++ uid
                , calendarName = calendarName
                , title =
                    firstPropertyValue "SUMMARY" properties
                        |> Maybe.withDefault "(untitled)"
                        |> unescapeCardText
                , startsAt = startsAt
                , endsAt =
                    firstProperty "DTEND" properties
                        |> Maybe.map (.value >> formatICalDateTime)
                , location =
                    firstPropertyValue "LOCATION" properties
                        |> Maybe.map unescapeCardText
                , description =
                    firstPropertyValue "DESCRIPTION" properties
                        |> Maybe.map unescapeCardText
                , allDay = isAllDayDate startProperty
                }

        Nothing ->
            Nothing


extractDavData : String -> String -> List DavData
extractDavData localName body =
    let
        namespace = if localName == "address-data" then "urn:ietf:params:xml:ns:carddav" else "urn:ietf:params:xml:ns:caldav"
    in
    DavXml.responses body
        |> Result.withDefault []
        |> List.filterMap (\response -> DavXml.property namespace localName response.properties |> Maybe.map (\data -> { href = response.href, body = data.content, etag = DavXml.property "DAV:" "getetag" response.properties |> Maybe.map .content |> Maybe.withDefault "" }))


xmlUnescape : String -> String
xmlUnescape value =
    value
        |> String.replace "&lt;" "<"
        |> String.replace "&gt;" ">"
        |> String.replace "&quot;" "\""
        |> String.replace "&apos;" "'"
        |> String.replace "&amp;" "&"


escapeXmlText : String -> String
escapeXmlText value =
    value
        |> String.replace "&" "&amp;"
        |> String.replace "<" "&lt;"
        |> String.replace ">" "&gt;"
        |> String.replace "\"" "&quot;"
        |> String.replace "'" "&apos;"


parseProperties : String -> List CardProperty
parseProperties =
    unfoldedLines >> List.filterMap parseProperty


unfoldedLines : String -> List String
unfoldedLines raw =
    raw
        |> String.lines
        |> List.map stripCarriageReturn
        |> List.foldl unfoldLine []
        |> List.reverse


unfoldLine : String -> List String -> List String
unfoldLine line accumulated =
    if String.startsWith " " line || String.startsWith "\t" line then
        case accumulated of
            previous :: rest ->
                (previous ++ String.dropLeft 1 line) :: rest

            [] ->
                [ String.dropLeft 1 line ]

    else
        line :: accumulated


stripCarriageReturn : String -> String
stripCarriageReturn value =
    if String.endsWith "\u{000D}" value then
        String.dropRight 1 value

    else
        value


parseProperty : String -> Maybe CardProperty
parseProperty line =
    case splitFirst ":" line of
        Just ( rawName, rawValue ) ->
            let
                parts =
                    String.split ";" rawName

                namePart =
                    List.head parts |> Maybe.withDefault ""

                params =
                    List.drop 1 parts

                groupAndName =
                    splitGroupAndName namePart
            in
            Just
                { group = Tuple.first groupAndName
                , name = String.toUpper (Tuple.second groupAndName)
                , params = List.map parseParameter params
                , value = rawValue
                }

        Nothing ->
            Nothing


splitGroupAndName : String -> ( Maybe String, String )
splitGroupAndName rawName =
    case splitFirst "." rawName of
        Just ( groupName, name ) ->
            ( Just groupName, name )

        Nothing ->
            ( Nothing, rawName )


parseParameter : String -> ( String, String )
parseParameter raw =
    case splitFirst "=" raw of
        Just ( key, value ) ->
            ( String.toUpper key, trimQuotes value )

        Nothing ->
            ( "TYPE", trimQuotes raw )


trimQuotes : String -> String
trimQuotes value =
    if String.startsWith "\"" value && String.endsWith "\"" value then
        value |> String.dropLeft 1 |> String.dropRight 1

    else
        value


propertiesNamed : String -> List CardProperty -> List CardProperty
propertiesNamed name properties =
    List.filter (\property -> property.name == name) properties


firstProperty : String -> List CardProperty -> Maybe CardProperty
firstProperty name properties =
    propertiesNamed name properties |> List.head


firstPropertyValue : String -> List CardProperty -> Maybe String
firstPropertyValue name properties =
    firstProperty name properties |> Maybe.map .value


typeValues : CardProperty -> List String
typeValues property =
    property.params
        |> List.filter (\( key, _ ) -> key == "TYPE")
        |> List.concatMap (\( _, value ) -> String.split "," value)
        |> List.map (String.trim >> String.toUpper)
        |> List.filter (\value -> value /= "")


typeLabel : CardProperty -> Maybe String
typeLabel property =
    case typeValues property of
        [] ->
            Nothing

        values ->
            Just (String.join ", " values)


componentBlocks : String -> List String -> List (List String)
componentBlocks componentName lines =
    componentBlocksHelp componentName lines False [] []


componentBlocksHelp : String -> List String -> Bool -> List String -> List (List String) -> List (List String)
componentBlocksHelp componentName lines inside current found =
    case lines of
        [] ->
            List.reverse found

        line :: rest ->
            if line == "BEGIN:" ++ componentName then
                componentBlocksHelp componentName rest True [] found

            else if line == "END:" ++ componentName && inside then
                componentBlocksHelp componentName rest False [] (List.reverse current :: found)

            else if inside then
                componentBlocksHelp componentName rest True (line :: current) found

            else
                componentBlocksHelp componentName rest False current found


formatICalDateTime : String -> String
formatICalDateTime raw =
    let
        value =
            raw |> String.trim |> String.dropRight (if String.endsWith "Z" raw then 1 else 0)
    in
    if String.length value >= 15 then
        String.slice 0 4 value
            ++ "-"
            ++ String.slice 4 6 value
            ++ "-"
            ++ String.slice 6 8 value
            ++ "T"
            ++ String.slice 9 11 value
            ++ ":"
            ++ String.slice 11 13 value
            ++ ":"
            ++ String.slice 13 15 value

    else if String.length value == 8 then
        String.slice 0 4 value
            ++ "-"
            ++ String.slice 4 6 value
            ++ "-"
            ++ String.slice 6 8 value

    else
        value


isAllDayDate : CardProperty -> Bool
isAllDayDate property =
    String.length (String.trim property.value) == 8
        || List.member ( "VALUE", "DATE" ) property.params


calendarNameFromHref : String -> String
calendarNameFromHref href =
    case String.split "/" href of
        "" :: "dav" :: "calendars" :: calendarName :: _ ->
            calendarName

        "dav" :: "calendars" :: calendarName :: _ ->
            calendarName

        _ ->
            "calendar"


idFromHref : String -> String
idFromHref href =
    href
        |> String.split "/"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault href
        |> stripSuffix ".vcf"
        |> stripSuffix ".ics"


stripSuffix : String -> String -> String
stripSuffix suffix value =
    if String.endsWith suffix value then
        String.dropRight (String.length suffix) value

    else
        value


splitFirst : String -> String -> Maybe ( String, String )
splitFirst separator value =
    case firstIndexOf separator value of
        Just index ->
            Just
                ( String.left index value
                , String.dropLeft (index + String.length separator) value
                )

        Nothing ->
            Nothing


firstIndexOf : String -> String -> Maybe Int
firstIndexOf needle haystack =
    String.indexes needle haystack |> List.head


beforeString : String -> String -> String
beforeString needle value =
    case firstIndexOf needle value of
        Just index ->
            String.left index value

        Nothing ->
            value


unescapeCardText : String -> String
unescapeCardText value =
    value
        |> String.replace "\\n" "\n"
        |> String.replace "\\N" "\n"
        |> String.replace "\\," ","
        |> String.replace "\\;" ";"
        |> String.replace "\\\\" "\\"


escapeVCardText : String -> String
escapeVCardText value =
    value
        |> String.replace "\\" "\\\\"
        |> String.replace "\n" "\\n"
        |> String.replace "," "\\,"
        |> String.replace ";" "\\;"


editDraftFromContact : Contact -> ContactEditDraft
editDraftFromContact contact =
    let
        primaryEmail =
            List.head contact.emails

        emailAddress =
            primaryEmail
                |> Maybe.map .address
                |> Maybe.withDefault ""

        emailTypes =
            primaryEmail
                |> Maybe.map (.types >> String.join ", ")
                |> Maybe.withDefault "INTERNET"
    in
    { etag = contact.etag
    , contactId = contact.id
    , href = contact.href
    , rawBody = contact.rawBody
    , displayName = contact.displayName
    , email = emailAddress
    , emailTypes = emailTypes
    , phone = firstContactFieldValue "Phone" contact
    , birthday = firstContactFieldValue "Birthday" contact
    , deathDate = firstContactFieldValue "Death date" contact
    , birthPlace = firstContactFieldValue "Birth place" contact
    , deathPlace = firstContactFieldValue "Death place" contact
    , note = firstContactFieldValue "Note" contact
    , relationships = relationshipDrafts contact
    }


firstContactFieldValue : String -> Contact -> String
firstContactFieldValue name contact =
    contact.fields
        |> List.filter (\field -> field.name == name)
        |> List.head
        |> Maybe.map .value
        |> Maybe.withDefault ""


relationshipDrafts : Contact -> List RelationshipDraft
relationshipDrafts contact =
    let
        properties = parseProperties contact.rawBody
        sourceLines = unfoldedLines contact.rawBody
        related = List.filter (\property -> property.name == "RELATED" || property.name == "X-ABRELATEDNAMES") properties
        primary = List.filter (\property -> property.name /= "X-ABRELATEDNAMES" || property.group == Nothing || not (List.any (\other -> other.name == "RELATED" && other.group == property.group) related)) related
        draft property =
            let
                field = relatedField properties property
                originalLines = List.filter (\line -> parseProperty line |> Maybe.map (\candidate -> candidate == property || candidate.name == "X-ABLABEL" && property.group /= Nothing && candidate.group == property.group) |> Maybe.withDefault False) sourceLines
            in
            { originalLines = originalLines
            , changed = False
            , target = if String.startsWith "https://" field.value || String.startsWith "http://" field.value then Just field.value else Nothing
            , label = relationshipDisplayLabel field
            , name = field.value
            }
    in
    List.map draft primary


contactEditVCard : ContactEditDraft -> String
contactEditVCard draft =
    let
        sourceLines =
            sourceVCardLines draft

        editedLines =
            sourceLines
                |> removeRelationshipLines
                |> replaceUniquePropertyLine "FN" (requiredPropertyLine "FN" draft.displayName)
                |> replaceFirstPropertyLine "EMAIL" (emailPropertyLine draft)
                |> replaceFirstPropertyLine "TEL" (optionalPropertyLine "TEL" draft.phone)
                |> replaceUniquePropertyLine "BDAY" (optionalPropertyLine "BDAY" draft.birthday)
                |> replaceUniquePropertyLine "DEATHDATE" (optionalPropertyLine "DEATHDATE" draft.deathDate)
                |> replaceUniquePropertyLine "BIRTHPLACE" (optionalPropertyLine "BIRTHPLACE" draft.birthPlace)
                |> replaceUniquePropertyLine "DEATHPLACE" (optionalPropertyLine "DEATHPLACE" draft.deathPlace)
                |> replaceFirstPropertyLine "NOTE" (optionalPropertyLine "NOTE" draft.note)
                |> insertLinesBeforeEnd (relationshipPropertyLines draft.relationships)
    in
    String.join "\r\n" editedLines ++ "\r\n"


sourceVCardLines : ContactEditDraft -> List String
sourceVCardLines draft =
    let
        lines =
            unfoldedLines draft.rawBody
    in
    if List.any (isExactVCardLine "BEGIN:VCARD") lines && List.any (isExactVCardLine "END:VCARD") lines then
        lines

    else
        [ "BEGIN:VCARD"
        , "VERSION:4.0"
        , "UID:" ++ escapeVCardText draft.contactId
        , "FN:" ++ escapeVCardText draft.displayName
        , "END:VCARD"
        ]


requiredPropertyLine : String -> String -> Maybe String
requiredPropertyLine propertyName rawValue =
    Just (propertyName ++ ":" ++ escapeVCardText (String.trim rawValue))


optionalPropertyLine : String -> String -> Maybe String
optionalPropertyLine propertyName rawValue =
    let
        valueText =
            String.trim rawValue
    in
    if valueText == "" then
        Nothing

    else
        Just (propertyName ++ ":" ++ escapeVCardText valueText)


emailPropertyLine : ContactEditDraft -> Maybe String
emailPropertyLine draft =
    let
        email =
            String.trim draft.email
    in
    if email == "" then
        Nothing

    else
        Just
            ("EMAIL"
                ++ renderVCardTypeParameters (parseEmailTypes draft.emailTypes)
                ++ ":"
                ++ escapeVCardText email
            )


relationshipPropertyLines : List RelationshipDraft -> List String
relationshipPropertyLines relationships =
    relationships
        |> List.filterMap relationshipPropertyLine


relationshipPropertyLine : RelationshipDraft -> Maybe String
relationshipPropertyLine relationship =
    let
        labelText =
            case String.trim relationship.label of
                "" ->
                    "related"

                valueText ->
                    valueText

        nameText =
            String.trim relationship.name
    in
    if not relationship.changed && not (List.isEmpty relationship.originalLines) then
        Just (String.join "\r\n" relationship.originalLines)

    else if nameText == "" then
        Nothing

    else
        Just
            ("RELATED;TYPE="
                ++ escapeVCardParameter (normalizedRelationshipLabel labelText)
                ++ (relationship.originalLines |> List.filterMap parseProperty |> List.head |> Maybe.map (.params >> List.filter (\( key, _ ) -> key /= "TYPE" && key /= "VALUE") >> List.map (\( key, valueText ) -> ";" ++ key ++ "=" ++ valueText) >> String.concat) |> Maybe.withDefault "")
                ++ (if relationship.target == Nothing then ";VALUE=text:" else ";VALUE=uri:")
                ++ (relationship.target |> Maybe.withDefault (escapeVCardText nameText))
            )


renderVCardTypeParameters : List String -> String
renderVCardTypeParameters values =
    case values of
        [] ->
            ""

        _ ->
            ";TYPE=" ++ String.join "," (List.map escapeVCardParameter values)


escapeVCardParameter : String -> String
escapeVCardParameter value =
    value
        |> String.filter (\char -> char /= ';' && char /= ':' && char /= ',')
        |> String.trim


replaceUniquePropertyLine : String -> Maybe String -> List String -> List String
replaceUniquePropertyLine propertyName maybeLine lines =
    let
        keptLines =
            List.filter (\line -> vcardLinePropertyName line /= propertyName) lines
    in
    case maybeLine of
        Just line ->
            insertLineBeforeEnd line keptLines

        Nothing ->
            keptLines


replaceFirstPropertyLine : String -> Maybe String -> List String -> List String
replaceFirstPropertyLine propertyName maybeLine lines =
    let
        ( found, reversedLines ) =
            List.foldl
                (replaceFirstPropertyStep propertyName maybeLine)
                ( False, [] )
                lines

        updatedLines =
            List.reverse reversedLines
    in
    case ( found, maybeLine ) of
        ( False, Just line ) ->
            insertLineBeforeEnd line updatedLines

        _ ->
            updatedLines


replaceFirstPropertyStep : String -> Maybe String -> String -> ( Bool, List String ) -> ( Bool, List String )
replaceFirstPropertyStep propertyName maybeLine line ( found, reversedLines ) =
    if not found && vcardLinePropertyName line == propertyName then
        case maybeLine of
            Just replacement ->
                ( True, replacement :: reversedLines )

            Nothing ->
                ( True, reversedLines )

    else
        ( found, line :: reversedLines )


removeRelationshipLines : List String -> List String
removeRelationshipLines lines =
    let
        relatedGroups =
            lines
                |> List.filterMap parseProperty
                |> List.filter (\property -> property.name == "X-ABRELATEDNAMES")
                |> List.filterMap .group
    in
    List.filter (not << isRelationshipLine relatedGroups) lines


isRelationshipLine : List String -> String -> Bool
isRelationshipLine relatedGroups line =
    let
        propertyName =
            vcardLinePropertyName line

        propertyGroup =
            parseProperty line |> Maybe.andThen .group
    in
    propertyName == "RELATED"
        || propertyName == "X-ABRELATEDNAMES"
        || (propertyName == "X-ABLABEL" && maybeMember propertyGroup relatedGroups)


maybeMember : Maybe String -> List String -> Bool
maybeMember maybeValue values =
    case maybeValue of
        Just valueText ->
            List.member valueText values

        Nothing ->
            False


insertLinesBeforeEnd : List String -> List String -> List String
insertLinesBeforeEnd newLines lines =
    List.foldl insertLineBeforeEnd lines newLines


insertLineBeforeEnd : String -> List String -> List String
insertLineBeforeEnd newLine lines =
    case lines of
        [] ->
            [ newLine ]

        line :: rest ->
            if isExactVCardLine "END:VCARD" line then
                newLine :: line :: rest

            else
                line :: insertLineBeforeEnd newLine rest


isExactVCardLine : String -> String -> Bool
isExactVCardLine expected line =
    String.toUpper (String.trim line) == expected


vcardLinePropertyName : String -> String
vcardLinePropertyName line =
    case splitFirst ":" line of
        Just ( left, _ ) ->
            left
                |> beforeString ";"
                |> localPropertyName
                |> String.toUpper

        Nothing ->
            ""


localPropertyName : String -> String
localPropertyName nameText =
    case String.split "." nameText |> List.reverse of
        localName :: _ ->
            localName

        [] ->
            nameText


selectedContactAfterLoad : Maybe String -> List Contact -> Maybe String
selectedContactAfterLoad selected contacts =
    case selected of
        Just contactId ->
            if hasContact contactId contacts then
                Just contactId

            else
                firstContactId contacts

        Nothing ->
            firstContactId contacts


selectedContactAfterFilter : Maybe String -> List Contact -> Maybe String
selectedContactAfterFilter =
    selectedContactAfterLoad


selectedCalendarAfterLoad : Maybe String -> List CalendarEntry -> Maybe String
selectedCalendarAfterLoad selected entries =
    case selected of
        Just entryId ->
            if hasCalendarEntry entryId entries then
                Just entryId

            else
                firstCalendarEntryId entries

        Nothing ->
            firstCalendarEntryId entries


selectedCalendarAfterFilter : Maybe String -> List CalendarEntry -> Maybe String
selectedCalendarAfterFilter =
    selectedCalendarAfterLoad


hasContact : String -> List Contact -> Bool
hasContact contactId contacts =
    List.any (\contact -> contact.id == contactId) contacts


hasCalendarEntry : String -> List CalendarEntry -> Bool
hasCalendarEntry entryId entries =
    List.any (\entry -> entry.id == entryId) entries


firstContactId : List Contact -> Maybe String
firstContactId contacts =
    case contacts of
        contact :: _ ->
            Just contact.id

        [] ->
            Nothing


firstCalendarEntryId : List CalendarEntry -> Maybe String
firstCalendarEntryId entries =
    case entries of
        entry :: _ ->
            Just entry.id

        [] ->
            Nothing


selectedContactFrom : List Contact -> Maybe String -> Maybe Contact
selectedContactFrom contacts selected =
    case selected of
        Just contactId ->
            List.head (List.filter (\contact -> contact.id == contactId) contacts)

        Nothing ->
            List.head contacts


selectedCalendarFrom : List CalendarEntry -> Maybe String -> Maybe CalendarEntry
selectedCalendarFrom entries selected =
    case selected of
        Just entryId ->
            List.head (List.filter (\entry -> entry.id == entryId) entries)

        Nothing ->
            List.head entries


replaceContact : Contact -> List Contact -> List Contact
replaceContact replacement contacts =
    List.map
        (\contact ->
            if contact.id == replacement.id then
                replacement

            else
                contact
        )
        contacts


filterContacts : String -> List Contact -> List Contact
filterContacts query contacts =
    let
        needle =
            String.toLower (String.trim query)
    in
    if needle == "" then
        contacts

    else
        List.filter (matchesContact needle) contacts


matchesContact : String -> Contact -> Bool
matchesContact needle contact =
    String.contains needle (String.toLower contact.displayName)
        || List.any
            (\email -> String.contains needle (String.toLower email.address))
            contact.emails


filterCalendarEntries : String -> List CalendarEntry -> List CalendarEntry
filterCalendarEntries query entries =
    let
        needle =
            String.toLower (String.trim query)
    in
    if needle == "" then
        entries

    else
        List.filter (matchesCalendarEntry needle) entries


matchesCalendarEntry : String -> CalendarEntry -> Bool
matchesCalendarEntry needle entry =
    String.contains needle (String.toLower entry.title)
        || String.contains needle (String.toLower entry.calendarName)
        || maybeContains needle entry.location
        || maybeContains needle entry.description


maybeContains : String -> Maybe String -> Bool
maybeContains needle maybeValue =
    case maybeValue of
        Just valueText ->
            String.contains needle (String.toLower valueText)

        Nothing ->
            False


defaultDate : Date
defaultDate =
    { year = 2026, month = 4, day = 30 }


dateFromPosix : Time.Zone -> Time.Posix -> Date
dateFromPosix zone posix =
    { year = Time.toYear zone posix
    , month = monthNumber (Time.toMonth zone posix)
    , day = Time.toDay zone posix
    }


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12


shiftCalendarCursor : CalendarView -> Int -> Date -> Date
shiftCalendarCursor calendarView delta cursorDate =
    case calendarView of
        AgendaView ->
            cursorDate

        DayView ->
            addDays delta cursorDate

        WeekView ->
            addDays (delta * 7) cursorDate

        MonthView ->
            addMonths delta cursorDate


calendarRangeLabel : CalendarView -> Date -> String
calendarRangeLabel calendarView cursorDate =
    case calendarView of
        AgendaView ->
            "Next upcoming"

        DayView ->
            prettyDateFromDate cursorDate

        WeekView ->
            weekRangeLabel cursorDate

        MonthView ->
            fullMonthName cursorDate.month ++ " " ++ String.fromInt cursorDate.year


weekRangeLabel : Date -> String
weekRangeLabel cursorDate =
    case ( List.head (weekDates cursorDate), List.reverse (weekDates cursorDate) ) of
        ( Just firstDay, lastDay :: _ ) ->
            shortDateFromDate firstDay ++ " - " ++ prettyDateFromDate lastDay

        _ ->
            prettyDateFromDate cursorDate


upcomingAgendaEntries : Date -> List CalendarEntry -> List CalendarEntry
upcomingAgendaEntries today entries =
    let
        todayIso =
            dateToIso today
    in
    entries
        |> List.filter (\entry -> datePart entry.startsAt >= todayIso)
        |> List.sortBy .startsAt
        |> List.take 12


entriesOnDate : Date -> List CalendarEntry -> List CalendarEntry
entriesOnDate day entries =
    let
        dayIso =
            dateToIso day
    in
    entries
        |> List.filter (\entry -> datePart entry.startsAt == dayIso)
        |> List.sortBy .startsAt


entriesInDates : List Date -> List CalendarEntry -> List CalendarEntry
entriesInDates days entries =
    let
        dayKeys =
            List.map dateToIso days
    in
    entries
        |> List.filter (\entry -> List.member (datePart entry.startsAt) dayKeys)
        |> List.sortBy .startsAt


entriesInMonth : Date -> List CalendarEntry -> List CalendarEntry
entriesInMonth cursorDate entries =
    entries
        |> List.filter
            (\entry ->
                let
                    entryDate =
                        dateFromIso entry.startsAt
                in
                entryDate.year == cursorDate.year && entryDate.month == cursorDate.month
            )
        |> List.sortBy .startsAt


weekDates : Date -> List Date
weekDates cursorDate =
    let
        start =
            startOfWeek cursorDate
    in
    List.map (\offset -> addDays offset start) (List.range 0 6)


startOfWeek : Date -> Date
startOfWeek cursorDate =
    addDays (0 - weekdayIndex cursorDate) cursorDate


weekDayFromDate : Date -> WeekDay
weekDayFromDate day =
    { isoDate = dateToIso day
    , weekday = weekdayName day
    , label = shortDateFromDate day
    }


monthGridDates : Date -> List Date
monthGridDates cursorDate =
    let
        firstDay =
            { cursorDate | day = 1 }

        start =
            addDays (0 - weekdayIndex firstDay) firstDay
    in
    List.map (\offset -> addDays offset start) (List.range 0 41)


weekdayNames : List String
weekdayNames =
    [ "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun" ]


weekdayName : Date -> String
weekdayName day =
    case List.drop (weekdayIndex day) weekdayNames |> List.head of
        Just name ->
            name

        Nothing ->
            ""


weekdayIndex : Date -> Int
weekdayIndex day =
    modBy 7 (dateToOrdinal day - 1)


dateToOrdinal : Date -> Int
dateToOrdinal day =
    daysBeforeYear day.year + daysBeforeMonth day.year day.month + day.day


daysBeforeYear : Int -> Int
daysBeforeYear year =
    let
        priorYears =
            year - 1
    in
    (365 * priorYears)
        + (priorYears // 4)
        - (priorYears // 100)
        + (priorYears // 400)


daysBeforeMonth : Int -> Int -> Int
daysBeforeMonth year month =
    if month <= 1 then
        0

    else
        List.range 1 (month - 1)
            |> List.map (daysInMonth year)
            |> List.sum


addDays : Int -> Date -> Date
addDays amount day =
    if amount == 0 then
        day

    else if amount > 0 then
        addDays (amount - 1) (nextDate day)

    else
        addDays (amount + 1) (previousDate day)


nextDate : Date -> Date
nextDate day =
    if day.day < daysInMonth day.year day.month then
        { day | day = day.day + 1 }

    else if day.month < 12 then
        { day | month = day.month + 1, day = 1 }

    else
        { year = day.year + 1, month = 1, day = 1 }


previousDate : Date -> Date
previousDate day =
    if day.day > 1 then
        { day | day = day.day - 1 }

    else if day.month > 1 then
        let
            previousMonth =
                day.month - 1
        in
        { day | month = previousMonth, day = daysInMonth day.year previousMonth }

    else
        { year = day.year - 1, month = 12, day = 31 }


addMonths : Int -> Date -> Date
addMonths amount day =
    let
        zeroBasedMonth =
            (day.year * 12) + (day.month - 1) + amount

        newYear =
            zeroBasedMonth // 12

        newMonth =
            modBy 12 zeroBasedMonth + 1
    in
    { year = newYear
    , month = newMonth
    , day = min day.day (daysInMonth newYear newMonth)
    }


daysInMonth : Int -> Int -> Int
daysInMonth year month =
    case month of
        2 ->
            if isLeapYear year then
                29

            else
                28

        4 ->
            30

        6 ->
            30

        9 ->
            30

        11 ->
            30

        _ ->
            31


isLeapYear : Int -> Bool
isLeapYear year =
    (modBy 400 year == 0)
        || (modBy 4 year == 0 && modBy 100 year /= 0)


dateFromIso : String -> Date
dateFromIso value =
    { year = parseInt defaultDate.year (String.slice 0 4 value)
    , month = parseInt defaultDate.month (String.slice 5 7 value)
    , day = parseInt defaultDate.day (String.slice 8 10 value)
    }


dateToIso : Date -> String
dateToIso day =
    String.fromInt day.year
        ++ "-"
        ++ twoDigits day.month
        ++ "-"
        ++ twoDigits day.day


twoDigits : Int -> String
twoDigits value =
    String.padLeft 2 '0' (String.fromInt value)


shortDateFromDate : Date -> String
shortDateFromDate day =
    monthName (twoDigits day.month) ++ " " ++ String.fromInt day.day


prettyDateFromDate : Date -> String
prettyDateFromDate day =
    shortDateFromDate day ++ ", " ++ String.fromInt day.year


weekStartHour : Int
weekStartHour =
    8


weekEndHour : Int
weekEndHour =
    18


weekSlotMinutes : Int
weekSlotMinutes =
    30


weekSlotCount : Int
weekSlotCount =
    (weekEndHour - weekStartHour + 1) * 2


weekHourLabels : List Int
weekHourLabels =
    List.range weekStartHour weekEndHour


datePart : String -> String
datePart value =
    String.left 10 value


eventGridStart : CalendarEntry -> String
eventGridStart entry =
    let
        visibleStart =
            weekStartHour * 60

        startOffset =
            max 0 (minutesOfDay entry.startsAt - visibleStart)

        slot =
            clamp 1 weekSlotCount ((startOffset // weekSlotMinutes) + 1)
    in
    String.fromInt slot


eventGridSpan : CalendarEntry -> String
eventGridSpan entry =
    let
        startMinutes =
            minutesOfDay entry.startsAt

        duration =
            case entry.endsAt of
                Just endsAt ->
                    max weekSlotMinutes (minutesOfDay endsAt - startMinutes)

                Nothing ->
                    weekSlotMinutes

        spanCount =
            clamp 1 weekSlotCount ((duration + weekSlotMinutes - 1) // weekSlotMinutes)
    in
    String.fromInt spanCount


minutesOfDay : String -> Int
minutesOfDay value =
    if String.length value >= 16 then
        (parseInt 0 (String.slice 11 13 value) * 60)
            + parseInt 0 (String.slice 14 16 value)

    else
        weekStartHour * 60


parseInt : Int -> String -> Int
parseInt fallback value =
    case String.toInt value of
        Just number ->
            number

        Nothing ->
            fallback


hourLabel : Int -> String
hourLabel hour =
    String.padLeft 2 '0' (String.fromInt hour) ++ ":00"


primaryContactLine : Contact -> String
primaryContactLine contact =
    case contact.emails of
        email :: _ ->
            email.address

        [] ->
            case firstFieldValue [ "Phone", "Organization", "URL", "Related" ] contact.fields of
                Just valueText ->
                    valueText

                Nothing ->
                    "No details"


firstFieldValue : List String -> List ContactField -> Maybe String
firstFieldValue names fields =
    case names of
        [] ->
            Nothing

        name :: rest ->
            case List.filter (\field -> field.name == name) fields of
                field :: _ ->
                    Just field.value

                [] ->
                    firstFieldValue rest fields


updateAt : Int -> (a -> a) -> List a -> List a
updateAt targetIndex updateValue values =
    values
        |> List.indexedMap
            (\index value ->
                if index == targetIndex then
                    updateValue value

                else
                    value
            )


removeAt : Int -> List a -> List a
removeAt targetIndex values =
    values
        |> List.indexedMap Tuple.pair
        |> List.filter (\( index, _ ) -> index /= targetIndex)
        |> List.map Tuple.second


parseEmailTypes : String -> List String
parseEmailTypes rawTypes =
    let
        values =
            rawTypes
                |> String.split ","
                |> List.map String.trim
                |> List.filter (\valueText -> valueText /= "")
    in
    if List.isEmpty values then
        [ "INTERNET" ]

    else
        values


canSubmitContact : Model -> Bool
canSubmitContact model =
    not model.savingContact && String.trim model.draftName /= ""


canSubmitContactEdit : Model -> ContactEditDraft -> Bool
canSubmitContactEdit model draft =
    not model.savingContactEdit && String.trim draft.displayName /= ""


pageTitle : Page -> String
pageTitle page =
    case page of
        ContactsPage ->
            "Contacts"

        CalendarPage ->
            "Calendar"


searchPlaceholder : Page -> String
searchPlaceholder page =
    case page of
        ContactsPage ->
            "Search name or email"

        CalendarPage ->
            "Search title, calendar, or location"


searchValue : Model -> String
searchValue model =
    case model.page of
        ContactsPage ->
            model.contactQuery

        CalendarPage ->
            model.calendarQuery


searchMessage : Page -> String -> Msg
searchMessage page =
    case page of
        ContactsPage ->
            ContactQueryChanged

        CalendarPage ->
            CalendarQueryChanged


shortDate : String -> String
shortDate value =
    let
        month =
            String.slice 5 7 value

        day =
            String.slice 8 10 value
    in
    monthName month ++ " " ++ stripLeadingZero day


prettyDate : String -> String
prettyDate value =
    let
        year =
            String.left 4 value
    in
    shortDate value ++ ", " ++ year


timeRange : CalendarEntry -> String
timeRange entry =
    if entry.allDay then
        "All day"

    else
        case entry.endsAt of
            Just endsAt ->
                timePart entry.startsAt ++ "-" ++ timePart endsAt

            Nothing ->
                timePart entry.startsAt


timePart : String -> String
timePart value =
    if String.length value >= 16 then
        String.slice 11 16 value

    else
        ""


monthName : String -> String
monthName month =
    case month of
        "01" ->
            "Jan"

        "02" ->
            "Feb"

        "03" ->
            "Mar"

        "04" ->
            "Apr"

        "05" ->
            "May"

        "06" ->
            "Jun"

        "07" ->
            "Jul"

        "08" ->
            "Aug"

        "09" ->
            "Sep"

        "10" ->
            "Oct"

        "11" ->
            "Nov"

        "12" ->
            "Dec"

        _ ->
            month


fullMonthName : Int -> String
fullMonthName month =
    case month of
        1 ->
            "January"

        2 ->
            "February"

        3 ->
            "March"

        4 ->
            "April"

        5 ->
            "May"

        6 ->
            "June"

        7 ->
            "July"

        8 ->
            "August"

        9 ->
            "September"

        10 ->
            "October"

        11 ->
            "November"

        12 ->
            "December"

        _ ->
            String.fromInt month


stripLeadingZero : String -> String
stripLeadingZero value =
    if String.left 1 value == "0" then
        String.dropLeft 1 value

    else
        value


statusClass : Status -> String
statusClass status =
    case status of
        Loading ->
            "is-loading"

        Loaded ->
            "is-loaded"

        Failed _ ->
            "is-failed"


statusText : Status -> String
statusText status =
    case status of
        Loading ->
            "Loading"

        Loaded ->
            "Ready"

        Failed message ->
            "Error: " ++ message


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl url ->
            "bad URL " ++ url

        Http.Timeout ->
            "request timed out"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            case status of
                401 -> "Authentication required. Sign in through the proxy and refresh."
                403 -> "You do not have permission for this collection."
                412 -> "This contact changed since it was loaded. Refresh before saving again."
                _ -> "HTTP " ++ String.fromInt status

        Http.BadBody message ->
            message
