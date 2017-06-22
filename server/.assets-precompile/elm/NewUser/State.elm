port module NewUser.State exposing (init, update, subscriptions)

import NewUser.Types exposing (..)
import Common.Types exposing (ContactS, Position, Permission(..))
import Utils.SearchBox as SearchBox
import NewUser.Rest exposing (searchContacts, searchPositions, createContact, createUser)
import Utils.Http
import RemoteData
import Task


init : () -> ( Model, Cmd Msg )
init _ =
    { screen = NoScreen } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.screen ) of
        ( UserCreation subMsg, ScreenUserCreation subModel ) ->
            updateUserCreation subMsg subModel
                |> Tuple.mapFirst (\m -> { model | screen = ScreenUserCreation m })

        ( ContactCreation subMsg, ScreenContactCreation subModel ) ->
            updateContactCreation subMsg subModel
                |> Tuple.mapFirst (\m -> { model | screen = ScreenContactCreation m })

        ( ContactFind subMsg, ScreenContactFind subModel ) ->
            updateContactFind subMsg subModel
                |> Tuple.mapFirst (\m -> { model | screen = ScreenContactFind m })

        ( SelectContact contact, _ ) ->
            let
                userCreationModel =
                    { submission = RemoteData.NotAsked
                    , user = emptyEditableUser contact
                    }
            in
                { model | screen = ScreenUserCreation userCreationModel }
                    ! []

        ( SetModalOpen val, _ ) ->
            if val then
                update GoToContactFind model
            else
                { model | screen = NoScreen } ! []

        ( GoToContactCreation, _ ) ->
            let
                m =
                    { contact =
                        { name = ""
                        , email = ""
                        }
                    , submission = RemoteData.NotAsked
                    }
            in
                { screen = ScreenContactCreation m }
                    ! []

        ( GoToContactFind, _ ) ->
            { screen = ScreenContactFind <| SearchBox.init contactSearchConfig Nothing
            }
                ! []

        _ ->
            model ! []



-- SUBSCRIPTIONS


port openModal : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    openModal (\_ -> SetModalOpen True)



--


emptyEditableUser : ContactS -> EditableUser
emptyEditableUser contact =
    { contact = contact
    , permission = STANDARD
    , position = SearchBox.init positionSearchConfig Nothing
    , email1 = contact.email
    , email2 = contact.email
    , pass1 = ""
    , pass2 = ""
    , active = True
    }


updateUserCreation : UserCreationMsg -> UserCreationModel -> ( UserCreationModel, Cmd Msg )
updateUserCreation msg model =
    let
        user =
            model.user
    in
        case msg of
            SubmitUser ->
                ( { model | submission = RemoteData.Loading }
                , createUser model.user
                    |> Cmd.map (SubmitUserUpdate >> UserCreation)
                )

            SubmitUserUpdate status ->
                { model | submission = status } ! []

            ChangePermission val ->
                case val of
                    "ADMINISTRATOR" ->
                        { model | user = { user | permission = ADMINISTRATOR } } ! []

                    "PRIVILEGED" ->
                        { model | user = { user | permission = PRIVILEGED } } ! []

                    _ ->
                        { model | user = { user | permission = STANDARD } } ! []

            ChangePosition subMsg ->
                SearchBox.update subMsg user.position
                    |> Tuple.mapFirst (\s -> { model | user = { user | position = s } })
                    |> Tuple.mapSecond (Cmd.map (ChangePosition >> UserCreation))

            ChangeEmail1 val ->
                { model | user = { user | email1 = val } } ! []

            ChangeEmail2 val ->
                { model | user = { user | email2 = val } } ! []

            ChangePassword1 val ->
                { model | user = { user | pass1 = val } } ! []

            ChangePassword2 val ->
                { model | user = { user | pass2 = val } } ! []

            ChangeActive val ->
                { model | user = { user | active = val } } ! []


updateContactCreation : ContactCreationMsg -> ContactCreationModel -> ( ContactCreationModel, Cmd Msg )
updateContactCreation msg model =
    let
        contact =
            model.contact
    in
        case msg of
            SubmitContact ->
                ( { model | submission = RemoteData.Loading }
                , createContact model.contact
                    |> Cmd.map (SubmitContactUpdate >> ContactCreation)
                )

            SubmitContactUpdate status ->
                case status of
                    RemoteData.Success contact ->
                        { model | submission = status }
                            ! [ toCmd <| SelectContact contact ]

                    _ ->
                        { model | submission = status } ! []

            ChangeContactName val ->
                { model | contact = { contact | name = val } } ! []

            ChangeContactEmail val ->
                { model | contact = { contact | email = val } } ! []


updateContactFind : ContactFindMsg -> ContactFindModel -> ( ContactFindModel, Cmd Msg )
updateContactFind msg model =
    case msg of
        ChooseFound ->
            ( model
            , SearchBox.getChosen model
                |> Maybe.map SelectContact
                |> Maybe.map toCmd
                |> Maybe.withDefault Cmd.none
            )

        ChangeSearch subMsg ->
            SearchBox.update subMsg model
                |> Tuple.mapSecond (Cmd.map (ChangeSearch >> ContactFind))


toCmd : Msg -> Cmd Msg
toCmd msg =
    Task.perform (\_ -> msg) (Task.succeed ())


contactSearchConfig : SearchBox.Config ContactS
contactSearchConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchContacts
    , placeholder = "Contact name"
    }


positionSearchConfig : SearchBox.Config Position
positionSearchConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchPositions
    , placeholder = "Position name"
    }
