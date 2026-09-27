module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type Page
    = Dashboard
    | Usuarios
    | Pedidos
    | Configuracion


type alias User =
    { id : Int
    , name : String
    , email : String
    , role : String
    , active : Bool
    }


type OrderStatus
    = Pendiente
    | Enviado
    | Entregado
    | Cancelado


type alias Order =
    { id : String
    , customer : String
    , total : Float
    , status : OrderStatus
    }


type alias Model =
    { page : Page
    , search : String
    , users : List User
    , newUserName : String
    , newUserEmail : String
    , orderFilter : Maybe OrderStatus
    , orders : List Order
    , darkMode : Bool
    , notifications : Bool
    }


initialModel : Model
initialModel =
    { page = Dashboard
    , search = ""
    , users =
        [ User 1 "Ana Torres" "ana@empresa.com" "Administrador" True
        , User 2 "Luis Pérez" "luis@empresa.com" "Editor" True
        , User 3 "María Gómez" "maria@empresa.com" "Lector" False
        , User 4 "Carlos Ruiz" "carlos@empresa.com" "Editor" True
        , User 5 "Sofía Herrera" "sofia@empresa.com" "Lector" True
        ]
    , newUserName = ""
    , newUserEmail = ""
    , orderFilter = Nothing
    , orders =
        [ Order "#1001" "Ana Torres" 120.5 Entregado
        , Order "#1002" "Luis Pérez" 89.99 Enviado
        , Order "#1003" "María Gómez" 45.0 Pendiente
        , Order "#1004" "Carlos Ruiz" 310.25 Pendiente
        , Order "#1005" "Sofía Herrera" 15.75 Cancelado
        , Order "#1006" "Ana Torres" 230.0 Enviado
        ]
    , darkMode = False
    , notifications = True
    }



-- UPDATE


type Msg
    = NavigateTo Page
    | SearchChanged String
    | ToggleUserStatus Int
    | NewUserNameChanged String
    | NewUserEmailChanged String
    | AddUser
    | OrderFilterChanged (Maybe OrderStatus)
    | ToggleDarkMode Bool
    | ToggleNotifications Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        NavigateTo page ->
            { model | page = page }

        SearchChanged text ->
            { model | search = text }

        ToggleUserStatus userId ->
            { model
                | users =
                    List.map
                        (\u ->
                            if u.id == userId then
                                { u | active = not u.active }

                            else
                                u
                        )
                        model.users
            }

        NewUserNameChanged name ->
            { model | newUserName = name }

        NewUserEmailChanged email ->
            { model | newUserEmail = email }

        AddUser ->
            if String.isEmpty (String.trim model.newUserName) || String.isEmpty (String.trim model.newUserEmail) then
                model

            else
                let
                    nextId =
                        1 + (List.maximum (List.map .id model.users) |> Maybe.withDefault 0)
                in
                { model
                    | users = model.users ++ [ User nextId (String.trim model.newUserName) (String.trim model.newUserEmail) "Lector" True ]
                    , newUserName = ""
                    , newUserEmail = ""
                }

        OrderFilterChanged filter ->
            { model | orderFilter = filter }

        ToggleDarkMode value ->
            { model | darkMode = value }

        ToggleNotifications value ->
            { model | notifications = value }



-- PALETTE


type alias Palette =
    { background : Color
    , surface : Color
    , sidebar : Color
    , text : Color
    , muted : Color
    , border : Color
    , primary : Color
    , onPrimary : Color
    }


palette : Bool -> Palette
palette dark =
    if dark then
        { background = rgb255 18 20 28
        , surface = rgb255 30 33 45
        , sidebar = rgb255 12 14 20
        , text = rgb255 230 232 240
        , muted = rgb255 150 155 175
        , border = rgb255 50 54 70
        , primary = rgb255 99 132 255
        , onPrimary = rgb255 255 255 255
        }

    else
        { background = rgb255 244 246 250
        , surface = rgb255 255 255 255
        , sidebar = rgb255 30 41 59
        , text = rgb255 30 35 45
        , muted = rgb255 110 118 135
        , border = rgb255 225 229 236
        , primary = rgb255 59 91 219
        , onPrimary = rgb255 255 255 255
        }


statusColor : OrderStatus -> Color
statusColor status =
    case status of
        Pendiente ->
            rgb255 234 162 33

        Enviado ->
            rgb255 59 130 246

        Entregado ->
            rgb255 34 160 90

        Cancelado ->
            rgb255 220 60 60


statusLabel : OrderStatus -> String
statusLabel status =
    case status of
        Pendiente ->
            "Pendiente"

        Enviado ->
            "Enviado"

        Entregado ->
            "Entregado"

        Cancelado ->
            "Cancelado"


pageLabel : Page -> String
pageLabel page =
    case page of
        Dashboard ->
            "Dashboard"

        Usuarios ->
            "Usuarios"

        Pedidos ->
            "Pedidos"

        Configuracion ->
            "Configuración"


pageIcon : Page -> String
pageIcon page =
    case page of
        Dashboard ->
            "📊"

        Usuarios ->
            "👥"

        Pedidos ->
            "📦"

        Configuracion ->
            "⚙️"



-- VIEW


view : Model -> Html Msg
view model =
    let
        p =
            palette model.darkMode
    in
    layout
        [ Background.color p.background
        , Font.color p.text
        , Font.size 15
        , Font.family [ Font.typeface "Inter", Font.typeface "Segoe UI", Font.sansSerif ]
        ]
        (row [ width fill, height fill ]
            [ viewSidebar p model.page
            , column [ width fill, height fill ]
                [ viewTopBar p model
                , el [ width fill, height fill, padding 28, scrollbarY ] (viewPage p model)
                ]
            ]
        )


viewSidebar : Palette -> Page -> Element Msg
viewSidebar p current =
    column
        [ width (px 230)
        , height fill
        , Background.color p.sidebar
        , Font.color (rgb255 220 225 235)
        , paddingXY 16 24
        , spacing 6
        ]
        (el [ Font.bold, Font.size 20, paddingEach { top = 0, bottom = 24, left = 8, right = 0 } ] (text "⚡ Admin Console")
            :: List.map (sidebarItem p current) [ Dashboard, Usuarios, Pedidos, Configuracion ]
        )


sidebarItem : Palette -> Page -> Page -> Element Msg
sidebarItem p current page =
    let
        selected =
            current == page
    in
    Input.button
        [ width fill
        , paddingXY 12 10
        , Border.rounded 8
        , Background.color
            (if selected then
                p.primary

             else
                rgba 0 0 0 0
            )
        , mouseOver [ Background.color (rgba 1 1 1 0.08) ]
        , Font.color
            (if selected then
                p.onPrimary

             else
                rgb255 200 205 220
            )
        ]
        { onPress = Just (NavigateTo page)
        , label = row [ spacing 10 ] [ text (pageIcon page), text (pageLabel page) ]
        }


viewTopBar : Palette -> Model -> Element Msg
viewTopBar p model =
    row
        [ width fill
        , height (px 64)
        , paddingXY 28 0
        , spacing 20
        , Background.color p.surface
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color p.border
        ]
        [ el [ Font.bold, Font.size 20 ] (text (pageLabel model.page))
        , Input.search
            [ width (maximum 360 fill)
            , alignRight
            , Background.color p.background
            , Border.color p.border
            , Border.rounded 8
            , paddingXY 12 8
            ]
            { onChange = SearchChanged
            , text = model.search
            , placeholder = Just (Input.placeholder [ Font.color p.muted ] (text "Buscar usuarios o pedidos..."))
            , label = Input.labelHidden "Buscar"
            }
        , row [ spacing 10, alignRight ]
            [ el
                [ width (px 36)
                , height (px 36)
                , Border.rounded 18
                , Background.color p.primary
                , Font.color p.onPrimary
                , Font.bold
                ]
                (el [ centerX, centerY ] (text "CS"))
            , column [ spacing 2 ]
                [ el [ Font.semiBold ] (text "Admin")
                , el [ Font.size 12, Font.color p.muted ] (text "Superusuario")
                ]
            ]
        ]


viewPage : Palette -> Model -> Element Msg
viewPage p model =
    case model.page of
        Dashboard ->
            viewDashboard p model

        Usuarios ->
            viewUsers p model

        Pedidos ->
            viewOrders p model

        Configuracion ->
            viewSettings p model


card : Palette -> List (Attribute Msg) -> Element Msg -> Element Msg
card p attrs content =
    el
        ([ Background.color p.surface
         , Border.rounded 12
         , Border.width 1
         , Border.color p.border
         , padding 20
         ]
            ++ attrs
        )
        content


cardTitle : String -> Element Msg
cardTitle title =
    el [ Font.bold, Font.size 17, paddingEach { top = 0, bottom = 12, left = 0, right = 0 } ] (text title)



-- DASHBOARD


viewDashboard : Palette -> Model -> Element Msg
viewDashboard p model =
    let
        activeUsers =
            List.length (List.filter .active model.users)

        pending =
            List.length (List.filter (\o -> o.status == Pendiente) model.orders)

        sales =
            model.orders
                |> List.filter (\o -> o.status /= Cancelado)
                |> List.map .total
                |> List.sum
    in
    column [ width fill, spacing 24 ]
        [ wrappedRow [ width fill, spacing 20 ]
            [ kpiCard p "👥" "Usuarios activos" (String.fromInt activeUsers) "+12% vs. mes anterior"
            , kpiCard p "💰" "Ventas del mes" ("$" ++ formatMoney sales) "+8% vs. mes anterior"
            , kpiCard p "⏳" "Pedidos pendientes" (String.fromInt pending) "Requieren atención"
            , kpiCard p "🎫" "Tickets abiertos" "7" "3 de alta prioridad"
            ]
        , wrappedRow [ width fill, spacing 20 ]
            [ card p [ width (fillPortion 2 |> minimum 360), alignTop ] (viewSalesChart p)
            , card p [ width (fillPortion 1 |> minimum 280), alignTop ] (viewActivity p)
            ]
        ]


kpiCard : Palette -> String -> String -> String -> String -> Element Msg
kpiCard p icon title value note =
    card p
        [ width (fill |> minimum 200) ]
        (column [ spacing 8 ]
            [ row [ spacing 8, Font.color p.muted ] [ text icon, text title ]
            , el [ Font.size 30, Font.bold ] (text value)
            , el [ Font.size 12, Font.color p.muted ] (text note)
            ]
        )


viewSalesChart : Palette -> Element Msg
viewSalesChart p =
    let
        data =
            [ ( "Ene", 42 ), ( "Feb", 55 ), ( "Mar", 48 ), ( "Abr", 70 ), ( "May", 62 ), ( "Jun", 85 ), ( "Jul", 78 ), ( "Ago", 92 ), ( "Sep", 88 ) ]

        maxValue =
            List.maximum (List.map Tuple.second data) |> Maybe.withDefault 1

        bar ( label, value ) =
            column [ width fill, height fill, spacing 6 ]
                [ el [ Font.size 11, Font.color p.muted, centerX, alignBottom ] (text (String.fromInt value))
                , el
                    [ width (fill |> maximum 36)
                    , height (px (round (160 * toFloat value / toFloat maxValue)))
                    , centerX
                    , Background.color p.primary
                    , Border.roundEach { topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0 }
                    ]
                    none
                , el [ Font.size 12, Font.color p.muted, centerX ] (text label)
                ]
    in
    column [ width fill, spacing 8 ]
        [ cardTitle "Ventas mensuales (miles $)"
        , row [ width fill, height (px 210), spacing 10 ] (List.map bar data)
        ]


viewActivity : Palette -> Element Msg
viewActivity p =
    let
        item ( icon, what, when ) =
            row [ width fill, spacing 12, paddingXY 0 8, Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }, Border.color p.border ]
                [ text icon
                , paragraph [] [ text what ]
                , el [ alignRight, Font.size 12, Font.color p.muted ] (text when)
                ]
    in
    column [ width fill ]
        (cardTitle "Actividad reciente"
            :: List.map item
                [ ( "🟢", "Ana Torres inició sesión", "hace 2 min" )
                , ( "📦", "Pedido #1006 enviado", "hace 15 min" )
                , ( "👤", "Nuevo usuario registrado", "hace 1 h" )
                , ( "⚠️", "Pago rechazado en #1005", "hace 3 h" )
                , ( "🛠️", "Configuración actualizada", "ayer" )
                ]
        )



-- USERS


matchesSearch : String -> List String -> Bool
matchesSearch search fields =
    let
        q =
            String.toLower (String.trim search)
    in
    String.isEmpty q || List.any (String.contains q << String.toLower) fields


viewUsers : Palette -> Model -> Element Msg
viewUsers p model =
    let
        users =
            List.filter (\u -> matchesSearch model.search [ u.name, u.email, u.role ]) model.users

        header label =
            el [ Font.bold, Font.color p.muted, Font.size 13, paddingXY 0 10 ] (text label)

        cell content =
            el [ paddingXY 0 10, centerY ] content
    in
    column [ width fill, spacing 24 ]
        [ card p
            [ width fill ]
            (column [ width fill, spacing 12 ]
                [ cardTitle "Agregar usuario"
                , wrappedRow [ width fill, spacing 12 ]
                    [ textInput p "Nombre" model.newUserName NewUserNameChanged
                    , textInput p "Correo electrónico" model.newUserEmail NewUserEmailChanged
                    , primaryButton p "➕ Agregar" (Just AddUser)
                    ]
                ]
            )
        , card p
            [ width fill ]
            (column [ width fill, spacing 8 ]
                [ cardTitle ("Usuarios (" ++ String.fromInt (List.length users) ++ ")")
                , if List.isEmpty users then
                    el [ Font.color p.muted, paddingXY 0 12 ] (text "No hay usuarios que coincidan con la búsqueda.")

                  else
                    table [ width fill, spacingXY 16 0 ]
                        { data = users
                        , columns =
                            [ { header = header "ID", width = px 50, view = \u -> cell (text (String.fromInt u.id)) }
                            , { header = header "Nombre", width = fill, view = \u -> cell (el [ Font.semiBold ] (text u.name)) }
                            , { header = header "Correo", width = fill, view = \u -> cell (text u.email) }
                            , { header = header "Rol", width = px 130, view = \u -> cell (text u.role) }
                            , { header = header "Estado"
                              , width = px 100
                              , view =
                                    \u ->
                                        cell
                                            (if u.active then
                                                badge (rgb255 34 160 90) "Activo"

                                             else
                                                badge (rgb255 140 145 160) "Inactivo"
                                            )
                              }
                            , { header = header "Acciones"
                              , width = px 120
                              , view =
                                    \u ->
                                        cell
                                            (secondaryButton p
                                                (if u.active then
                                                    "Desactivar"

                                                 else
                                                    "Activar"
                                                )
                                                (ToggleUserStatus u.id)
                                            )
                              }
                            ]
                        }
                ]
            )
        ]


textInput : Palette -> String -> String -> (String -> Msg) -> Element Msg
textInput p label value toMsg =
    Input.text
        [ width (fill |> minimum 220)
        , Background.color p.background
        , Border.color p.border
        , Border.rounded 8
        , paddingXY 12 10
        ]
        { onChange = toMsg
        , text = value
        , placeholder = Just (Input.placeholder [ Font.color p.muted ] (text label))
        , label = Input.labelHidden label
        }


primaryButton : Palette -> String -> Maybe Msg -> Element Msg
primaryButton p label onPress =
    Input.button
        [ Background.color p.primary
        , Font.color p.onPrimary
        , Font.semiBold
        , paddingXY 18 10
        , Border.rounded 8
        , mouseOver [ alpha 0.9 ]
        ]
        { onPress = onPress, label = text label }


secondaryButton : Palette -> String -> Msg -> Element Msg
secondaryButton p label msg =
    Input.button
        [ Border.width 1
        , Border.color p.border
        , Border.rounded 6
        , paddingXY 10 6
        , Font.size 13
        , mouseOver [ Background.color p.background ]
        ]
        { onPress = Just msg, label = text label }


badge : Color -> String -> Element Msg
badge color label =
    el
        [ Background.color color
        , Font.color (rgb255 255 255 255)
        , Font.size 12
        , Font.semiBold
        , paddingXY 10 4
        , Border.rounded 12
        ]
        (text label)



-- ORDERS


viewOrders : Palette -> Model -> Element Msg
viewOrders p model =
    let
        orders =
            model.orders
                |> List.filter (\o -> model.orderFilter == Nothing || model.orderFilter == Just o.status)
                |> List.filter (\o -> matchesSearch model.search [ o.id, o.customer ])

        header label =
            el [ Font.bold, Font.color p.muted, Font.size 13, paddingXY 0 10 ] (text label)

        cell content =
            el [ paddingXY 0 10, centerY ] content

        filterChip label value =
            let
                selected =
                    model.orderFilter == value
            in
            Input.button
                [ paddingXY 14 8
                , Border.rounded 16
                , Border.width 1
                , Border.color
                    (if selected then
                        p.primary

                     else
                        p.border
                    )
                , Background.color
                    (if selected then
                        p.primary

                     else
                        p.surface
                    )
                , Font.color
                    (if selected then
                        p.onPrimary

                     else
                        p.text
                    )
                , Font.size 13
                ]
                { onPress = Just (OrderFilterChanged value), label = text label }
    in
    card p
        [ width fill ]
        (column [ width fill, spacing 12 ]
            [ cardTitle ("Pedidos (" ++ String.fromInt (List.length orders) ++ ")")
            , wrappedRow [ spacing 8 ]
                (filterChip "Todos" Nothing
                    :: List.map (\s -> filterChip (statusLabel s) (Just s)) [ Pendiente, Enviado, Entregado, Cancelado ]
                )
            , if List.isEmpty orders then
                el [ Font.color p.muted, paddingXY 0 12 ] (text "No hay pedidos que coincidan.")

              else
                table [ width fill, spacingXY 16 0 ]
                    { data = orders
                    , columns =
                        [ { header = header "Pedido", width = px 90, view = \o -> cell (el [ Font.semiBold ] (text o.id)) }
                        , { header = header "Cliente", width = fill, view = \o -> cell (text o.customer) }
                        , { header = header "Total", width = px 120, view = \o -> cell (text ("$" ++ formatMoney o.total)) }
                        , { header = header "Estado", width = px 120, view = \o -> cell (badge (statusColor o.status) (statusLabel o.status)) }
                        ]
                    }
            ]
        )



-- SETTINGS


viewSettings : Palette -> Model -> Element Msg
viewSettings p model =
    card p
        [ width (fill |> maximum 560) ]
        (column [ width fill, spacing 20 ]
            [ cardTitle "Preferencias"
            , Input.radioRow [ spacing 20 ]
                { onChange = ToggleDarkMode
                , selected = Just model.darkMode
                , label = Input.labelAbove [ Font.semiBold, paddingEach { top = 0, bottom = 8, left = 0, right = 0 } ] (text "Tema")
                , options =
                    [ Input.option False (text "☀️ Claro")
                    , Input.option True (text "🌙 Oscuro")
                    ]
                }
            , Input.checkbox []
                { onChange = ToggleNotifications
                , icon = Input.defaultCheckbox
                , checked = model.notifications
                , label = Input.labelRight [ paddingEach { top = 0, bottom = 0, left = 8, right = 0 } ] (text "Recibir notificaciones por correo")
                }
            , el [ Font.size 13, Font.color p.muted ]
                (text
                    (if model.notifications then
                        "✅ Las notificaciones están activadas."

                     else
                        "🔕 Las notificaciones están desactivadas."
                    )
                )
            ]
        )



-- HELPERS


formatMoney : Float -> String
formatMoney amount =
    let
        cents =
            round (amount * 100)

        whole =
            cents // 100

        fraction =
            modBy 100 cents
    in
    String.fromInt whole ++ "." ++ String.padLeft 2 '0' (String.fromInt fraction)
