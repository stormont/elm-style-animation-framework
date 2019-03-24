
module Main exposing (..)

import Animation exposing (px)
import Browser
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (..)
import Time

import AnimationFramework as AF
import Components.Component1 as Component1
import Components.Component2 as Component2
import Model.Model as Model
import Model.System as System


type Msg
  = Init System.Model
  | Component1Msg Component1.Msg
  | Component2Msg Component2.Msg


init : ( Model.Model, Cmd Msg )
init =
  ( Model.initModel
  , Task.perform Init System.initModelTask
  )


subscriptions : Model.Model -> Sub Msg
subscriptions model =
  let
    liftSub msg componentMsg componentSub =
      Sub.map (\x -> msg (componentMsg x)) componentSub
  in
    Sub.batch
      [ liftSub Component1Msg Component1.AnimMsg (AF.subscriptions model.component1.animatedElements)
      , liftSub Component2Msg Component2.AnimMsg (AF.subscriptions model.component2.animatedElements)
      ]


update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
  case msg of

    Init system ->
      ( model
        |> Model.setSystem (Just system)
      , Cmd.map Component1Msg (send Component1.InitComponent)
      )
    
    Component1Msg msg_ ->
      updateComponent1View msg_ model
    
    Component2Msg msg_ ->
      updateComponent2View msg_ model


updateComponent1View : Component1.Msg -> Model.Model -> ( Model.Model, Cmd Msg )
updateComponent1View msg model =
  case msg of

    Component1.AnimMsg (AF.AnimationCompleted Component1.DismissComponent) ->
      let
        (m, c) =
          Component1.update msg model.component1
      in
        ( model
          |> Model.setComponent1 m
          |> Model.setActiveView Model.Component2View
        , Cmd.batch
          [ Cmd.map Component2Msg (send Component2.InitComponent)
          , Cmd.map Component1Msg c
          ]
        )
    
    Component1.AnimMsg afMsg ->
      let
        (m, c) =
          Component1.update msg model.component1
      in
        ( model
          |> Model.setComponent1 m
        , Cmd.map Component1Msg c
        )
    
    Component1.InitComponent ->
      let
        (m, c) =
          Component1.update msg model.component1
      in
        ( model
          |> Model.setComponent1 m
        , Cmd.map Component1Msg c
        )


updateComponent2View : Component2.Msg -> Model.Model -> ( Model.Model, Cmd Msg )
updateComponent2View msg model =
  case msg of

    Component2.AnimMsg (AF.AnimationCompleted Component2.DismissComponent) ->
      let
        (m, c) =
          Component2.update msg model.component2
      in
        ( model
          |> Model.setComponent2 m
          |> Model.setActiveView Model.Component1View
        , Cmd.batch
          [ Cmd.map Component1Msg (send Component1.InitComponent)
          , Cmd.map Component2Msg c
          ]
        )
    
    Component2.AnimMsg afMsg ->
      let
        (m, c) =
          Component2.update msg model.component2
      in
        ( model
          |> Model.setComponent2 m
        , Cmd.map Component2Msg c
        )
    
    Component2.InitComponent ->
      let
        (m, c) =
          Component2.update msg model.component2
      in
        ( model
          |> Model.setComponent2 m
        , Cmd.map Component2Msg c
        )


send : msg -> Cmd msg
send msg =
  Task.succeed msg
    |> Task.perform identity


view : Model.Model -> Html Msg
view model =
  case model.activeView of

    Model.Component1View ->
      renderComponent1View model

    Model.Component2View ->
      renderComponent2View model


renderComponent1View : Model.Model -> Html Msg
renderComponent1View model =
  Html.map Component1Msg (Component1.view model.component1)


renderComponent2View : Model.Model -> Html Msg
renderComponent2View model =
  Html.map Component2Msg (Component2.view model.component2)


main : Program () Model.Model Msg
main =
  Browser.element
    { init = always init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }
