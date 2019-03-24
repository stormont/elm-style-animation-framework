
module Components.Component2 exposing
  ( AnimationCompleteMsg(..)
  , ElementId(..)
  , Model
  , Msg(..)
  , initModel
  , setAnimatedElements
  , toString
  , update
  , view
  )

import Animation
import Animation.Messenger
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task

import AnimationFramework as AF


type ElementId
  = Element1


type AnimationCompleteMsg
  = DismissComponent


type alias Model =
  { animatedElements : AF.AnimDict AnimationCompleteMsg
  }


type Msg
  = AnimMsg (AF.Msg AnimationCompleteMsg)
  | InitComponent


initModel : Model
initModel =
  { animatedElements = Dict.empty
  }


setAnimatedElements : AF.AnimDict AnimationCompleteMsg -> Model -> Model
setAnimatedElements dict model =
  { model | animatedElements = dict }


toString : ElementId -> String
toString elementId =
  case elementId of

    Element1 -> "Element1"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    AnimMsg afMsg ->
      let
        (m, c) =
          AF.update afMsg model.animatedElements
      in
        ( model
          |> setAnimatedElements m
        , Cmd.map AnimMsg c
        )
    
    InitComponent ->
      ( model
        |> setAnimatedElements (AF.insert defaultAnimations Dict.empty)
      , Cmd.none
      )


view : Model -> Html.Html Msg
view model =
  let
    animId =
      Element1
    clickAnimation x =
      createAnimation x fadeOut
        |> AF.StartAnimation (toString animId)
        |> AnimMsg
    animatedStyle =
      case Dict.get (toString animId) model.animatedElements of
        Nothing -> []
        Just x ->
          Animation.render x
            ++ [ onClick (clickAnimation x)
                , style "position" "relative"
                , style "margin" "100px auto"
                , style "padding" "25px"
                , style "height" "200px"
                , style "color" "white"
                ]
  in
    div
      animatedStyle
      [ text "Component 2"
      , br [] []
      , text "Click to Animate!"
      ]


-- INTERNAL (not exposed)


createAnimation currentState animation =
  AF.createActionable
    Animation.interrupt
    animation
    currentState


defaultAnimations : List ( AF.AnimationId, Animation.Messenger.State (AF.Msg AnimationCompleteMsg) )
defaultAnimations =
  -- In order for animation updates to fire, each element definition used must have
  -- an initial non-empty animation defined.
  [ (toString Element1, createAnimation staticTransparent fadeIn)
  ]


staticTransparent =
  Animation.style
    [ Animation.opacity 0
    , Animation.backgroundColor { red = 255, green = 255, blue = 255, alpha = 1.0 }
    , Animation.width (Animation.px 600)
    ]


fadeIn =
  [ Animation.to
    [ Animation.opacity 1
    , Animation.backgroundColor { red = 128, green = 0, blue = 0, alpha = 1.0 }
    , Animation.width (Animation.px 200)
    ]
  ]


fadeOut =
  [ Animation.to [ Animation.opacity 0 ]
  , Animation.Messenger.send (AF.AnimationCompleted DismissComponent)
  ]
