
module AnimationFramework exposing
  ( AnimationId
  , AnimDict
  , Msg(..)
  , createActionable
  , defaultStaticAnimation
  , insert
  , subscriptions
  , update
  )

import Animation
import Animation.Messenger
import Dict
import Browser.Events
import Html
import Time


type alias AnimationId
  = String


type alias AnimDict t
  = Dict.Dict AnimationId (Animation.Messenger.State (Msg t))


type Msg t
  = AnimationCompleted t
  | StartAnimation AnimationId (Animation.Messenger.State (Msg t))
  | UpdateAnimation Animation.Msg


-- Infer the types, to allow access to un-exposed types in Animation.Model
createActionable how what currentState =
  how what currentState


defaultStaticAnimation : Animation.Messenger.State (Msg t)
defaultStaticAnimation =
  Animation.style [ Animation.opacity 1 ]


insert : List ( AnimationId, (Animation.Messenger.State (Msg t)) ) -> AnimDict t -> AnimDict t
insert animations dict =
  let
    accFunc (k, v) acc =
      Dict.insert k v acc
  in
    List.foldl accFunc dict animations


subscriptions : AnimDict t -> Sub (Msg t)
subscriptions dict =
  Animation.subscription UpdateAnimation (Dict.values dict)


update : (Msg t) -> AnimDict t -> ( AnimDict t, Cmd (Msg t) )
update msg dict =
  case msg of

    AnimationCompleted id ->
      ( dict, Cmd.none )
    
    StartAnimation id state ->
      ( Dict.insert id state dict, Cmd.none )

    UpdateAnimation animMsg ->
      updateDict animMsg dict


-- INTERNAL (not exposed)


updateDict : Animation.Msg -> AnimDict t -> ( AnimDict t, Cmd (Msg t) )
updateDict msg dict =
  let
    accFunc (k, (v, c)) (dict_, cmds) =
      (Dict.insert k v dict_, c :: cmds)
  in
    updateValues msg dict
      |> List.foldl accFunc (dict, [])
      |> (\(d, cs) -> (d, Cmd.batch cs))


updateValues : Animation.Msg -> AnimDict t -> List ( AnimationId, (Animation.Messenger.State (Msg t),  Cmd (Msg t)) )
updateValues msg dict =
  let
    updateAnim (k, v) =
      (k, Animation.Messenger.update msg v)
  in
    Dict.toList dict
      |> List.map updateAnim
