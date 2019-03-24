
module Model.System exposing
  ( Model
  , initModelTask
  )

import Browser.Dom
import Task
import Time


type alias Model =
  { timeZone : Time.Zone
  , curTime : Time.Posix
  , viewport : Browser.Dom.Viewport
  }


initModelTask : Task.Task a Model
initModelTask =
  Task.map3
    (\zone time viewport -> Model zone time viewport)
    Time.here
    Time.now
    Browser.Dom.getViewport
