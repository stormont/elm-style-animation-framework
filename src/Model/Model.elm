
module Model.Model exposing (..)

import Components.Component1 as Component1
import Components.Component2 as Component2
import Model.System as System


type ActiveView
  = Component1View
  | Component2View


type alias Model =
  { activeView : ActiveView
  , component1 : Component1.Model
  , component2 : Component2.Model
  , system : Maybe System.Model
  }


initModel : Model
initModel =
  { activeView = Component1View
  , component1 = Component1.initModel
  , component2 = Component2.initModel
  , system = Nothing
  }


setActiveView : ActiveView -> Model -> Model
setActiveView activeView model =
  { model | activeView = activeView }


setComponent1 : Component1.Model -> Model -> Model
setComponent1 component model =
  { model | component1 = component }


setComponent2 : Component2.Model -> Model -> Model
setComponent2 component model =
  { model | component2 = component }


setSystem : Maybe System.Model -> Model -> Model
setSystem system model =
  { model | system = system }
