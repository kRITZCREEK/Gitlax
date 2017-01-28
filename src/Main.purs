module Main where

import Prelude
import React as R
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Maybe (fromJust)
import Data.Nullable (toMaybe)
import Partial.Unsafe (unsafePartial)
import React.DOM as RD
import React.DOM.Props as RP

hello :: forall eff. T.Spec eff Unit Unit Unit
hello =
  T.simpleSpec T.defaultPerformAction render
  where render _ _ _ _ =
          [ RD.div
            [RP.className "container"]
            [ RD.header'
              [RD.h1' [RD.text "Gitlax"]]
            , RD.section [RP.className "main"] []
            , RD.footer' []
            ]
          ]

main :: forall eff. Eff (dom :: DOM | eff) Unit
main = void do
  let component = T.createClass hello unit
  document <- DOM.window >>= DOM.document
  container <- unsafePartial
               (fromJust <<< toMaybe <$> DOM.querySelector "#thermite"
                (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component unit) container
