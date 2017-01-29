module Main where

import Prelude
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Nullable (toMaybe)
import Debug.Trace (traceShowA)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

newtype GxState = GxState
                  { repos :: Array Repository
                  , selected :: Maybe Repository
                  }

derive instance newtypeState :: Newtype GxState _

emptyState :: GxState
emptyState = GxState {repos: [], selected: Nothing}

data GxAction
  = Refresh
  | SelectRepo Repository

gitlaxComponent :: forall eff. T.Spec (ajax :: AJAX | eff) GxState Unit GxAction
gitlaxComponent =
  T.simpleSpec performAction render
  where
    render :: T.Render GxState Unit GxAction
    render dispatch _ (GxState {repos, selected}) _ =
      [ RD.div
        [RP.className "container"]
        [ RD.header'
          [RD.h1' [RD.text (maybe "Gitlax" show selected)]]
        , RD.section [RP.className "main"]
          [ RD.div
              [RP.className "pure-g"]
              [ RD.div
                [RP.className "pure-u-1-5"]
                ((repos <#> (\repo ->
                              RD.div
                                [ RP.className "repo-name"
                                , RP.onClick \_ -> dispatch (SelectRepo repo)
                                ]
                                [RD.text (repo # unwrap # _.name)]))
                 <> [ RD.button
                        [ RP.onClick (\_ -> dispatch Refresh)
                        , RP.className "pure-button button-small"
                        ]
                        [RD.text "Refresh Repositories"]
                    ])
              , RD.div
                [RP.className "pure-u-1-5"]
                [RD.text ""]
              ]
          ]
        , RD.footer' []
        ]
      ]
    performAction :: T.PerformAction (ajax :: AJAX | eff) _ _ _
    performAction a _ state = void case a of
      Refresh -> do
        repos <- lift fetchRepos
        traceShowA repos
        T.writeState (_ {repos = repos} `over GxState` state)
      SelectRepo repo -> do
        T.writeState (_ {selected = Just repo} `over GxState` state)

newtype Repository = Repository
                     { url :: String
                     , name :: String
                     , issues_url :: String
                     }

derive instance newtypeRepo :: Newtype Repository _

instance showRepo :: Show Repository where
  show (Repository r) = "Repository: " <> r.name

parseRepos :: Json -> Array Repository
parseRepos = unsafeCoerce

fetchRepos :: forall m. (MonadAff _ m) => m (Array Repository)
fetchRepos = do
  repos <- getGithub "/user/repos"
  pure (parseRepos repos)

getGithub :: forall m. (MonadAff _ m) => String -> m Json
getGithub url = do
  r <- liftAff $ affjax (defaultRequest { headers = [ RequestHeader "Accept" "application/vnd.github.v3+json"
                                                    , RequestHeader "Authorization" "token xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                                                    ]
                                        , url = "https://api.github.com" <> url
                                        })
  pure r.response

main :: forall e. Eff ( err :: EXCEPTION , ajax :: AJAX , console :: CONSOLE , dom :: DOM | e) Unit
main = void do
  -- launchAff $ fetchRepos >>= map (unwrap >>> _.name) >>> logShow
  let component = T.createClass gitlaxComponent emptyState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial
               (fromJust <<< toMaybe <$> DOM.querySelector "#thermite"
                (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component unit) container
