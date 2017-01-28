module Main where

import Prelude
import React as R
import React.DOM as RD
import React.DOM.Props as RP
import ReactDOM as RDOM
import Thermite as T
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE, logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Data.Argonaut.Core (Json)
import Data.Array (singleton)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Nullable (toMaybe)
import Debug.Trace (traceShowA)
import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

newtype GxState = GxState
                  { repos :: Array Repository
                  }

derive instance newtypeState :: Newtype GxState _

emptyState :: GxState
emptyState = GxState {repos: []}

data GxAction = Refresh

gitlaxComponent :: forall eff. T.Spec (ajax :: AJAX | eff) GxState Unit GxAction
gitlaxComponent =
  T.simpleSpec performAction render
  where
    render :: T.Render GxState Unit GxAction
    render dispatch _ (GxState {repos}) _ =
      [ RD.div
        [RP.className "container"]
        [ RD.header'
          [RD.h1' [RD.text "Gitlax"]]
        , RD.section [RP.className "main"]
          [ RD.button
            [RP.onClick (\_ -> dispatch Refresh)]
            [RD.text "Refresh Repositories"]
          , RD.ul' (map (\repo -> RD.li' [RD.text (show repo)]) repos)
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
  launchAff $ fetchRepos >>= map (unwrap >>> _.name) >>> logShow
  let component = T.createClass gitlaxComponent emptyState
  document <- DOM.window >>= DOM.document
  container <- unsafePartial
               (fromJust <<< toMaybe <$> DOM.querySelector "#thermite"
                (DOM.htmlDocumentToParentNode document))
  RDOM.render (R.createFactory component unit) container
