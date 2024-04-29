{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Soundbooth.Frontend (defaultMain) where

import Control.Applicative ((<|>))
import Control.Arrow ((>>>))
import Control.Lens
import Data.Aeson (FromJSON)
import Data.Aeson.Types (FromJSON (..))
import Data.Coerce (coerce)
import Data.Foldable (foldMap')
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NE
import Data.Map.Ordered qualified as OMap (alter)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe (maybeToList)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics
import Miso
import Miso.String hiding (filter)
import Soundbooth.Common.Types
import Prelude hiding (unwords)

defaultMain :: JSM ()
defaultMain = do
  uri <- toWSUri <$> getCurrentURI
  startApp App {subs = toSubs uri, view = viewModel, ..}
  where
    initialAction = NoOp
    model =
      Model
        { playlist = OMap.empty
        , fadeIn = False
        , fadeOut = False
        , crossFade = False
        }
    update = updateModel

    events = defaultEvents
    toSubs uri =
      [ websocketSub uri protocols Websock
      ]
    protocols = Protocols []
    mountPoint = Nothing
    logLevel = Off

toWSUri :: URI -> URL
toWSUri = handleWsProto >>> handleWsPath >>> show >>> T.pack >>> URL

handleWsPath :: URI -> URI
handleWsPath =
  #uriPath
    %~ ( T.pack >>> \fp ->
          let paren =
                if "." `T.isInfixOf` T.takeWhileEnd (/= '/') fp
                  then T.dropWhileEnd (/= '/') fp
                  else fp
           in T.unpack $ T.dropWhileEnd (== '/') paren <> "/ws"
       )

handleWsProto :: URI -> URI
handleWsProto uri
  | "https:" <- uri.uriScheme = uri {uriScheme = "wss:"}
  | otherwise = uri {uriScheme = "ws:"}

updateModel :: Action -> Model -> Effect Action Model
updateModel (Websock (WebSocketMessage (Event evt))) m = handleEvent evt m
updateModel (Websock (WebSocketMessage (Response rsp))) m = handleResponse rsp m
updateModel (Websock _) m = noEff m
updateModel (Toggle sound) m =
  let (m', cmd) = toggleCmd m sound
   in m' <# do NoOp <$ send cmd
updateModel NoOp m = noEff m
updateModel (Request req) m = m <# do NoOp <$ consoleLog ("Requesting: " <> fromString (show req)) <* send req
updateModel ToggleFadeIn m =
  (m & #fadeIn %~ not)
    <@ consoleLog "ToggleFadeIn"
updateModel ToggleFadeOut m =
  (m & #fadeOut %~ not)
    <@ consoleLog "ToggleFadeOut"
updateModel ToggleCrossFade m =
  (m & #crossFade %~ not)
    <@ consoleLog "ToggleCrossFade"

(<@) :: model -> JSM () -> Effect action model
m <@ act = Effect m [const act]

toggleCmd :: Model -> SoundName -> (Model, Request)
toggleCmd m@Model {..} sn =
  case OMap.lookup sn playlist of
    Just Playing
      | fadeOut -> (m {fadeOut = False}, FadeOut Fading {steps = 10, duration = 3.0} sn)
      | otherwise -> (m, Stop sn)
    _
      | crossFade
      , Just froms <-
          NE.nonEmpty $ filter ((== Playing) . snd) $ OMap.assocs playlist ->
          ( m {crossFade = False}
          , CrossFade
              Fading {steps = 10, duration = 3.0}
              (fst <$> froms)
              (sn NE.:| [])
          )
      | fadeIn ->
          ( m {fadeIn = False}
          , FadeIn Fading {steps = 10, duration = 3.0} sn
          )
      | otherwise -> (m, Play sn)

handleResponse :: Response -> Model -> Effect Action Model
handleResponse = const noEff

handleEvent :: Event -> Model -> Effect Action Model
handleEvent (Started sns) =
  noEff . (#playlist %~ alaf Endo foldMap' (OMap.alter (const $ Just Playing)) sns)
handleEvent (Stopped sns) =
  noEff . (#playlist %~ alaf Endo foldMap' (OMap.alter (const $ Just Idle)) sns)
handleEvent (CurrentPlaylist pl) =
  noEff . (#playlist .~ OMap.fromList (V.toList pl.sounds))

mdiDark :: MisoString -> View a
mdiDark name =
  span_
    [class_ "icon"]
    [ span_
        [ class_ "iconify mdi mdi-dark"
        , data_ "icon" $ "mdi-" <> name
        ]
        []
    ]

viewModel :: Model -> View Action
viewModel Model {..} =
  section_
    [data_ "theme" "dark", class_ "section"]
    [ div_
        [class_ "container"]
        [ h1_ [class_ "title"] [text "Soundbooth"]
        , section_
            [class_ "soundbooth"]
            [ section_
                [class_ "buttons"]
                [ button_
                    [onClick $ Request GetPlaylist, class_ "button is-success is-outlined"]
                    [mdiDark "sync"]
                , button_
                    [onClick $ Request StopAll, class_ "button is-danger is-outlined"]
                    [mdiDark "stop"]
                , button_
                    [ onClick ToggleFadeIn
                    , class_ $
                        unwords $
                          "button"
                            : "is-link"
                            : if fadeIn then ["is-active"] else ["is-outlined"]
                    ]
                    [mdiDark "arrow-top-right"]
                , button_
                    [ onClick ToggleFadeOut
                    , class_ $
                        unwords $
                          "button"
                            : "is-link"
                            : ["is-outlined" | not fadeOut]
                    ]
                    [mdiDark "arrow-bottom-right"]
                , button_
                    [ onClick ToggleCrossFade
                    , class_ $
                        unwords $
                          "button"
                            : "is-link"
                            : ["is-outlined" | not crossFade]
                    ]
                    [mdiDark "shuffle"]
                ]
            , section_
                [class_ "main buttons columns"]
                [ a_
                  [ onClick (Toggle sn)
                  , class_ $ T.unwords $ "column" : "button" : maybeToList (statusClass s)
                  ]
                  [text $ coerce sn]
                | (sn, s) <- OMap.assocs playlist
                ]
            ]
        ]
    ]

statusClass :: Status -> Maybe T.Text
statusClass Idle = Nothing
statusClass Playing = Just "is-primary"

data Model = Model
  { playlist :: OMap SoundName Status
  , fadeIn :: !Bool
  , fadeOut :: !Bool
  , crossFade :: !Bool
  }
  deriving (Show, Eq, Ord, Generic)

data EventOrResponse = Event !Event | Response !Response
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EventOrResponse where
  parseJSON v = Event <$> parseJSON v <|> Response <$> parseJSON v

data Action
  = Websock (WebSocket EventOrResponse)
  | Toggle SoundName
  | Request Request
  | ToggleFadeIn
  | ToggleFadeOut
  | ToggleCrossFade
  | NoOp
  deriving (Show, Eq, Generic)
