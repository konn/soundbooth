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
import Data.Map.Ordered qualified as OMap (alter)
import Data.Map.Ordered.Strict (OMap)
import Data.Map.Ordered.Strict qualified as OMap
import Data.Maybe (maybeToList)
import Data.Monoid (Endo (..))
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics
import Miso
import Soundbooth.Common.Types

defaultMain :: JSM ()
defaultMain = do
  uri <- toWSUri <$> getCurrentURI
  startApp App {subs = toSubs uri, view = viewModel, ..}
  where
    initialAction = NoOp
    model = Model {playlist = OMap.empty}
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
  m <# do NoOp <$ send (toggleCmd m sound)
updateModel NoOp m = noEff m
updateModel SyncPlaylist m = m <# do NoOp <$ send GetPlaylist

toggleCmd :: Model -> SoundName -> Request
toggleCmd Model {..} sn =
  case OMap.lookup sn playlist of
    Just Playing -> Stop sn
    _ -> Play sn

handleResponse :: Response -> Model -> Effect Action Model
handleResponse = const noEff

handleEvent :: Event -> Model -> Effect Action Model
handleEvent (Started sns) =
  noEff . (#playlist %~ alaf Endo foldMap' (OMap.alter (const $ Just Playing)) sns)
handleEvent (Stopped sns) =
  noEff . (#playlist %~ alaf Endo foldMap' (OMap.alter (const $ Just Idle)) sns)
handleEvent (CurrentPlaylist pl) =
  noEff . (#playlist .~ OMap.fromList (V.toList pl.sounds))

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
                [class_ "main"]
                [ button_
                  [ onClick (Toggle sn)
                  , class_ $ T.unwords $ "button" : maybeToList (statusClass s)
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

data Model = Model {playlist :: OMap SoundName Status}
  deriving (Show, Eq, Ord, Generic)

data EventOrResponse = Event !Event | Response !Response
  deriving (Show, Eq, Ord, Generic)

instance FromJSON EventOrResponse where
  parseJSON v = Event <$> parseJSON v <|> Response <$> parseJSON v

data Action
  = Websock (WebSocket EventOrResponse)
  | Toggle SoundName
  | SyncPlaylist
  | NoOp
  deriving (Show, Eq, Generic)
