{-# LANGUAGE OverloadedStrings #-}
module YandexParser where

import           Telegram.Bot.API
import           Telegram.Bot.Simple

import Data.Maybe
import Network.HTTP.Conduit (simpleHttp)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy as T(unpack)
import Data.List
import Text.XML.HXT.Core
import System.IO.Unsafe

type AlbomID = String
type AlbomNum = String
type NameTrack = String

podlodka :: String
podlodka = "7570122"

habr :: String
habr = "7600069"

hekslet :: String
hekslet = "6408500"

sdCast :: String
sdCast = "6880277"

artProgram :: String
artProgram = "6883832"

twoCapital :: String
twoCapital = "6880541"

gide :: NameTrack
gide = "Сережа попади наконец своими пальчиками куда надо\n\
       \На данный момент ты можешь посмотреть такие подкасты как\n\
       \Подлодка - 1\n\
       \Хабр - 2\n\
       \Хекслет - 3\n\
       \SDCast - 4\n\
       \Art of programming - 5\n\
       \Две столицы - 6\n\
       \Все и разом - 10\n"

getNum :: AlbomID -> Maybe [AlbomNum]
getNum name = case name of
                    "1" -> Just [podlodka]
                    "2" -> Just [habr]
                    "3" -> Just [hekslet]
                    "4" -> Just [sdCast]
                    "5" -> Just [artProgram]
                    "6" -> Just [twoCapital]
                    "10"-> Just [podlodka,
                                 habr,
                                 hekslet,
                                 sdCast,
                                 artProgram,
                                 twoCapital
                                ]
                    _   -> Nothing

getLastPodcast :: [AlbomNum] -> NameTrack
getLastPodcast nums = concatMap (\num -> unsafePerformIO(downloadNameLastPodcast num) ++ "\n\n\n") nums

downloadNameLastPodcast :: String -> IO String
downloadNameLastPodcast  num = do
                                html <- get ("https://music.yandex.ru/album/" ++ num)
                                let doc = readString [withParseHTML yes, withWarnings no] (T.unpack(decodeUtf8(html)))
                                links <- runX $ doc //> hasAttrValue "class" (=="d-track__name")  >>> getAttrValue "title"
                                return (head links)
                                where
                                  get url = simpleHttp url