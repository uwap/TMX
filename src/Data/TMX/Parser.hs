{-# LANGUAGE Arrows #-}
module Data.TMX.Parser where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Exception.Base
import Control.Lens
import Data.TMX.Types
import Data.Map (Map, fromList)
import Data.Char
import Data.Version
import Text.XML.HXT.Core
import Text.ParserCombinators.ReadP
import Linear

loadMap :: (Exception e, MonadIO m) => FilePath -> ExceptT e m TiledMap
loadMap file = join . liftIO $ do
  let r = runX $ parseMap <<< readDocument [ withValidate no, withRemoveWS yes] file
  catch (return . head <$> r) (return . throwE)

defaultIfEmpty :: b -> ([a] -> b) -> [a] -> b
defaultIfEmpty b _ [] = b
defaultIfEmpty _ f xs = f xs

parseProperties :: IOSArrow XmlTree [(String,String)]
parseProperties = listA $ getChildren >>> isElem >>> hasName "properties"
                  >>> getChildren >>> isElem >>> hasName "property"
                  >>> getAttrValue0 "name" &&& getAttrValue0 "value"

parseOffset :: IOSArrow XmlTree (String, String)
parseOffset = getChildren >>> isElem >>> hasName "tileoffset"
            >>> getAttrValue0 "x" &&& getAttrValue0 "y"

parseTileSets :: IOSArrow XmlTree [TileSet]
parseTileSets = listA $ getChildren >>> isElem
              >>> hasName "tileset" >>> proc ts -> do
    firstgid    <- getAttrValue0 "firstgid"     -< ts
    _source     <- getAttrValue  "source"       -< ts
    name        <- getAttrValue0 "name"         -< ts
    tilewidth   <- getAttrValue0 "tilewidth"    -< ts
    tileheight  <- getAttrValue0 "tileheight"   -< ts
    _spacing    <- getAttrValue  "spacing"      -< ts
    _margin     <- getAttrValue  "margin"       -< ts
    tilecount   <- getAttrValue  "tilecount"    -< ts
    offset      <- parseOffset                  -< ts
    properties  <- parseProperties              -< ts
    returnA -<
      TileSet { tileSetFirstGID   = read firstgid
              , tileSetName       = name
              , tileSetTileSize   = V2 (read tilewidth) (read tileheight)
              , tileSetTileCount  = defaultIfEmpty 0 read tilecount
              , tileSetOffset     = uncurry V2 $ offset & each %~ defaultIfEmpty 0 read
              , tileSetProperties = fromList properties
              } 

parseMap :: IOSArrow XmlTree TiledMap
parseMap = getChildren >>> isElem
        >>> hasName "map" >>> proc map -> do
    version     <- getAttrValue0 "version"         -< map
    orientation <- getAttrValue0 "orientation"     -< map
    width       <- getAttrValue0 "width"           -< map
    height      <- getAttrValue0 "height"          -< map
    tilewidth   <- getAttrValue0 "tilewidth"       -< map
    tileheight  <- getAttrValue0 "tileheight"      -< map
    _bgcolor    <- getAttrValue  "backgroundcolor" -< map
    renderorder <- getAttrValue  "renderorder"     -< map
    properties  <- parseProperties                 -< map
    tilesets    <- parseTileSets                   -< map
    returnA -< TiledMap 
      { tiledMapVersion     = toVersion $ readP_to_S parseVersion version
      , tiledMapOrientation = read $ normalize orientation
      , tiledMapSize        = V2 (read width) (read height)
      , tiledMapTileSize    = V2 (read tilewidth) (read tileheight)
      , tiledMapRenderOrder = defaultIfEmpty RightDown read $ normalize renderorder -- TODO: doesn't parse
      , tiledMapProperties  = fromList properties
      , tiledMapTileSets    = fromList $ tilesets & each %~ \ts -> (ts^.name,ts)
      }
  where
    normalize (x:xs) = toUpper x : fmap toLower xs

    toVersion [] = error "Couldn't parse map version"
    toVersion ((v,x):xs) | null x = v
                         | otherwise = toVersion xs
