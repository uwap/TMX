{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.TMX.Types where

import Linear
import Control.Lens
import Data.Map
import Data.Version

type GlobalID   = Int
type Properties = Map String String

data TileSet = TileSet { tileSetFirstGID   :: GlobalID   -- ^ The GlobalID of the first tile in the TileSet
                       , tileSetName       :: String
                       , tileSetTileSize   :: V2 Int     -- ^ The (maximum) size of the tiles in this TileSet
                       , tileSetTileCount  :: Int        -- ^ The Number of tiles in this TileSet
                       , tileSetOffset     :: V2 Int     -- ^ The offset between tiles. `V2 0 0` on default.
                       , tileSetProperties :: Properties -- ^ The properties of the tileset
                       } deriving (Show, Eq)
makeFields ''TileSet

data Orientation = Orthogonal | Isometric | Staggered          deriving (Read, Show, Eq)
data RenderOrder = RightDown  | RightUp   | LeftDown  | LeftUp deriving (Read, Show, Eq)

data TiledMap =
  TiledMap { tiledMapVersion     :: Version
           , tiledMapOrientation :: Orientation
           , tiledMapSize        :: V2 Int              -- ^ The Number of tiles in the map
           , tiledMapTileSize    :: V2 Int              -- ^ The Size of the tiles in the map
           , tiledMapRenderOrder :: RenderOrder         -- ^ The order the tiles are being rendered. RightDown per default
           , tiledMapProperties  :: Properties          -- ^ The properties of the map defined in a property-tag.
           , tiledMapTileSets    :: Map String TileSet  -- ^ The TileSets used in this map
-- TODO: implement , _bgColor     :: Maybe Color -- ^ The Background color of the map
           } deriving (Show, Eq)
makeFields ''TiledMap
