{-# OPTIONS_GHC -Wno-missing-fields #-}

module Decoder.Polygons (
  decPolygon,
  isInner,
  PolygonG (..),
  SPolygon (..),
  MPolygon,
  Point,
  GeoAction (..),
)
where

import Data.List
import qualified Data.List.Split as LS
import Data.Maybe
import Decoder.Helper
import Lens.Micro
import Lens.Micro.Extras

decodePolygonCommands :: [Int] -> [[GeoAction]]
decodePolygonCommands r = splitAtMove $ map singleDecoder (splitCommands r)
  where
    singleDecoder (l : ls) =
      GeoAction
        { _command = decodeCommand l
        , _parameters = tuplify $ map decodeParam ls
        }
    singleDecoder [] = error "should not happen"

absolutePolygonG :: SPolygon -> SPolygon
absolutePolygonG p =
  set (pClosePath . parameters) [closePath] $
    set (pLineTo . parameters) progSumLineTo p
  where
    sumMoveTo = foldl1 sumTuple (view (pMoveTo . parameters) p)
    progSumLineTo = drop 1 $ scanl sumTuple sumMoveTo (view (pLineTo . parameters) p)
    closePath = last $ view (pMoveTo . parameters) p

relativeMoveTo :: [SPolygon] -> [SPolygon]
relativeMoveTo = f []
  where
    f _ [] = []
    f acc (p : ps) =
      if not $ null acc
        then set pMoveTo (newMoveTo p acc) p : f (p : acc) ps
        else p : f (p : acc) ps
    newMoveTo p c =
      GeoAction
        { _command = view (pMoveTo . command) p
        , _parameters = zipWith sumTuple (view (pMoveTo . parameters) p) [sumMoveToAndLineTo c]
        }

-- refactor: is extractPoints even needed??
sumMoveToAndLineTo :: [SPolygon] -> Point
sumMoveToAndLineTo polygons =
  let extractPoints geoAction =
        if view (command . cmd) geoAction /= ClosePath
          then view parameters geoAction
          else []
      allPoints =
        concatMap
          ( \polygon ->
              extractPoints (view pMoveTo polygon) ++ extractPoints (view pLineTo polygon)
          )
          polygons
   in foldl' sumTuple coordsOrigin allPoints

decPolygon :: [Int] -> [PolygonG]
decPolygon = delegator . map absolutePolygonG . relativeMoveTo . (map actionToPolygon . decodePolygonCommands)
  where
    actionToPolygon :: [GeoAction] -> SPolygon
    actionToPolygon [x] = SPolygon{_pMoveTo = x, _pLineTo = x, _pClosePath = x}
    actionToPolygon g =
      SPolygon
        { _pMoveTo = head g
        , _pLineTo = g !! 1
        , _pClosePath = last g
        }
    delegator [x] = [SinglePolygon x]
    delegator xs =
      mapMaybe
        ( \e ->
            if length e == 1
              then
                Just $ SinglePolygon (head e)
              else
                MultiPolygon <$> uncons e
        )
        $ LS.split (LS.keepDelimsL $ LS.whenElt (not . isInner)) xs

-- criteria inner/outer polygon
-- https://en.wikipedia.org/wiki/Shoelace_formula
-- p1 = (1, 6), p2 = (3, 1), p3 = (7, 2)

--  |1 3|    |3 7|   |7  1|
--  |   | +  |   | + |    | -- matrix multp.
--  |6 1|    |1 2|   |2  6|

--  res is negative: inner polygon
polygonParams :: SPolygon -> [Point]
polygonParams (SPolygon pMoveTo' pLineTo' pClosePath') =
  concat
    [ pMoveTo' ^. parameters
    , pLineTo' ^. parameters
    , pClosePath' ^. parameters
    ]

shoelace :: [Point] -> Double
shoelace (p : ps) = sh' (p : ps) / 2
  where
    sh' [] = 0.0
    sh' [x] = shoelaceStep x fst'
    sh' (x : x' : xs) = shoelaceStep x x' + sh' (x' : xs)
    fst' = p
shoelace [] = error "empty"

isInner :: SPolygon -> Bool
isInner = (< 0) . shoelace . polygonParams
