
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (map)

import System.IO.Unsafe (unsafePerformIO)
import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (fromJust)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef (IORef, newIORef, writeIORef, readIORef, modifyIORef)

import qualified Miso as M
import Miso (View, App(..), startApp)
import Miso.String (MisoString, toMisoString, fromMisoString)

import Debug.Trace

data Expr = Literal Float | Operation Expr Op Expr deriving (Show, Ord, Eq)
data Op = Add | Sub | Mul | Div deriving (Show, Ord, Eq)

eval :: Expr -> Float
eval (Literal x) = x
eval (Operation lhs op rhs) = (eval lhs) `fun` (eval rhs)
  where fun = case op of { Add -> (+); Sub -> (-); Mul -> (*); Div -> (/) }

data Expr'
  = Literal' Float Changedness
  | Operation' Expr' Op' Expr' Changedness
  deriving (Show, Ord, Eq)

data Op'
  = Add' Changedness
  | Sub' Changedness
  | Mul' Changedness
  | Div' Changedness
  deriving (Show, Ord, Eq)

data Changedness = Changed | Unchanged
  deriving (Show, Ord, Eq)

class Nabla nabla where
  changedness :: nabla -> Changedness
  markAsUnchanged :: nabla -> nabla

class Nabla nabla => NablaFor base nabla where
  base :: nabla -> base

wasChanged :: Nabla nabla => nabla -> Bool
wasChanged = changedness <&> (== Changed)

wasn'tChanged :: Nabla nabla => nabla -> Bool
wasn'tChanged = not . wasChanged

instance Nabla Expr' where
  changedness (Literal' _ c) = c
  changedness (Operation' _ _ _ c) = c

  markAsUnchanged (Literal' x _) = Literal' x Unchanged
  markAsUnchanged (Operation' lhs' op' rhs' _) = Operation' (markAsUnchanged lhs') (markAsUnchanged op') (markAsUnchanged rhs') Unchanged

instance NablaFor Expr Expr' where
  base (Literal' x _) = Literal x
  base (Operation' lhs' op' rhs' _) = Operation (base lhs') (base op') (base rhs')

instance Nabla Op' where
  changedness (Add' c) = c
  changedness (Sub' c) = c
  changedness (Mul' c) = c
  changedness (Div' c) = c

  markAsUnchanged (Add' _) = Add' Unchanged
  markAsUnchanged (Sub' _) = Sub' Unchanged
  markAsUnchanged (Mul' _) = Mul' Unchanged
  markAsUnchanged (Div' _) = Div' Unchanged

instance NablaFor Op Op' where
  base (Add' _) = Add
  base (Sub' _) = Sub
  base (Mul' _) = Mul
  base (Div' _) = Div

main :: IO ()
main = startApp App
  { M.initialAction = id
  , M.model = Literal' 0 Changed
  , M.update = \step model -> return (traceShowId $ step model)
  , M.view = \model -> let view = unsafePerformIO $ readIORef viewRef in (view model) id
  , M.events = M.defaultEvents
  , M.subs = []
  , M.mountPoint = Nothing
  , M.logLevel = M.Off
  }

shifting :: forall base arg res x y. (NablaFor base arg, Ord base) => IORef (x -> y) -> (arg -> res) -> (arg -> res)
shifting shifterRef shiftee = unsafePerformIO $ do

  putCacheRef :: IORef (Map base res) <- newIORef Map.empty
  getCacheRef :: IORef (Map base res) <- newIORef Map.empty

  let shift = do
        writeIORef getCacheRef =<< readIORef putCacheRef
        writeIORef putCacheRef Map.empty

  modifyIORef shifterRef (>>> \result -> unsafePerformIO $ shift *> pure result)

  let memoized param = unsafePerformIO $ do
        result <-
          if wasn'tChanged param
          then readIORef getCacheRef <&> confidentLookup (base param)
               & trace "using cache"
          else return $ shiftee param
               & trace "no cache"
        modifyIORef putCacheRef (Map.insert (base param) result)
        return result 

  return memoized

 where
    -- Map lookup assuming existence of an associated value
    confidentLookup :: Ord k => k -> Map k v -> v
    confidentLookup key map
      | Map.size map == 1 = Map.elems map !! 0
      | otherwise = fromJust $ Map.lookup key map

viewRef :: IORef (Model -> Component there Model)
viewRef = unsafePerformIO $ newIORef viewModel

shifty :: forall base arg res. (NablaFor base arg, Ord base) => (arg -> res) -> (arg -> res)
shifty = shifting @base viewRef

type Step a = a -> a
type Nestle here there = Step here -> Step there
type Model = Expr'
type Html model = View (Step model)
type Component there here = Nestle here there -> Html there

viewModel :: Model -> Component there Model
viewModel = shifty @Expr $ \expr' ->
        \nestle ->
          M.div_ []
            [ M.link_ [ M.rel_ "stylesheet", M.href_ "../../../main.css" ]
            , (viewExpr' expr') nestle
            , M.text " = "
            , M.text . toMisoString . show $ eval (base expr')
            ]

viewExpr' :: Expr' -> Component there Expr'
viewExpr' = shifty @Expr $ \expr' nestle ->
  case expr' of

    Literal' x _ ->
      M.span_ [ M.class_ "number" ]
        [ M.input_ [ M.type_ "number"
                   , M.value_ (toMisoString . show $ (round x :: Int))
                   , M.onChange (\str -> nestle $ \_model -> Literal' (read $ fromMisoString str) Changed)
                   ]
        , M.text " "
        , M.button_ [ M.onClick (nestle $ \model -> Operation' model (Add' Changed) model Changed) ] [] -- TODO: repositioning
        ]

    Operation' lhs' op' rhs' _ ->
        let
          lhsHtml  = viewExpr' lhs' $ \step -> nestle $ \(Operation' l o r _) -> Operation' (step l) (markAsUnchanged o) (markAsUnchanged r) Changed
          opHtml   = viewOp'   op'  $ \step -> nestle $ \(Operation' l o r _) -> Operation' (markAsUnchanged l) (step o) (markAsUnchanged r) Changed
          rhsHtml  = viewExpr' rhs' $ \step -> nestle $ \(Operation' l o r _) -> Operation' (markAsUnchanged l) (markAsUnchanged o) (step r) Changed
          -- ^ TODO: for next version, switch from 'markAsUnchanged' to using paths (perhaps) which wouldnt need 'markAsUnchanged'
          in M.span_ [ M.class_ "parens" ] [ lhsHtml, M.text " ", opHtml, M.text " ", rhsHtml ]

viewOp' :: Op' -> Component there Op'
viewOp' = shifty @Op $ \op' nestle ->
        M.select_
          [ M.onChange (\str -> nestle $ \_model -> unstringify str) ]
          ([Add, Sub, Mul, Div] <&> \op ->
            M.option_ [ M.selected_ (op == base op')
                      , M.value_ (stringify op)
                      ]
                      [ M.text . stringify $ op ])

stringify :: Op -> MisoString
stringify = (\case
      Add -> toMisoString ("+" :: String)
      Sub -> toMisoString ("-" :: String)
      Mul -> toMisoString ("×" :: String)
      Div -> toMisoString ("÷" :: String))

unstringify :: MisoString -> Op'
unstringify = (\s ->
      case fromMisoString s :: String of
        "+" -> Add' Changed
        "-" -> Sub' Changed
        "×" -> Mul' Changed
        "÷" -> Div' Changed
        _ -> undefined)
