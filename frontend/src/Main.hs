{-# language GADTs #-}
{-# language DataKinds #-}
{-# language RankNTypes #-}
-- {-# language TypeInType #-}
{-# language LambdaCase #-}
{-# language RecursiveDo #-}
{-# language InstanceSigs #-}
{-# language TypeFamilies #-}
{-# language DeriveGeneric #-}
{-# language KindSignatures #-}
{-# language ConstraintKinds #-}
{-# language PatternSynonyms #-}
{-# language FlexibleContexts #-}
{-# language TypeApplications #-}
{-# language FlexibleInstances #-}
{-# language NoImplicitPrelude #-}
{-# language ScopedTypeVariables #-}
{-# language TypeSynonymInstances #-}
{-# language ExtendedDefaultRules #-}
{-# language DuplicateRecordFields #-}
{-# language MultiParamTypeClasses #-}
{-# language NoPartialTypeSignatures #-}
{-# language NoMonomorphismRestriction #-}

module Main where

import            Prelude
-- import            Reflex.Dom
import            Reflex.Dom.Core
import            Reflex.Dom.Core -- (liftJSM)
import            Language.Javascript.JSaddle.Warp
import            Language.Javascript.JSaddle       (liftJSM, JSM)
-- import            Reflex.
import            Test.Hspec
-- import            Data.Text
import            Control.Monad.Trans
import            Control.Concurrent
import            Control.Monad

import qualified  GHCJS.DOM.Window                  as DOM
import qualified  GHCJS.DOM                         as DOM
import qualified  GHCJS.DOM                         as DOM (currentDocument, currentWindow)
import qualified  JSDOM                             as DOM (syncPoint)
import qualified  GHCJS.DOM.Document                as DOM -- (Document, createElement, getBody)
import qualified  GHCJS.DOM.Element                 as DOM
import qualified  GHCJS.DOM.HTMLElement             as DOM
import            Data.Time

debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

prnt = io . print

-- hspec $ do
main :: IO ()
main = do
  debugAndWait 3198 $ do
    let
      p = io . putStrLn . ("p: " ++)

    (Just doc) <- DOM.currentDocument
    (Just sel) <- DOM.getBody doc


    mainWidget $ do
      pb <- getPostBuild
      pb2 <- delay 0.1 pb
      text " hi1 "
      text " hi3 "
      time <- io getCurrentTime
      ticker <- tickLossy 0.1 time
      as <- count ticker
      display as
      text " hi4 "

    DOM.getInnerHTML sel >>= p

    io $ threadDelay $ 1000 * 1000
    DOM.getInnerHTML sel >>= p

    io $ threadDelay $ 1000 * 1000
    DOM.getInnerHTML sel >>= p

    mainWidget $ do
      pb <- getPostBuild
      pb2 <- delay 0.1 pb
      text " hi1 "
      text " hi3 "
      time <- io getCurrentTime
      ticker <- tickLossy 0.1 time
      as <- count ticker
      display as
      text " hi4 "

    DOM.getInnerHTML sel >>= p

    io $ threadDelay $ 1000 * 1000
    DOM.getInnerHTML sel >>= p

    io $ threadDelay $ 1000 * 1000
    DOM.getInnerHTML sel >>= p


jsm = liftJSM

-- testRender :: MonadWidget t m => m () -> JSM () -> JSM ()
-- testRender attachee tests = do

--   mainWidget attachee
--   tests

--   noop

-- -- testRender :: MonadWidget t m => m a -> m Text
-- -- testRender :: MonadWidget t m => m () -> m ()
-- testRender :: MonadWidget t m => m () -> m () -> m ()
-- testRender attachee tests = do
--   (event, trigger) <- newTriggerEvent
--   showHide <- holdDyn True event

--   dyn $ ffor showHide $ \case
--     True  -> do
--       attachee
--       tests
--       io $ trigger False
--       noop

--     False -> noop

--   noop

noop = pure ()
io = liftIO
