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
import            Language.Javascript.JSaddle.Warp -- (run)
-- import            Reflex.
import            Test.Hspec
-- import            Data.Text
import            Control.Monad.Trans
import            Control.Concurrent
import            Control.Monad
-- import qualified  GHCJS.DOM.Window                  as DOM
import qualified  GHCJS.DOM                         as DOM (currentDocument, currentWindow)

debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

-- hspec $ do
main :: IO ()
main = do
  debugAndWait 3198 $ mainWidget $ do

    -- a <- testRender (text "test1")
    -- if a == "test1" then pure () else error "asdasd"
    -- testRender (text "test1") `shouldBe` "test1"

    text " hi1"

    text " hi3"

    testRender (text "test1") $ do
      io $ threadDelay $ 1000 * 1000
      pure ()
      -- html `shouldReturn` "test1"

  -- error "asdasd"

-- testRender :: MonadWidget t m => m a -> m Text
-- testRender :: MonadWidget t m => m () -> m ()
testRender :: MonadWidget t m => m () -> m () -> m ()
testRender attachee tests = do
  (event, trigger) <- newTriggerEvent
  showHide <- holdDyn True event

  dyn $ ffor showHide $ \case
    True  -> do
      attachee
      tests
      io $ trigger False
      noop

    False -> noop

  noop

noop = pure ()
io = liftIO
