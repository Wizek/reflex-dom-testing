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
import qualified  Language.Javascript.JSaddle       as JSA
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
import qualified  GHCJS.DOM.Element                 as DOMe
import qualified  GHCJS.DOM.HTMLElement             as DOM
import            Data.Time
import            ComposeLTR                        hiding ((<$))
import            Text.InterpolatedString.Perl6
import            Data.IORef
import            GHC.Stack
import            Debug.Trace
import            Data.Monoid
import qualified  Data.Text as T
import qualified  Control.Concurrent.Lock           as Lock

debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

prnt :: (MonadIO m, Show a) => a -> m ()
prnt = io . print


tshow = T.pack . show


type HCS = HasCallStack



widget1 :: forall t m. (HCS, MonadWidget t m) =>  m ()
widget1 = do
  text " hi1 "
  bClick <- button "test"
  text " "
  cnt <- count bClick
  display $ fmap (trace "trace") $ cnt

  text " hi2 "

-- hspec $ do
mainTestwidget :: forall t m. (HCS, MonadWidget t m) => IORef (IO () -> IO ()) -> m () ->  m ()
mainTestwidget exfiltrate widgetToTest = do
  widgetToTest

  (event, trigger :: IO () -> IO ()) <- newTriggerEvent
  io $ writeIORef exfiltrate trigger

  dRender <- holdDyn False (True <$ event)

  dyn $ ffor dRender  $ \case
    False -> noop
    True  -> do
      el "script" $ text $ "window.reflexRenderDone()"

  noop

main :: IO ()
main = do
  debugAndWait 3198 $ do
    let
      p = io . putStrLn . ("p: " ++)

    (Just doc) <- DOM.currentDocument
    (Just sel) <- DOM.getBody doc

    -- reflexRender <- io $ Lock.newAcquired
    reflexRender <- io $ Lock.new

    triggerRef <- io $ newIORef $ const noop
    jsm $ do
      jsFun <- JSA.eval [q|(function(cb) {
        try {
          console.log("reflexRenderDone defined")
          window.reflexRenderDone = function () {
            console.log("reflexRenderDone called")
            cb()
          }
        } catch (e) {
          console.error(e)
        }
      })|]
      JSA.call jsFun JSA.global
        [ JSA.asyncFunction $ \_ _ _ -> do
            prnt "trying to release"
            io $ Lock.release reflexRender
        ]

    mainWidget (mainTestwidget triggerRef widget1)
    trigger <- io $ readIORef triggerRef
    renderSync <- mkRenderSync trigger reflexRender

    renderSync

    DOM.getInnerHTML sel >>= p
    lock <- io Lock.newAcquired

    jsm $ do
      jsFun <- JSA.eval [q|(function(cb) {
        try {
          console.log(123)

          var a = document.getElementsByTagName("button")[0].click()

          setTimeout(function () {
            cb()
          })

          // console.log(a)
        } catch (e) {
          console.error(e)
        }
      })|]
      JSA.call jsFun JSA.global
        [ JSA.asyncFunction $ \_ _ _ -> do
            prnt 123
            io $ Lock.release lock
        ]

    renderSync

    DOM.getInnerHTML sel >>= p



mkRenderSync trigger reflexRender = do
  return $ do
    io $ Lock.acquire reflexRender
    io $ trigger noop
    io $ Lock.wait reflexRender


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
