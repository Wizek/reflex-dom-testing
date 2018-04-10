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
import            ComposeLTR
import            Text.InterpolatedString.Perl6
import            Data.IORef
import            Debug.Trace
import qualified  Control.Concurrent.Lock           as Lock

debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

prnt :: (MonadIO m, Show a) => a -> m ()
prnt = io . print



-- hspec $ do
widget :: forall t m. MonadWidget t m => IORef (IO () -> IO ()) ->  m ()
widget exfiltrate = do
  text " hi1 "
  -- time <- io getCurrentTime
  -- ticker <- tickLossy 0.1 time
  -- as <- count ticker
  -- display as
  -- MonadWidget m => m (Event ())
  bClick <- button "test"
  text " "
  cnt <- count bClick
  display $ fmap (trace "trace") $ cnt

  text " hi2 "

  (event, trigger :: IO () -> IO ()) <- newTriggerEvent
  io $ writeIORef exfiltrate  trigger

  performEvent_ $ ffor (event :: Event t (IO ())) $ \cont -> do
    prnt "performEvent_ inside"
    io $ cont
    -- io $ print 123123332

  noop

main :: IO ()
main = do
  debugAndWait 3198 $ do
    let
      p = io . putStrLn . ("p: " ++)

    (Just doc) <- DOM.currentDocument
    (Just sel) <- DOM.getBody doc

    triggerRef <- io $ newIORef $ const noop

    mainWidget (widget triggerRef)

    trigger <- io $ readIORef triggerRef

      -- withRenderHook (\a -> do
      --     prnt 2
      --     ret <- a
      --     prnt 3
      --     pure ret
      --     -- pure
      --   ) $ do
      --   prnt 4444
      --   text " hi6 "

      -- noop

    DOM.syncPoint


    -- io $ threadDelay $ 1000 * 10
    l1 <- io $ Lock.newAcquired
    io $ trigger $ do
      print 999999
      Lock.release l1
    io $ Lock.wait l1

    DOM.getInnerHTML sel >>= p
    -- io $ threadDelay $ 10000
    DOM.syncPoint
    -- DOMe.getElementsByTagName sel "button" >>= JSA.toJSVal >>= prnt
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
      -- JSA.call jsFun JSA.global [()]
      JSA.call jsFun JSA.global
        [ JSA.asyncFunction $ \_ _ _ -> do
            prnt 123
            io $ Lock.release lock
        ]


    -- io $ threadDelay $ 10000
    DOM.syncPoint
    io $ Lock.wait lock
    DOM.syncPoint
    -- io $ threadDelay $ 1000 * 100
    -- withRenderHook (id) noop
    -- io $ threadDelay $ 1000 * 100

    l2 <- io $ Lock.newAcquired
    io $ trigger $ do
      print 999999
      Lock.release l2

    io $ Lock.wait l2

    DOM.getInnerHTML sel >>= p

    -- io $ threadDelay $ 10000



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
