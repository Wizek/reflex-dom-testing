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
import            Language.Javascript.JSaddle       (liftJSM, JSM, js1, js0, jsf, js, jsg)
import qualified  Language.Javascript.JSaddle       as JSA
-- import            Reflex.
-- import            Test.Hspec
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
import            Control.Lens
import            System.IO.Unsafe
-- import            Control.Exception
import            Control.Monad.Catch
import            System.Exit
import            Data.Monoid
import qualified  Data.Text as T
import qualified  Control.Concurrent.Lock           as Lock
import qualified Data.Traversable as T

debugAndWait p f = debug p f >> forever (threadDelay $ 1000 * 1000)

prnt :: (MonadIO m, Show a) => a -> m ()
prnt = io . print


tshow = T.pack . show


type HCS = HasCallStack




widget1 :: forall t m. (HCS, MonadWidget t m) =>  m ()
widget1 = do
  bClick <- button "Increment"
  -- let bClick = never
  cnt <- count bClick
  elAttr "div" ("id" =: "output") $ do
    display cnt

-- hspec $ do
mainTestwidget :: forall t m. (HCS, MonadWidget t m) => IORef (IO () -> IO (), El t) -> m () ->  m ()
mainTestwidget exfiltrate widgetToTest = do
  (elem, _) <- elClass' "div" "test-bench" widgetToTest

  (event, trigger :: IO () -> IO ()) <- newTriggerEvent
  io $ writeIORef exfiltrate (trigger, elem)

  dRender <- holdDyn False (True <$ event)

  dyn $ ffor dRender  $ \case
    False -> noop
    True  -> do
      el "script" $ text "window.reflexRenderDone()"

  noop


-- testWidget :: forall t m. (HCS, MonadWidget t m) => (forall m. MonadWidget t m => m ()) -> JSM (JSM ())
testWidget :: (forall t m. MonadWidget t m => m ()) -> JSM (JSM (), DOM.Element)
testWidget widgetToTest = do
  reflexRender <- io $ Lock.new

  -- triggerRef <- io $ newIORef $ const noop
  triggerRef <- io $ newIORef undefined
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
          -- prnt "trying to release"
          io $ Lock.release reflexRender
      ]

  mainWidget (mainTestwidget triggerRef widgetToTest)
  (trigger, elem) <- io $ readIORef triggerRef
  renderSync <- mkRenderSync trigger reflexRender

  renderSync
  -- return noop
  return (renderSync, _el_element elem)

(<<$>>) = fmap . fmap
(<<*>>) = (<*>) . (<*>)

main1 :: IO ()
main1 = do
  -- Just foo <- action1
  -- Just bar <- action2 foo

  -- Just bar <- pure <<$>> action2 <<*>> action1
  -- Just bar <- action2 `mbind` action1
  -- Just bar <- action2 foo

  Just bar <- join <$> (traverse action2 =<< action1)

  print bar

data A = A   deriving (Show)
data B = B   deriving (Show)

action1 :: IO (Maybe A)
action1 = return $ Just A

action2 :: A -> IO (Maybe B)
action2 A = return $ Just B

mbind :: (Monad m, Monad n, Functor m, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
mbind = (join .) . fmap . (fmap join .) . T.mapM


act `shouldBe` exp
  | act == exp = io $ putStrLn $ "OK: " <> show act
  | otherwise  = do
      io $ putStrLn $ "FAIL, actual: " <> show act <> ", expected: " <> show exp
      mainThreadId <- io $ readIORef globalThreadId
      io $ killThread mainThreadId

  -- | otherwise  = io $ evaluate $ error $ "FAIL, actual: " <> show act <> "expected: " <> show exp

act `shouldReturn` exp = act >>= (`shouldBe` exp)


globalThreadId :: IORef ThreadId
globalThreadId = unsafePerformIO $ newIORef undefined

-- errorHandler mainThreadId cont = do
--   io $ catch $ cont $ \(e :: SomeException) -> do
--     prnt e
--     io $ killThread mainThreadId

main :: IO ()
main = do
  mainThreadId <- myThreadId

  writeIORef globalThreadId mainThreadId
  -- forkIO $ do

  -- debugAndWait 3198 $ errorHandler mainThreadId $ do
  debugAndWait 3198 $ do

    (renderSync, elem) <- testWidget widget1

    (jsg "document" ^. js1 "getElementById" "output" . js "innerHTML" >>= JSA.fromJSVal)
      `shouldReturn` Just "0"

    elem ^.js1 "getElementsByTagName" "button" . js "0" . js0 "click"
    renderSync

    (jsg "document" ^. js1 "getElementById" "output" . js "innerHTML" >>= JSA.fromJSVal)
      `shouldReturn` Just "1"


jsEval = jsm . JSA.eval


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
