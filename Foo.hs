{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Foo where

import           Polysemy
import           Polysemy.Error

data Foo m a where
    Foo ::Foo m ()

makeSem ''Foo

runFoo :: Member (Error String) r => Sem (Foo ': r) a -> Sem r a
runFoo = interpret $ \Foo -> throw "Bar"
