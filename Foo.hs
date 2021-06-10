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

module Foo
  ( Foo
  , runFoo
  , foo
  ) where

import           Polysemy                       ( Member
                                                , Sem
                                                , interpret
                                                , makeSem
                                                )
import           Polysemy.Error                 ( Error
                                                , throw
                                                )

data Foo m a where
    Foo ::b -> Foo m ()

makeSem ''Foo

runFoo :: Member (Error String) r => Sem (Foo ': r) a -> Sem r a
runFoo = interpret $ \(Foo _) -> throw "Bar"
