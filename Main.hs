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

import           Polysemy                       ( Member
                                                , Sem
                                                , runM
                                                )
import           Polysemy.Error                 ( Error
                                                , catch
                                                , runError
                                                )

import           Foo

program :: forall r . (Member Foo r) => Sem r Int
program = do
  foo ()
  pure 1

main :: IO ()
main = do
  res <- runM $ runError @String $ runFoo $ program
  print res
