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

import           Foo
import           Polysemy
import           Polysemy.Error

catchesImplicitError
  :: (Member Foo r, Member (Embed IO) r, Member (Error String) r) => Sem r Int
catchesImplicitError = do
  foo `catch` (\err -> embed @IO $ putStrLn $ "Caught error: " <> err)
  pure 1

failsToRunImplicitError :: (Member Foo r, Member (Embed IO) r) => Sem r Int
failsToRunImplicitError = do
  res <- runError @String $ foo
  case res of
    Left  err -> embed @IO $ putStrLn $ "Ran error: " <> err
    Right _   -> pure ()
  pure 2

main :: IO ()
main = do
  putStrLn "runError:"
  res <- runM $ runError @String $ runFoo $ failsToRunImplicitError
  print res
  putStrLn "---"
  putStrLn "catch:"
  res2 <- runM $ runError @String $ runFoo $ catchesImplicitError
  print res2
