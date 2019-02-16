{-# LANGUAGE OverloadedStrings #-}

module FunctionalBadProjectSpec where

import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Language.Haskell.LSP.Test hiding (message)
import           Language.Haskell.LSP.Types as LSP
import           Language.Haskell.LSP.Types.Lens as LSP hiding (contents, error )
import           Test.Hspec
import           TestUtils
import           Utils

-- ---------------------------------------------------------------------

spec :: Spec
spec = describe "behaviour on malformed projects" $ do
    it "deals with cabal file with unsatisfiable dependency" $
      runSession hieCommandExamplePlugin codeActionSupportCaps "test/testdata/badProjects/cabal" $ do
      -- runSessionWithConfig logConfig hieCommandExamplePlugin codeActionSupportCaps "test/testdata" $ do
        _doc <- openDoc "Foo.hs" "haskell"

        diags@(d:_) <- waitForDiagnosticsSource "ghcmod"
        -- liftIO $ show diags `shouldBe` ""
        -- liftIO $ putStrLn $ show diags
        -- liftIO $ putStrLn "a"
        liftIO $ do
          length diags `shouldBe` 1
          d ^. range `shouldBe` Range (Position 0 0) (Position 1 0)
          d ^. severity `shouldBe` (Just DsError)
          d ^. code `shouldBe` Nothing
          d ^. source `shouldBe` Just "ghcmod"
          let prefix = "callProcessStderr: stack --stack-yaml="
              suffix = "stack.yaml build --only-configure . (exit 1): failed\n"
          T.take    (length prefix) (d ^. message) `shouldBe` (T.pack prefix)
          T.takeEnd (length suffix) (d ^. message) `shouldBe` (T.pack suffix)

                    -- "callProcessStderr: stack --stack-yaml=/home/alanz/mysrc/github/alanz/haskell-ide-engine/test/testdata/badProjects/cabal/stack.yaml build --only-configure . (exit 1): failed\n"
    -- ---------------------------------

-- ---------------------------------------------------------------------
