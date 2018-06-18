module Main where

import           RFC.Prelude
import qualified Spec
import           Test.Hspec
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = hspecWith
	(defaultConfig
		{ configSmallCheckDepth = 8
		, configColorMode = ColorAlways
		, configQuickCheckMaxSuccess = Just 100
		, configPrintCpuTime = True
		, configRerun = True
		, configRerunAllOnSuccess = True
		, configFormatter = Just specdoc
		, configFailureReport = Just "./failures.hspec"
		}
	)
	Spec.spec

