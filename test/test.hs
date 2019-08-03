{-# LANGUAGE PackageImports #-}
import Prelude ()

import Options.Applicative

import Game.LambdaHack.Client.UI.Frontend.Chosen
import Game.LambdaHack.Core.Prelude
import Game.LambdaHack.Server

-- For the case of flattened .cabal file, to avoid ambiguity.
import "Allure" TieKnot

main :: IO ()
main = do
  let args = words "--dbgMsgSer --logPriority 4 --newGame 1 --noAnim --maxFps 100000 --frontendNull --benchmark --stopAfterFrames 50 --automateAll --keepAutomated --gameMode crawl" ++ ["--setDungeonRng", "SMGen 123 123", "--setMainRng", "SMGen 123 123"]
  serverOptions <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
  tieKnot serverOptions
  when (frontendName == "sdl") $ do
    -- The hacky log priority 0 tells SDL frontend to init and quit at once,
    -- for testing on CIs without graphics access.
    let args2 = words "--dbgMsgSer --logPriority 0 --newGame 3 --maxFps 100000 --benchmark --stopAfterFrames 50 --automateAll --keepAutomated --gameMode battle" ++ ["--setDungeonRng", "SMGen 125 125", "--setMainRng", "SMGen 125 125"]
    serverOptions2 <- handleParseResult $ execParserPure defaultPrefs serverOptionsPI args2
    tieKnot serverOptions2
