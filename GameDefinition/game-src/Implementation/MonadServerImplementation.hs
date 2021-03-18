-- Copyright (c) 2008--2011 Andres Loeh
-- Copyright (c) 2010--2021 Mikolaj Konarski and others (see git history)
-- This file is a part of the computer game Allure of the Stars
-- and is released under the terms of the GNU Affero General Public License.
-- For license and copyright information, see the file LICENSE.
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | The implementation of our custom game server monads. Just as any other
-- component of the library, this implementation can be substituted.
module Implementation.MonadServerImplementation
  ( executorSer
#ifdef EXPOSE_INTERNAL
    -- * Internal operations
  , SerState(..), SerImplementation(..)
#endif
  ) where

import Prelude ()

import Game.LambdaHack.Core.Prelude

import           Control.Concurrent
import qualified Control.Exception as Ex
import qualified Control.Monad.IO.Class as IO
import           Control.Monad.Trans.State.Strict hiding (State)
import qualified Data.EnumMap.Strict as EM
import qualified Data.Text.IO as T
import           Options.Applicative
  (defaultPrefs, execParserPure, handleParseResult)
import           System.Exit (ExitCode (ExitSuccess))
import           System.FilePath
import           System.IO (hFlush, stdout)

import           Game.LambdaHack.Atomic
import           Game.LambdaHack.Client
import           Game.LambdaHack.Common.ClientOptions (sbenchmark)
import           Game.LambdaHack.Common.File
import           Game.LambdaHack.Common.Kind
import           Game.LambdaHack.Common.Misc
import           Game.LambdaHack.Common.MonadStateRead
import qualified Game.LambdaHack.Common.Save as Save
import           Game.LambdaHack.Common.State
import           Game.LambdaHack.Common.Thread
import           Game.LambdaHack.Server
import           Game.LambdaHack.Server.BroadcastAtomic
import           Game.LambdaHack.Server.HandleAtomicM
import           Game.LambdaHack.Server.MonadServer
import           Game.LambdaHack.Server.ProtocolM
import           Game.LambdaHack.Server.ServerOptions
import           Game.LambdaHack.Server.State

import Implementation.MonadClientImplementation (executorCli)

data SerState = SerState
  { serState  :: State           -- ^ current global state
  , serServer :: StateServer     -- ^ current server state
  , serDict   :: ConnServerDict  -- ^ client-server connection information
  , serToSave :: Save.ChanSave (State, StateServer)
                                 -- ^ connection to the save thread
  }

-- | Server state transformation monad.
newtype SerImplementation a =
    SerImplementation {runSerImplementation :: StateT SerState IO a}
  deriving (Monad, Functor, Applicative)

instance MonadStateRead SerImplementation where
  {-# INLINE getsState #-}
  getsState f = SerImplementation $ gets $ f . serState

instance MonadStateWrite SerImplementation where
  {-# INLINE modifyState #-}
  modifyState f = SerImplementation $ state $ \serS ->
    let !newSerS = serS {serState = f $ serState serS}
    in ((), newSerS)
  {-# INLINE putState #-}
  putState newSerState = SerImplementation $ state $ \serS ->
    let !newSerS = serS {serState = newSerState}
    in ((), newSerS)

instance MonadServer SerImplementation where
  {-# INLINE getsServer #-}
  getsServer   f = SerImplementation $ gets $ f . serServer
  {-# INLINE modifyServer #-}
  modifyServer f = SerImplementation $ state $ \serS ->
    let !newSerS = serS {serServer = f $ serServer serS}
    in ((), newSerS)
  chanSaveServer = SerImplementation $ gets serToSave
  liftIO         = SerImplementation . IO.liftIO

instance MonadServerComm SerImplementation where
  {-# INLINE getsDict #-}
  getsDict   f = SerImplementation $ gets $ f . serDict
  {-# INLINE modifyDict #-}
  modifyDict f = SerImplementation $ state $ \serS ->
    let !newSerS = serS {serDict = f $ serDict serS}
    in ((), newSerS)
  liftIO = SerImplementation . IO.liftIO

instance MonadServerAtomic SerImplementation where
  execUpdAtomic cmd = do
    oldState <- getState
    (ps, atomicBroken, executedOnServer) <- handleCmdAtomicServer cmd
    when executedOnServer $ cmdAtomicSemSer oldState cmd
    handleAndBroadcast ps atomicBroken (UpdAtomic cmd)
  execUpdAtomicSer cmd = SerImplementation $ StateT $ \cliS -> do
    cliSNewOrE <- Ex.try
                  $ execStateT (runSerImplementation $ handleUpdAtomic cmd)
                               cliS
    case cliSNewOrE of
      Left AtomicFail{} -> return (False, cliS)
      Right !cliSNew ->
        -- We know @cliSNew@ differs only in @serState@.
        return (True, cliSNew)
  execUpdAtomicFid fid cmd = SerImplementation $ StateT $ \cliS -> do
    -- Don't catch anything; assume exceptions impossible.
    let sFid = sclientStates (serServer cliS) EM.! fid
    cliSNew <- execStateT (runSerImplementation $ handleUpdAtomic cmd)
                          cliS {serState = sFid}
    -- We know @cliSNew@ differs only in @serState@.
    let serServerNew = (serServer cliS)
          {sclientStates = EM.insert fid (serState cliSNew)
                           $ sclientStates $ serServer cliS}
        !newCliS = cliS {serServer = serServerNew}
    return ((), newCliS)
  execUpdAtomicFidCatch fid cmd = SerImplementation $ StateT $ \cliS -> do
    let sFid = sclientStates (serServer cliS) EM.! fid
    cliSNewOrE <- Ex.try
                  $ execStateT (runSerImplementation $ handleUpdAtomic cmd)
                               cliS {serState = sFid}
    case cliSNewOrE of
      Left AtomicFail{} -> return (False, cliS)
      Right cliSNew -> do
        -- We know @cliSNew@ differs only in @serState@.
        let serServerNew = (serServer cliS)
              {sclientStates = EM.insert fid (serState cliSNew)
                               $ sclientStates $ serServer cliS}
            !newCliS = cliS {serServer = serServerNew}
        return (True, newCliS)
  execSfxAtomic sfx = do
    ps <- posSfxAtomic sfx
    handleAndBroadcast ps [] (SfxAtomic sfx)
  execSendPer = sendPer

-- Don't inline this, to keep GHC hard work inside the library
-- for easy access of code analysis tools.
-- | Run the main server loop, with the given arguments and empty
-- initial states, in the @IO@ monad.
executorSer :: COps -> CCUI -> ServerOptions -> UIOptions -> IO ()
executorSer cops@COps{corule} ccui soptionsNxtCmdline sUIOptions = do
  soptionsNxtRaw <- case uCmdline sUIOptions of
    []   -> return soptionsNxtCmdline
    args -> handleParseResult $ execParserPure defaultPrefs serverOptionsPI args
  -- Options for the clients modified with the configuration file.
  let clientOptions = applyUIOptions cops sUIOptions
                      $ sclientOptions soptionsNxtRaw
      soptionsNxt = soptionsNxtRaw {sclientOptions = clientOptions}
      -- Partially applied main loop of the clients.
      executorClient = executorCli ccui sUIOptions clientOptions cops
  -- Wire together game content, the main loop of game clients
  -- and the game server loop.
  let stateToFileName (_, ser) =
        ssavePrefixSer (soptions ser) <> Save.saveNameSer corule
      totalState serToSave = SerState
        { serState = updateCOpsAndCachedData (const cops) emptyState
            -- state is empty, so the cached data is left empty and untouched
        , serServer = emptyStateServer
        , serDict = EM.empty
        , serToSave
        }
      m = loopSer soptionsNxt executorClient
      exe = evalStateT (runSerImplementation m) . totalState
      exeWithSaves = Save.wrapInSaves cops stateToFileName exe
      defPrefix = ssavePrefixSer defServerOptions
      bkpOneSave name = do
        dataDir <- appDataDir
        let path bkp = dataDir </> "saves" </> bkp <> name
        b <- doesFileExist (path "")
        when b $ renameFile (path "") (path "bkp.")
      bkpAllSaves = unless (sbenchmark $ sclientOptions soptionsNxt) $ do
        T.hPutStrLn stdout "The game crashed, so savefiles are moved aside."
        bkpOneSave $ defPrefix <> Save.saveNameSer corule
        forM_ [-199..199] $ \n ->
          bkpOneSave $ defPrefix <> Save.saveNameCli corule (toEnum n)
  -- Wait for clients to exit even in case of server crash
  -- (or server and client crash), which gives them time to save
  -- and report their own inconsistencies, if any.
  Ex.handle (\ex -> case Ex.fromException ex of
               Just ExitSuccess ->
                 -- User-forced shutdown, not crash, so the intention is
                 -- to keep old saves and also clients may be not ready to save.
                 Ex.throwIO ex
               _ -> do
                 Ex.uninterruptibleMask_ $ threadDelay 1000000
                   -- let clients report their errors and save
                 when (ssavePrefixSer soptionsNxt == defPrefix) bkpAllSaves
                 hFlush stdout
                 Ex.throwIO ex  -- crash eventually, which kills clients
            )
            exeWithSaves
--  T.hPutStrLn stdout "Server exiting, waiting for clients."
--  hFlush stdout
  waitForChildren childrenServer  -- no crash, wait for clients indefinitely
--  T.hPutStrLn stdout "Server exiting now."
--  hFlush stdout
