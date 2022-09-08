#! /usr/bin/env runghc

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedRecordDot #-}

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import GHC.IO.Handle
import System.IO
import Control.Concurrent
import System.Process
import System.Posix.Signals
import System.Directory
import System.Environment
import System.Console.GetOpt

type Verbosity = Int

lvlPrint :: Verbosity -> Verbosity -> String -> IO ()
lvlPrint reqLvl lvl str | lvl >= reqLvl = putStrLn str
lvlPrint _ _ _ = return ()

data CHERI_BGAS_Sim_Instance = CHERI_BGAS_Sim_Instance {
    x :: Int
  , y :: Int
  , path_sim_folder :: String
  , cmd_cheri_bgas_sim :: String
  , cmd_cheri_bgas_devfs :: String
  , cmd_gdbstub :: String
  , sim_handles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  , devfs_handles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  , gdbstub_handles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
}

data CHERI_BGAS_Sim_Connection = CHERI_BGAS_Sim_Connection {
    from :: CHERI_BGAS_Sim_Instance
  , to :: CHERI_BGAS_Sim_Instance
  , cmd :: String
  , tx_rx_handles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
  , rx_tx_handles :: (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
}

spawn_CHERI_BGAS_Sim_Instance :: Verbosity
                              -> (Int, Int) -> Int
                              -> String -> String -> String -> String
                              -> IO (CHERI_BGAS_Sim_Instance)
spawn_CHERI_BGAS_Sim_Instance v (x, y) pBase dir simCmd devfsCmd gdbstubCmd = do
  lvlPrint 2 v $ "Creating directory " ++ dir
  createDirectoryIfMissing True dir
  withCurrentDirectory dir do

    simStdout <- openFile ("sim_stdout") AppendMode
    simStderr <- openFile ("sim_stderr") AppendMode
    let simProc = (proc simCmd []) {
        std_in = CreatePipe --NoStream
      , std_out = UseHandle simStdout
      , std_err = UseHandle simStderr
      }
    lvlPrint 2 v $ "Spawning simulator: " ++ show simProc
    simHandles <- createProcess simProc

    threadDelay 50000

    devfsStdout <- openFile ("devfs_stdout") AppendMode
    devfsStderr <- openFile ("devfs_stderr") AppendMode
    let devfsDir = "simdev/"
    createDirectoryIfMissing True devfsDir
    let devfsProc = (proc devfsCmd ["simports/", devfsDir]) {
        std_in = CreatePipe --NoStream
      , std_out = UseHandle devfsStdout
      , std_err = UseHandle devfsStderr
      }
    lvlPrint 2 v $ "Spawning devfs: " ++ show devfsProc
    devfsHandles <- createProcess devfsProc

    threadDelay 50000

    gdbstubStdout <- openFile ("gdbstub_stdout") AppendMode
    gdbstubStderr <- openFile ("gdbstub_stderr") AppendMode
    let fmemDev = "simdev/debug_unit"
    let port = pBase + 100 * x + y
    let gdbstubProc = (proc gdbstubCmd []) {
        std_in = CreatePipe --NoStream
        , std_out = UseHandle gdbstubStdout
        , std_err = UseHandle gdbstubStderr
        , env = Just [ ("RISCV_GDB_STUB_FMEM_DEV", fmemDev)
                     , ("RISCV_GDB_STUB_PORT", show port) ]
      }
    lvlPrint 2 v $ "Spawning gdbstub: " ++ show gdbstubProc
    gdbstubHandles <- createProcess gdbstubProc

    return CHERI_BGAS_Sim_Instance {
        x = x
      , y = y
      , path_sim_folder = dir
      , cmd_cheri_bgas_sim = simCmd
      , cmd_cheri_bgas_devfs = devfsCmd
      , cmd_gdbstub = gdbstubCmd
      , sim_handles = simHandles
      , devfs_handles = devfsHandles
      , gdbstub_handles = gdbstubHandles
    }

spawn_CHERI_BGAS_Sim_Connection :: Verbosity
                                -> String -> String
                                -> Map.Map (Int, Int) CHERI_BGAS_Sim_Instance
                                -> ((Int, Int), (Int, Int))
                                -> IO (CHERI_BGAS_Sim_Connection)
spawn_CHERI_BGAS_Sim_Connection v cmd simFolder insts ((x0, y0), (x1, y1)) = do
  let t0 = fromMaybe (error "bad tile coord") $ Map.lookup (x0, y0) insts
  let t1 = fromMaybe (error "bad tile coord") $ Map.lookup (x1, y1) insts
  let conn = CHERI_BGAS_Sim_Connection {
      from = t0
    , to = t1
    , cmd = cmd
    , tx_rx_handles = error "no handles yet!"
    , rx_tx_handles = error "no handles yet!"
    }
  -- t0 <-> t1
  -- S <-> N
  if (x0 == x1 && y0 < y1) then do
    tr <- connectTXRX (nTXPipe t0.path_sim_folder) (sRXPipe t1.path_sim_folder)
    rt <- connectTXRX (sTXPipe t1.path_sim_folder) (nRXPipe t0.path_sim_folder)
    return conn {tx_rx_handles = tr, rx_tx_handles = rt}
  -- N <-> S
  else if (x0 == x1 && y0 > y1) then do
    tr <- connectTXRX (sTXPipe t0.path_sim_folder) (nRXPipe t1.path_sim_folder)
    rt <- connectTXRX (nTXPipe t1.path_sim_folder) (sRXPipe t0.path_sim_folder)
    return conn {tx_rx_handles = tr, rx_tx_handles = rt}
  -- W <-> E
  else if (y0 == y1 && x0 < x1) then do
    tr <- connectTXRX (eTXPipe t0.path_sim_folder) (wRXPipe t1.path_sim_folder)
    rt <- connectTXRX (wTXPipe t1.path_sim_folder) (eRXPipe t0.path_sim_folder)
    return conn {tx_rx_handles = tr, rx_tx_handles = rt}
  -- E <-> W
  else if (y0 == y1 && x0 > x1) then do
    tr <- connectTXRX (wTXPipe t0.path_sim_folder) (eRXPipe t1.path_sim_folder)
    rt <- connectTXRX (eTXPipe t1.path_sim_folder) (wRXPipe t0.path_sim_folder)
    return conn {tx_rx_handles = tr, rx_tx_handles = rt}
  else error "Unhandled connection"
  --
  where nRXPipe sf = globalPorts sf ++ "/north/rx"
        nTXPipe sf = globalPorts sf ++ "/north/tx"
        eRXPipe sf = globalPorts sf ++ "/east/rx"
        eTXPipe sf = globalPorts sf ++ "/east/tx"
        sRXPipe sf = globalPorts sf ++ "/south/rx"
        sTXPipe sf = globalPorts sf ++ "/south/tx"
        wRXPipe sf = globalPorts sf ++ "/west/rx"
        wTXPipe sf = globalPorts sf ++ "/west/tx"
        globalPorts sf = sf ++ "/simports/bgas-global-ports"
        dir = simFolder ++ "/conn__" ++ show x0 ++ "_" ++ show y0
                        ++ "__" ++ show x1 ++ "_" ++ show y1
        connectTXRX tx rx = do
          lvlPrint 2 v $ "Creating directory " ++ dir
          createDirectoryIfMissing True dir
          withCurrentDirectory dir do
            let p = proc cmd [tx, rx]
            connStdout <- openFile ("conn_stdout") AppendMode
            connStderr <- openFile ("conn_stderr") AppendMode
            lvlPrint 2 v $ "Spawning connection: " ++ show p
            handles@(_, _, _, ph) <-
              createProcess p { std_in = CreatePipe --NoStream
                              , std_out = UseHandle connStdout
                              , std_err = UseHandle connStderr }
            Just pid <- getPid ph
            lvlPrint 2 v $ "Connection spawned with pid: " ++ show pid
            return handles

cleanup_CHERI_BGAS_Sim_Instance :: Verbosity -> CHERI_BGAS_Sim_Instance -> IO ()
cleanup_CHERI_BGAS_Sim_Instance v CHERI_BGAS_Sim_Instance {..} = do
  lvlPrint 2 v $ "Cleaning sim instance"
  cleanupProcess gdbstub_handles
  cleanupProcess devfs_handles
  cleanupProcess sim_handles

cleanup_CHERI_BGAS_Sim_Connection :: Verbosity -> CHERI_BGAS_Sim_Connection
                                  -> IO ()
cleanup_CHERI_BGAS_Sim_Connection v CHERI_BGAS_Sim_Connection {..} = do
  lvlPrint 2 v $ "Cleaning sim connection"
  cleanupProcess tx_rx_handles
  cleanupProcess rx_tx_handles

-- command line arguments
--------------------------------------------------------------------------------
data Options = Options {
    verbosity  :: Int
  , simCmd     :: String
  , devfsCmd   :: String
  , gdbstubCmd :: String
  , connectCmd :: String
  , runFolder  :: String
  , topology   :: (Int, Int)
  } deriving Show

defaultOptions :: Options
defaultOptions = Options {
    verbosity = 0
  , simCmd = "../build/simdir/sim_CHERI_BGAS"
  , devfsCmd = "./cheri-bgas-fuse-devfs/cheri-bgas-fuse-devfs"
  , gdbstubCmd = "./RISCV_gdbstub/src/main"
  , connectCmd = "./forever-splice/forever-splice"
  , runFolder = "./sim-cheri-bgas"
  , topology = (1,1)
  }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['v'] ["verbosity"]
           (OptArg (\x opts -> opts { verbosity =
                                        case x of Just n -> read n
                                                  _ -> opts.verbosity + 1 })
                   "VERBOSITY")
           "Set verbosity level"
  , Option ['s'] ["simulator-bin-path"]
           (ReqArg (\x opts -> opts { simCmd = x }) "SIMULATOR_BIN_PATH")
           "Specify path to the simulator binary"
  , Option ['d'] ["devfs-bin-path"]
           (ReqArg (\x opts -> opts { devfsCmd = x }) "DEVFS_BIN_PATH")
           "Specify path to the devfs binary"
  , Option ['g'] ["gdbstub-bin-path"]
           (ReqArg (\x opts -> opts { gdbstubCmd = x }) "GDBSTUB_BIN_PATH")
           "Specify path to the gdb stub binary"
  , Option ['c'] ["connect-bin-path"]
           (ReqArg (\x opts -> opts { devfsCmd = x }) "CONNECT_BIN_PATH")
           "Specify path to the command to connect instances"
  , Option ['r'] ["run-folder-path"]
           (ReqArg (\x opts -> opts { runFolder = x }) "RUN_FOLDER_PATH")
           "Specify path to the run folder"
  , Option ['t'] ["topology"]
           (ReqArg (\x opts -> opts { topology = read x })
                   "\"(WIDTH, HEIGHT)\"")
           "Specify the topology for simulation"
  ]

commandOpts :: [String] -> IO (Options, [String])
commandOpts argv =
  case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: ./run-cheri-bgas-sim.hs [OPTION...]"

main = do
  -- parse command line arguments
  rawArgs <- getArgs
  (opts, _) <- commandOpts rawArgs

  --
  let v = opts.verbosity
  runFolder <- makeAbsolute opts.runFolder
  simCmd <- makeAbsolute opts.simCmd
  devfsCmd <- makeAbsolute opts.devfsCmd
  gdbstubCmd <- makeAbsolute opts.gdbstubCmd
  connectCmd <- makeAbsolute opts.connectCmd
  let (width, height) = opts.topology
  let simFolder x y = runFolder ++ "/sim_" ++ show x ++ "_" ++ show y
  let coords = [ (x, y) | x <- [0..(width-1)], y <- [0..(height-1)] ]
  let connections =    [ ((x, y-1), (x, y)) | (x, y) <- coords, y > 0]
                    ++ [ ((x-1, y), (x, y)) | (x, y) <- coords, x > 0]

  createDirectoryIfMissing True runFolder
  lvlPrint 1 v $ ">> Creating simulator instances"
  insts <- forM coords \(x, y) ->
    spawn_CHERI_BGAS_Sim_Instance v (x, y) 80000 (simFolder x y)
                                    simCmd devfsCmd gdbstubCmd
  let instsMap = Map.fromList [ ((x, y), inst)
                              | inst@CHERI_BGAS_Sim_Instance{..} <- insts ]
  lvlPrint 1 v $ ">> Simulator instances created"
  lvlPrint 1 v $ ">> Creating simulator connections"
  conns <- forM connections \conn ->
    spawn_CHERI_BGAS_Sim_Connection v connectCmd runFolder instsMap conn
  lvlPrint 1 v $ ">> Simulator connections created"

  tid <- myThreadId
  let cleanup = do
        lvlPrint 1 v $ ">> Destroying simulator connections"
        forM_ conns \conn -> cleanup_CHERI_BGAS_Sim_Connection v conn
        lvlPrint 1 v $ ">> Simulator connections destroyed"
        lvlPrint 1 v $ ">> Destroying simulator instances"
        forM_ insts \inst -> cleanup_CHERI_BGAS_Sim_Instance v inst
        lvlPrint 1 v $ ">> Simulator instances destroyed"
        killThread tid
  lvlPrint 1 v $ ">> Install signal handler"
  installHandler keyboardSignal (Catch cleanup) Nothing
  installHandler sigKILL (Catch cleanup) Nothing
  installHandler sigQUIT (Catch cleanup) Nothing
  installHandler sigTERM (Catch cleanup) Nothing

  -- the line bellow does not seam to behave
  -- awaitSignal Nothing
  -- using this instead:
  let foreverDoomed = foreverDoomed
  foreverDoomed

  return ()
