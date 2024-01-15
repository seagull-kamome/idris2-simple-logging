module Main

import Control.Logging.Simple


test1 : {auto _ : Logging "main" IO String} ->
        {auto _ : Logging "test1" IO String} -> IO ()
test1 = do
    logFatal "main" $ pure "main-fatal"
    logInfo "main" $ pure "main-info"
    --
    logError "test1" $ pure "test1-error"
    logWarn "test1" $ pure "test1-warn"
    logInfo "test1" $ pure "test1-info"
    pure ()



main : IO ()
main = do
  let _ = loggingStderr "main" WARN
  let _ = splitLogFacility "main" "test1"
  test1

