module Control.Logging.Simple

import System.File

public export LogLevel : Type
LogLevel = Bits8

public export
FATAL, ERROR, WARN, INFO, DEBUG, TRACE, ALL : LogLevel
FATAL = 0
ERROR = 1
WARN = 2
INFO = 3
DEBUG = 4
TRACE = 5
ALL = 255


-- --------------------------------------------------------------------------

export
record Logging (facility : String) (m : Type -> Type) (msgTy : Type) where
  constructor MkLogging
  receiver : LogLevel -> String -> msgTy -> m ()
  level : LogLevel



export log : Monad m => (facility : String) -> {0 msgTy : Type} ->
             {auto c : Logging facility m msgTy} ->
             (lvl : LogLevel) -> (msg : m msgTy) -> m ()
log {c=c} facility lvl msg = when (lvl < c.level) $ msg >>= c.receiver lvl facility



export logFatal, logError, logWarn, logInfo, logDebug, logTrace
    : Monad m => (facility : String) -> {0 msgTy : Type} ->
      {auto c : Logging facility m msgTy} ->
      (msg : m msgTy) -> m ()
logFatal facility msg = log facility FATAL msg
logError facility msg = log facility ERROR msg
logWarn facility msg = log facility WARN msg
logInfo facility msg = log facility INFO msg
logDebug facility msg = log facility DEBUG msg
logTrace facility msg = log facility TRACE msg


public export splitLogFacility : (0 facility : String) -> (0 facility' : String) ->
                         {auto c : Logging facility m msgTy} ->
                         Logging facility' m msgTy
splitLogFacility {c=c} _ _ = believe_me c



-- --------------------------------------------------------------------------

export
loggingStderr : HasIO io => (facility : String) -> LogLevel -> Logging facility io String
loggingStderr facility lvl = MkLogging {
  receiver = \lvl, facility', msg => ignore $ fPutStrLn stderr $ "[\{facility'}] \{msg}",
  level = lvl
  }

