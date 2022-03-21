import qualified Language.Nano.Types  as Nano
import qualified Language.Nano.Eval   as Nano
import           Language.Nano.Repl
import           Text.Printf
import           GHC.IO.Encoding

main :: IO ()                             
main = do
  setLocaleEncoding utf8
  putStrLn(welcome) 
  helper 0 Nano.prelude

helper :: Int -> Nano.Env -> IO()
helper x env = do
    putStrFlush ( printf "\x03BB [%d] " x)
    c <- getLine
    case strCmd c of
      CQuit -> do 
              doQuit

      CEval input -> do 
                    doEval env input
                    helper (x + 1) env

      CRun input -> do 
                  doRun input
                  helper (x + 1) env

      CLoad input -> do
                    env <- doLoad input
                    putStrLn("definitions:" ++ (help env)) 
                    helper (x + 1) env
                      where 
                        help :: Nano.Env -> String
                        help [] = "" 
                        help ((x1, x2) : r) = " " ++ x1 ++ (help r)
                                   
      CUnknown -> do 
                doUnknown
                helper (x + 1) env

--------------------------------------------------------------------------------
-- | Some useful functions 
--------------------------------------------------------------------------------
-- putStr   :: String -> IO ()
-- hFlush   :: 
-- putStrLn :: String -> IO ()

-- getLine  :: IO String 

