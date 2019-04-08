module CryptolScript where


import System.Environment
import System.Process
import System.IO as IO
import Data.Maybe

stringToBool x = case x of "True" -> Left True
                           "False" -> Left False
                           _      -> Right "Result was not a boolean"

parseCryptolBoolOutput :: String -> Either Bool String
parseCryptolBoolOutput output = stringToBool $ last $ words $ last $ init $ lines output


addToPath dir = do
  currentPath <- getEnv "PATH"
  setEnv "PATH" (concat [currentPath, ":", dir])
  createProcess (proc "printenv" []) 
  return ()

copyLibs = do
         (_,_, _, ph) <- createProcess (proc "cp"  ["cryptol/dependencies/libgomp.so.1","/lib64/libgomp.so.1"])         
                     { std_out = Inherit
                     , std_in  = Inherit
                     , std_err = CreatePipe
                     }
         exitCode <- waitForProcess ph
         return ()

runCryptol hout file prop = do
         addToPath "cryptol/z3/bin"
         addToPath "cryptol/cryptol/bin"
         copyLibs
         setEnv "HOME" "/root"
         (Just hin,_, herr , ph) <- createProcess (proc "cryptol"  [file])         
                     { std_out = UseHandle hout
                     , std_in  = CreatePipe
                     , std_err = CreatePipe
                     }
         hPutStrLn hin prop
         exitCode <- waitForProcess ph
         return ()

checkCryptolOutput file prop = do        
         withFile resultOut WriteMode $ \ hout ->
          do hSetBuffering hout NoBuffering
             runCryptol hout file prop
         out <- readFile resultOut
         pure $ parseCryptolBoolOutput $ out
         where
         resultOut = "/tmp/resultOut.stdout"
