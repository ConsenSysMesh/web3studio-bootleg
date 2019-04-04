module CryptolScript where


import System.Environment
import System.Process
import System.IO as IO

stringToBool x = case x of "True" -> Left True
                           "False" -> Left False
                           _      -> Right "Result was not a boolean"

parseCryptolBoolOutput :: String -> Either Bool String
parseCryptolBoolOutput output = stringToBool $ last $ words $ last $ init $ lines output

runCryptol hout file prop = do
         (hin,_, herr , ph) <- createProcess_ "" (proc "cryptol"  [file])
                     { std_out = UseHandle hout
                     , std_in  = CreatePipe
                     , std_err = CreatePipe
                     }
         let undoMaybe (Just x) = x
         hPutStrLn (undoMaybe hin) prop
         exitCode <- waitForProcess ph
         return ()

checkCryptolOutput file prop = do        
         withFile resultOut WriteMode $ \ hout ->
          do hSetBuffering hout NoBuffering
             runCryptol hout file prop
         out <- readFile resultOut
         pure $ parseCryptolBoolOutput $ out
         where
         resultOut = "resultOut.stdout"
