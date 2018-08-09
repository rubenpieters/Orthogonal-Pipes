module OrthPipes.Tutorial where

import OrthPipes.Plan

import Data.Void
import Control.Monad (unless)
import System.IO (isEOF)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G

{- | This tutorial file mimics the Pipes Tutorial to highlight similarities and differences -}

{- |
An example Producer stdinLn. It yields received values from stdin until it recieves EOF.
-}

stdinLn :: Plan a' a () String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof $ do
    str <- lift getLine
    yield str
    stdinLn

{- |
The for function replaces yields with a given function.
Note that it is more restricted than Pipes for and it replaces any returns with exit due to its internal use of +>>.
-}

loop :: Plan Void () b' b IO ()
loop = for stdinLn $ \str -> do
    lift $ putStrLn str

exampleStdin :: IO ()
exampleStdin = runEffect loop

triple :: b -> Plan a' a () b m ()
triple x = do
  yield x
  yield x
  yield x

tripleLoop :: Plan a' a () String IO ()
tripleLoop = for stdinLn triple

exampleStdinTriple :: IO ()
exampleStdinTriple = runEffect (for tripleLoop (lift . putStrLn))

{-
An example Consumer stdoutLn
Continually awaits strings from stinLn
-}

stdoutLn :: Plan () String b' b IO ()
stdoutLn = do
    str <- await  -- 'await' a 'String'
    x   <- lift $ try $ putStrLn str
    case x of
        -- Gracefully terminate if we got a broken pipe error
        Left e@(G.IOError { G.ioe_type = t}) ->
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        -- Otherwise loop
        Right () -> stdoutLn

exampleStdOut :: IO ()
exampleStdOut = runEffect (lift getLine >~ stdoutLn)
