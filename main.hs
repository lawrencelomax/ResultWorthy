#!/usr/bin/env runhaskell

import ResultWorthy.ArgumentParser(Options, opts)
import ResultWorthy.Runner(doRunWithOptions)
import Options.Applicative.Extra(execParser)

main :: IO ()
main = execParser opts >>= doRunWithOptions

