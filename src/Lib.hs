module Lib
  ( CoreProgram,
    Program,
    Name,
    Expr (..),
    parse,
    parseExpr,
    parseProg,
  )
where

import Corelang.Parser.Parse
import Corelang.Parser.ParseProg