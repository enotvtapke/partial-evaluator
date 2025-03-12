{-# LANGUAGE OverloadedStrings #-}

module Turing.Interpreter (turingInterpreter) where

import Flowchart.AST
import Flowchart.DSL
import Prelude hiding ((==))

turingInterpreter :: Program
turingInterpreter =
  program
    ["q", "right"]
    [ bb "init" ["qtail" @= "q", "left" @= unit] $ jump "loop",
      bb "loop" [] $ jumpc ("qtail" == unit) "stop" "cont",

      bb "cont" [
        "instruction" @= car "qtail",
        "qtail" @= cdr "qtail",
        "operator" @= car "instruction"
      ] $ jumpc ("operator" == s "right") "do-right" "cont1",
      bb "cont1" [] $ jumpc ("operator" == s "left") "do-left" "cont2",
      bb "cont2" [] $ jumpc ("operator" == s "write") "do-write" "cont3",
      bb "cont3" [] $ jumpc ("operator" == s "goto") "do-goto" "cont4",
      bb "cont4" [] $ jumpc ("operator" == s "if") "do-if" "error",

      bb "do-right" [
        "left" @= cons (car "right") "left",
        "right" @= cdr "right"
      ] $ jump "loop",

      bb "do-left" [
        "right" @= cons (car "left") "right",
        "left" @= cdr "left"
      ] $ jump "loop",

      bb "do-write" [
        "symbol" @= car (cdr "instruction"),
        "right" @= cons "symbol" (cdr "right")
      ] $ jump "loop",

      bb "do-goto" [
        "label" @= car (cdr "instruction")
      ] $ jump "jump",

      bb "do-if" [
        "symbol" @= car (cdr "instruction"),
        "label" @= car (cdr $ cdr "instruction")
      ] $ jumpc ("symbol" == car "right") "jump" "loop",

      bb "jump" [
        "qtail" @= suffixFrom "q" "label"
      ] $ jump "loop",

      bb "error" [] $ ret "unknown operator",
      bb "stop" [] $ ret $ var "right"
    ]
