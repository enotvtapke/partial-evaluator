{-# LANGUAGE OverloadedStrings #-}

module Turing.Interpreter (turingInterpreter) where

import Flowchart.AST
import Flowchart.DSL
import Prelude hiding ((==))

turingInterpreter :: Program
turingInterpreter =
  program
    ["q", "right"]
    [ bb "init" ["qtail" @= "q", "left" @= list []] $ jump "loop",
      bb "loop" [] $ jumpc ("qtail" == list []) "stop" "cont",

      bb "cont" [
        "instruction" @= hd "qtail",
        "qtail" @= tl "qtail",
        "operator" @= hd "instruction"
      ] $ jumpc ("operator" == s "right") "do-right" "cont1",
      bb "cont1" [] $ jumpc ("operator" == s "left") "do-left" "cont2",
      bb "cont2" [] $ jumpc ("operator" == s "write") "do-write" "cont3",
      bb "cont3" [] $ jumpc ("operator" == s "goto") "do-goto" "cont4",
      bb "cont4" [] $ jumpc ("operator" == s "if") "do-if" "error",

      bb "do-right" [
        "left" @= cons (hd "right") "left",
        "right" @= tl "right"
      ] $ jump "loop",

      bb "do-left" [
        "right" @= cons (hd "left") "right",
        "left" @= tl "left"
      ] $ jump "loop",

      bb "do-write" [
        "symbol" @= hd (tl "instruction"),
        "right" @= cons "symbol" (tl "right")
      ] $ jump "loop",

      bb "do-goto" [
        "label" @= hd (tl "instruction")
      ] $ jump "jump",

      bb "do-if" [
        "symbol" @= hd (tl "instruction"),
        "label" @= hd (tl $ tl "instruction")
      ] $ jumpc ("symbol" == hd "right") "jump" "loop",

      bb "jump" [
        "qtail" @= suffixFrom "q" "label"
      ] $ jump "loop",

      bb "error" [] $ ret "unknown operator",
      bb "stop" [] $ ret $ var "right"
    ]
