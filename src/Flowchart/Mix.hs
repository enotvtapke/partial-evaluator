{-# LANGUAGE OverloadedStrings #-}

module Flowchart.Mix (mix) where

import Flowchart.AST
import Flowchart.DSL
import Prelude hiding (lookup, (==))

mix :: Program
mix =
  program
    ["program", "vs0"]
    [ bb
        "init"
        [ "pending" @= cons (cons (s "init") "vs0") unit,
          "marked" @= unit,
          "residual" @= unit
        ]
        $ jump "blocksLoop",
      -- Basic Blocks loop
      bb
        "blocksLoop"
        []
        $ jumpc ("pending" == unit) "end" "blocksLoopInit",
      bb
        "blocksLoopInit"
        [ "pp" @= car (car "pending"),
          "vs" @= cdr (car "pending"),
          "pending" @= cdr "pending",
          "marked" @= cons (cons "pp" "vs") "marked",
          "bb" @= commands "program" "pp",
          "code" @= cons (cons "pp" "vs") unit -- should be reversed afterwards
        ]
        $ jump "commandsLoop",
      bb
        "blocksLoopEpilogue"
        ["residual" @= cons "code" "residual"]
        $ jump "blocksLoop",
      -- Commands loop
      bb
        "commandsLoop"
        []
        $ jumpc ("bb" == unit) "blocksLoopEpilogue" "commandsLoopInit",
      bb
        "commandsLoopInit"
        [ "command" @= car "bb",
          "bb" @= cdr "bb"
        ]
        $ jumpc (car "command" == s "assign") "assign" "cont1",
      bb "cont1" [] $ jumpc (car "command" == s "goto") "goto" "cont2",
      bb "cont2" [] $ jumpc (car "command" == s "if") "if" "cont3",
      bb "cont3" [] $ jumpc (car "command" == s "return") "return" "error",
      -- Assign
      bb
        "assign"
        [ "varName" @= car (cdr "command"),
          "varExpr" @= car (cdr $ cdr "command")
        ]
        $ jumpc (isStatic "varExpr" "vs") "assignStatic" "assignDyn",
      bb
        "assignDyn"
        ["code" @= cons (list [s "assign", "varName", reduce "varExpr" "vs"]) "code"]
        $ jump "commandsLoop",
      bb
        "assignStatic"
        ["vs" @= insert "vs" "varName" (eval "varExpr" "vs")]
        $ jump "commandsLoop",
      -- Goto
      bb
        "goto"
        [ "pp'" @= car (cdr "command"),
          "bb" @= commands "program" "pp'"
        ]
        $ jump "commandsLoop",
      -- If
      bb
        "if"
        [ "cond" @= car (cdr "command"),
          "ppTrue" @= car (cdr $ cdr "command"),
          "ppFalse" @= car (cdr $ cdr $ cdr "command")
        ]
        $ jumpc (isStatic "cond" "vs") "ifStatic" "ifDyn",
      bb
        "ifStatic"
        []
        $ jumpc (eval "cond" "vs") "ifStaticTrue" "ifStaticFalse",
      bb
        "ifStaticTrue"
        ["bb" @= commands "program" "ppTrue"]
        $ jump "commandsLoop",
      bb
        "ifStaticFalse"
        ["bb" @= commands "program" "ppFalse"]
        $ jump "commandsLoop",
      bb
        "ifDynamic"
        [ "code" @= cons (list [s "if", reduce "cond" "vs", cons "ppTrue" "vs", cons "ppFalse" "vs"]) "code"
        ]
        $ jumpc (member "marked" (cons "ppTrue" "vs")) "addPpFalseToPendingCheck" "addPpTrueToPending",
      bb
        "addPpTrueToPending"
        ["pending" @= cons (cons "ppTrue" "vs") "pending"]
        $ jump "addPpFalseToPendingCheck",
      bb
        "addPpFalseToPendingCheck"
        []
        $ jumpc (member "marked" (cons "ppFalse" "vs")) "commandsLoop" "addPpFalseToPending",
      bb
        "addPpFalseToPending"
        ["pending" @= cons (cons "ppFalse" "vs") "pending"]
        $ jump "commandsLoop",
      -- Return
      bb
        "return"
        [ "exp" @= car (cdr "command"),
          "code" @= cons (list [s "return", reduce "exp" "vs"]) "code"
        ]
        $ jump "commandsLoop",
      -- End labels
      bb "error" [] $ ret (s "error"),
      bb "end" [] $ ret "residual"
    ]
