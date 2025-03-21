{-# LANGUAGE OverloadedStrings #-}

module Flowchart.Mix (mix) where

import Flowchart.AST
import Flowchart.DSL
import Prelude hiding (or, lookup, (==))

mix :: Program
mix =
  program
    ["program", "staticVars", "vs0"]
    [ bb
        "init"
        [ "pending" @= list [pair (s "init") "vs0"],
          "marked" @= list [],
          "residual" @= list [],
          "liveVars" @= progLiveVars "program"
        ]
        $ jump "blocksLoop",
      -- Basic Blocks loop
      bb
        "blocksLoop"
        []
        $ jumpc ("pending" == list []) "end" "blocksLoopInit",
      bb
        "blocksLoopInit"
        [ "pp" @= hd (hd "pending"),
          "vs" @= hd (tl (hd "pending")),
          "pending" @= tl "pending",
          "marked" @= cons (pair "pp" "vs") "marked",
          "pps" @= dynamicLabels "program" "staticVars",
          "code" @= list [pair "pp" "vs"]
        ]
        $ jump "searchBb",
      bb
        "blocksLoopEpilogue"
        ["residual" @= cons "code" "residual"]
        $ jump "blocksLoop",
      -- Commands loop
      bb
        "commandsLoop"
        []
        $ jumpc ("bb" == list []) "blocksLoopEpilogue" "commandsLoopInit",
      bb
        "commandsLoopInit"
        [ "command" @= hd "bb",
          "bb" @= tl "bb"
        ]
        $ jumpc (hd "command" == s "assign") "assign" "cont1",
      bb "cont1" [] $ jumpc (hd "command" == s "goto") "goto" "cont2",
      bb "cont2" [] $ jumpc (hd "command" == s "if") "if" "cont3",
      bb "cont3" [] $ jumpc (hd "command" == s "return") "return" "error",
      -- Assign
      bb
        "assign"
        [ "varName" @= hd (tl "command"),
          "varExpr" @= hd (tl $ tl "command")
        ]
        $ jumpc (member "staticVars" "varName") "assignStatic" "assignDyn",
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
        [ "pp'" @= hd (tl "command"),
          "bb" @= commands "program" "pp'"
        ]
        $ jump "commandsLoop",
      -- If
      bb
        "if"
        [ "cond" @= hd (tl "command"),
          "ppTrue" @= hd (tl $ tl "command"),
          "ppFalse" @= hd (tl $ tl $ tl "command")
        ]
        $ jumpc (isStatic "cond" "staticVars") "ifStatic" "ifDynamic",
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
        [ "ppTrueLiveVs" @= filterKeys "vs" (lookup "liveVars" "ppTrue"),
          "ppFalseLiveVs" @= filterKeys "vs" (lookup "liveVars" "ppFalse"),
          "code" @= cons (list [s "if", reduce "cond" "vs", toLabel $ pair "ppTrue" "ppTrueLiveVs", toLabel $ pair "ppFalse" "ppFalseLiveVs"]) "code"
        ]
        $ jumpc (member "marked" (pair "ppTrue" "ppTrueLiveVs") `or` member "pending" (pair "ppTrue" "ppTrueLiveVs")) "addPpFalseToPendingCheck" "addPpTrueToPending",
      bb
        "addPpTrueToPending"
        ["pending" @= cons (pair "ppTrue" "ppTrueLiveVs") "pending"]
        $ jump "addPpFalseToPendingCheck",
      bb
        "addPpFalseToPendingCheck"
        []
        $ jumpc (member "marked" (pair "ppFalse" "ppFalseLiveVs") `or` member "pending" (pair "ppFalse" "ppFalseLiveVs")) "commandsLoop" "addPpFalseToPending",
      bb
        "addPpFalseToPending"
        ["pending" @= cons (pair "ppFalse" "ppFalseLiveVs") "pending"]
        $ jump "commandsLoop",
      -- Return
      bb
        "return"
        [ "exp" @= hd (tl "command"),
          "code" @= cons (list [s "return", reduce "exp" "vs"]) "code"
        ]
        $ jump "commandsLoop",
      -- End labels
      bb "error" [] $ ret (s "error"),
      bb "end" [] $ ret $ compressLabels (toLabel $ pair (s "init") "vs0") $ descrToProg "program" "vs0" "residual",
      -- Lookup
      bb "searchBb" [] $ jumpc ("pp" == hd "pps") "found" "cont",
      bb
        "cont"
        [ "pps" @= tl "pps"
        ]
        $ jumpc ("pps" == list []) "error" "searchBb",
      bb "found" [
        "bb" @= commands "program" (hd "pps")
      ] $ jump "commandsLoop"
    ]
