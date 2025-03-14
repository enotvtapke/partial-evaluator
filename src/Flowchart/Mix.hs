{-# LANGUAGE OverloadedStrings #-}

module Flowchart.Mix (mix) where

import Flowchart.AST
import Flowchart.DSL hiding ((==))

mix :: Program
mix =
  program
    ["program", "division", "vs0"]
    [ bb
        "init"
        [ "pending" @= cons (cons (s "init") "vs0") unit,
          "marked" @= unit
        ]
        $ jump "loop",
      bb
        "loop"
        [ "pp" @= car (car "pending"),
          "vs" @= cdr (car "pending"),
          "pending" @= cdr "pending",
          "marked" @= cons (cons "pp" "vs") "marked",
          "bb" @= undefined, -- lookup "pp" "program"
          "code" @= undefined -- initialCode "pp" "vs"
        ]
        undefined
    ]
