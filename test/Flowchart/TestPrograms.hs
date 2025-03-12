{-# LANGUAGE OverloadedStrings #-}

module Flowchart.TestPrograms
  ( returnTwo,
    loop,
    swapPair,
    returnList,
    returnStr,
    indexOf,
    caseProgram,
    suffixFromProgram,
  )
where

import Flowchart.AST
import Flowchart.DSL
import Prelude hiding ((+), (==))

returnTwo :: Program
returnTwo =
  program
    []
    [ bb
        "ret2"
        []
        $ ret (int 2)
    ]

loop :: Program
loop =
  program
    ["x"]
    [ bb
        "loop"
        ["x" @= var "x" + int 1]
        $ jumpc (var "x" == int 10) "ret" "loop",
      bb "ret" [] $ ret $ var "x"
    ]

swapPair :: Program
swapPair =
  program
    []
    [ bb
        "swap"
        ["x" @= cons (int 1) (int 2)]
        $ ret
        $ cons (cdr $ var "x") (car $ var "x")
    ]

returnList :: Program
returnList =
  program
    []
    [ bb
        "retList"
        ["x" @= cons (int 1) (cons (int 2) unit)]
        $ ret
        $ var "x"
    ]

indexOf :: Program
indexOf =
  program
    ["list", "el"]
    [ bb "init" ["x" @= int 0] $ jumpc (car (var "list") == var "el") "ret" "loop",
      bb "loop" ["x" @= var "x" + int 1, "list" @= cdr (var "list")] $ jumpc (car (var "list") == var "el") "ret" "loop",
      bb "ret" [] $ ret $ var "x"
    ]

returnStr :: Program
returnStr =
  program
    []
    [ bb
        "retStr"
        []
        $ ret $ s "str"
    ]

caseProgram :: Program
caseProgram =
  program
    ["operator"]
    [ bb "init" [] $ jumpc ("operator" == s "right") "ret" "cont1",
      bb "cont1" [] $ jumpc ("operator" == s "left") "ret" "cont2",
      bb "cont2" [] $ jumpc ("operator" == s "write") "ret" "cont3",
      bb "cont3" [] $ jumpc ("operator" == s "goto") "ret" "cont4",
      bb "cont4" [] $ jumpc ("operator" == s "if") "ret" "error",
      bb "error" [] $ ret $ s "error",
      bb "ret" [] $ ret $ var "operator"
    ]

suffixFromProgram :: Program
suffixFromProgram =
  program []
    [ bb "init" ["x" @= list [s "a", s "b", s "c", s "d"]] $ ret $ suffixFrom "x" (int 1)]