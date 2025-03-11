{-# LANGUAGE OverloadedStrings #-}

module TestPrograms
  ( returnTwo,
    loop,
    swapPair,
    returnList,
    returnStr,
    indexOf,
    caseProgram,
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
        $ ret "str"
    ]

caseProgram :: Program
caseProgram =
  program
    ["operator"]
    [ bb "init" [] $ jumpc (var "operator" == "right") "ret" "cont1",
      bb "cont1" [] $ jumpc (var "operator" == "left") "ret" "cont2",
      bb "cont2" [] $ jumpc (var "operator" == "write") "ret" "cont3",
      bb "cont3" [] $ jumpc (var "operator" == "goto") "ret" "cont4",
      bb "cont4" [] $ jumpc (var "operator" == "if") "ret" "error",
      bb "error" [] $ ret "error",
      bb "ret" [] $ ret $ var "operator"
    ]
