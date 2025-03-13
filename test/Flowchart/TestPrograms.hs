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
    insertProgram,
    lookupProgram,
    memberProgram,
  )
where

import Flowchart.AST
import Flowchart.DSL
import Prelude hiding (lookup, (+), (==))

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
        $ ret
        $ s "str"
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
  program
    []
    [bb "init" ["x" @= list [s "a", s "b", s "c", s "d"]] $ ret $ suffixFrom "x" (int 1)]

insertProgram :: Program
insertProgram =
  program
    ["k"]
    [ bb
        "init"
        [ "m" @= list [cons (s "b") (int 3), cons (s "a") (int 11)],
          "m" @= insert "m" (cons (s "a") (int 10)) unit,
          "m" @= insert "m" "k" (int 2)
        ]
        $ ret "m"
    ]

lookupProgram :: Program
lookupProgram =
  program
    ["k"]
    [ bb
        "init"
        [ "m" @= list [cons (s "b") (int 3), cons (s "a") (int 11), cons (cons (s "c") (int 10)) (s "cv")]
        ]
        $ ret (lookup "m" "k")
    ]

memberProgram :: Program
memberProgram =
  program
    ["e"]
    [bb "init" ["x" @= list [s "a", s "b", s "c", s "d"]] $ ret $ member "x" "e"]
