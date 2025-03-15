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
    reduceProgram,
    evalProgram,
    commandsProgram,
    descrProgram,
    descrToProgProgram,
    searchContDescr,
    mixedSearchProgram,
    searchProgram,
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
        $ cons (tl $ var "x") (hd $ var "x")
    ]

returnList :: Program
returnList =
  program
    []
    [ bb
        "retList"
        ["x" @= cons (int 1) (cons (int 2) (list []))]
        $ ret
        $ var "x"
    ]

indexOf :: Program
indexOf =
  program
    ["list", "el"]
    [ bb "init" ["x" @= int 0] $ jumpc (hd (var "list") == var "el") "ret" "loop",
      bb "loop" ["x" @= var "x" + int 1, "list" @= tl (var "list")] $ jumpc (hd (var "list") == var "el") "ret" "loop",
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
        [ "m" @= list [pair (s "b") (int 3), pair (s "a") (int 11)],
          "m" @= insert "m" (pair (s "a") (int 10)) unit,
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
        [ "m" @= list [pair (s "b") (int 3), pair (s "a") (int 11), pair (pair (s "c") (int 10)) (s "cv")]
        ]
        $ ret (lookup "m" "k")
    ]

memberProgram :: Program
memberProgram =
  program
    ["e"]
    [bb "init" ["x" @= list [s "a", s "b", s "c", s "d"]] $ ret $ member "x" "e"]

reduceProgram :: Program
reduceProgram =
  program
    ["vars"]
    [bb "init" ["z" @= expr (hd (pair ("x" + int 3) (s "s")) + "y")] $ ret $ reduce "z" "vars"]

evalProgram :: Program
evalProgram =
  program
    ["vars"]
    [bb "init" ["z" @= expr (hd (pair ("x" + int 3) (s "s")) + "y")] $ ret $ eval "z" "vars"]

commandsProgram :: Program
commandsProgram =
  program
    ["prog", "l"]
    [bb "init" [] $ ret $ commands "prog" "l"]

descrProgram :: Program
descrProgram =
  program
    ["namelist", "valuelist"]
    [ bb
        "[\"cont\",[]]"
        [ "namelist" @= tl "namelist",
          "valuelist" @= tl "valuelist"
        ]
        $ jump "search"
    ]

descrToProgProgram :: Program
descrToProgProgram =
  program
    ["program", "staticVars"]
    [ bb "init" [] $ ret (descrToProg "program" "staticVars" searchContDescr)
    ]

searchContDescr :: Expr
searchContDescr =
  list
    [ list
        [ list [s "goto", s "search"],
          list [s "assign", s "valuelist", expr (tl "valuelist")],
          list [s "assign", s "namelist", expr (tl "namelist")],
          pair (s "cont") (list [])
        ]
    ]

mixedSearchProgram :: Program
mixedSearchProgram =
  program
    ["valuelist"]
    [ bb
        "[\"init\",[[\"name\",\"c\"],[\"namelist\",[\"b\",\"a\",\"c\"]]]]"
        [ "valuelist" @= tl "valuelist",
          "valuelist" @= tl "valuelist"
        ]
        $ ret
        $ hd "valuelist"
    ]

searchProgram :: Program
searchProgram =
  program
    ["name", "namelist", "valuelist"]
    [ bb "init" [] $ jump "search",
      bb "search" [] $ jumpc ("name" == hd "namelist") "found" "cont",
      bb
        "cont"
        [ "valuelist" @= tl "valuelist",
          "namelist" @= tl "namelist"
        ]
        $ jump "search",
      bb "found" [] $ ret (hd "valuelist")
    ]
