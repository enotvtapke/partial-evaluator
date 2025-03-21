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
    mixedTuringProgram,
    dynamicLabelsProgram,
    progLiveVarsProgram,
    filterKeysProgram,
    compilerGeneratorProgram,
    generatedTuringMachineCompilerProgram,
  )
where

import Flowchart.AST
import Flowchart.DSL
import Flowchart.DivisionCalculator (programStaticVars)
import Flowchart.Mix (mix)
import TestUtils (runProgram)
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

filterKeysProgram :: Program
filterKeysProgram =
  program
    ["a", "b"]
    [bb "init" [] $ ret $ filterKeys "a" "b"]

progLiveVarsProgram :: Program
progLiveVarsProgram =
  program
    ["p"]
    [bb "init" [] $ ret $ progLiveVars "p"]

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
        "List [StringLiteral \"cont\",List []]"
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
        "init"
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

mixedTuringProgram :: Program
mixedTuringProgram =
  Program
    [VarName "right"]
    [ BasicBlock
        (Label "init")
        [ Assignment (VarName "left") (Constant (List []))
        ]
        (If (Eq (Constant (IntLiteral 0)) (Hd (Var (VarName "right")))) (Label "l0") (Label "l1")),
      BasicBlock
        (Label "l1")
        [ Assignment (VarName "left") (Cons (Hd (Var (VarName "right"))) (Var (VarName "left"))),
          Assignment (VarName "right") (Tl (Var (VarName "right")))
        ]
        (If (Eq (Constant (IntLiteral 0)) (Hd (Var (VarName "right")))) (Label "l0") (Label "l1")),
      BasicBlock
        (Label "l0")
        [ Assignment (VarName "right") (Cons (Constant (IntLiteral 1)) (Tl (Var (VarName "right"))))
        ]
        (Return (Var (VarName "right")))
    ]

dynamicLabelsProgram :: Program
dynamicLabelsProgram =
  program
    ["prog", "staticVars"]
    [bb "init" [] $ ret $ dynamicLabels "prog" "staticVars"]

compilerGeneratorProgram :: Program
compilerGeneratorProgram =
  runProgram
    mix
    [ prog mix,
      programStaticVars mix ["program", "staticVars"],
      list
        [ pair (s "program") (prog mix),
          pair (s "staticVars") (programStaticVars mix ["program", "staticVars"])
        ]
    ]

generatedTuringMachineCompilerProgram :: Program
generatedTuringMachineCompilerProgram =
  Program
    [VarName "vs0"]
    [ BasicBlock (Label "init") [Assignment (VarName "pending") (Cons (Cons (Constant (StringLiteral "init")) (Cons (Var (VarName "vs0")) (Constant (List [])))) (Constant (List []))), Assignment (VarName "marked") (Constant (List [])), Assignment (VarName "residual") (Constant (List []))] (If (Eq (Var (VarName "pending")) (Constant (List []))) (Label "l0") (Label "l1")),
      BasicBlock (Label "l1") [Assignment (VarName "pp") (Hd (Hd (Var (VarName "pending")))), Assignment (VarName "vs") (Hd (Tl (Hd (Var (VarName "pending"))))), Assignment (VarName "pending") (Tl (Var (VarName "pending"))), Assignment (VarName "marked") (Cons (Cons (Var (VarName "pp")) (Cons (Var (VarName "vs")) (Constant (List [])))) (Var (VarName "marked"))), Assignment (VarName "code") (Cons (Cons (Var (VarName "pp")) (Cons (Var (VarName "vs")) (Constant (List [])))) (Constant (List [])))] (If (Eq (Var (VarName "pp")) (Constant (StringLiteral "init"))) (Label "l2") (Label "l3")),
      BasicBlock (Label "l3") [] (If (Eq (Var (VarName "pp")) (Constant (StringLiteral "jump"))) (Label "l4") (Label "l5")),
      BasicBlock (Label "l5") [] (If (Eq (Var (VarName "pp")) (Constant (StringLiteral "loop"))) (Label "l6") (Label "l7")),
      BasicBlock (Label "l7") [] (Return (Constant (StringLiteral "error"))),
      BasicBlock (Label "l6") [] (If (Eval (Constant (Expr (Eq (Var (VarName "qtail")) (Constant (List []))))) (Var (VarName "vs"))) (Label "l8") (Label "l9")),
      BasicBlock (Label "l9") [Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "instruction")) (Eval (Constant (Expr (Hd (Var (VarName "qtail"))))) (Var (VarName "vs")))), Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "qtail")) (Eval (Constant (Expr (Tl (Var (VarName "qtail"))))) (Var (VarName "vs")))), Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "operator")) (Eval (Constant (Expr (Hd (Var (VarName "instruction"))))) (Var (VarName "vs"))))] (If (Eval (Constant (Expr (Eq (Var (VarName "operator")) (Constant (StringLiteral "right"))))) (Var (VarName "vs"))) (Label "l10") (Label "l11")),
      BasicBlock (Label "l11") [] (If (Eval (Constant (Expr (Eq (Var (VarName "operator")) (Constant (StringLiteral "left"))))) (Var (VarName "vs"))) (Label "l12") (Label "l13")),
      BasicBlock (Label "l13") [] (If (Eval (Constant (Expr (Eq (Var (VarName "operator")) (Constant (StringLiteral "write"))))) (Var (VarName "vs"))) (Label "l14") (Label "l15")),
      BasicBlock (Label "l15") [] (If (Eval (Constant (Expr (Eq (Var (VarName "operator")) (Constant (StringLiteral "goto"))))) (Var (VarName "vs"))) (Label "l16") (Label "l17")),
      BasicBlock (Label "l17") [] (If (Eval (Constant (Expr (Eq (Var (VarName "operator")) (Constant (StringLiteral "if"))))) (Var (VarName "vs"))) (Label "l18") (Label "l19")),
      BasicBlock (Label "l19") [Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "return")) (Cons (Reduce (Constant (Expr (Var (VarName "unknown operator")))) (Var (VarName "vs"))) (Constant (List [])))) (Var (VarName "code"))), Assignment (VarName "residual") (Cons (Var (VarName "code")) (Var (VarName "residual")))] (If (Eq (Var (VarName "pending")) (Constant (List []))) (Label "l0") (Label "l1")),
      BasicBlock (Label "l18") [Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "symbol")) (Eval (Constant (Expr (Hd (Tl (Var (VarName "instruction")))))) (Var (VarName "vs")))), Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "label")) (Eval (Constant (Expr (Hd (Tl (Tl (Var (VarName "instruction"))))))) (Var (VarName "vs")))), Assignment (VarName "ppTrueLiveVs") (FilterKeys (Var (VarName "vs")) (Constant (List [StringLiteral "left", StringLiteral "q", StringLiteral "right", StringLiteral "unknown operator", StringLiteral "label"]))), Assignment (VarName "ppFalseLiveVs") (FilterKeys (Var (VarName "vs")) (Constant (List [StringLiteral "left", StringLiteral "q", StringLiteral "qtail", StringLiteral "right", StringLiteral "unknown operator"]))), Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "if")) (Cons (Reduce (Constant (Expr (Eq (Var (VarName "symbol")) (Hd (Var (VarName "right")))))) (Var (VarName "vs"))) (Cons (ToLabel (Cons (Constant (StringLiteral "jump")) (Cons (Var (VarName "ppTrueLiveVs")) (Constant (List []))))) (Cons (ToLabel (Cons (Constant (StringLiteral "loop")) (Cons (Var (VarName "ppFalseLiveVs")) (Constant (List []))))) (Constant (List [])))))) (Var (VarName "code")))] (If (Or (Member (Var (VarName "marked")) (Cons (Constant (StringLiteral "jump")) (Cons (Var (VarName "ppTrueLiveVs")) (Constant (List []))))) (Member (Var (VarName "pending")) (Cons (Constant (StringLiteral "jump")) (Cons (Var (VarName "ppTrueLiveVs")) (Constant (List [])))))) (Label "l20") (Label "l21")),
      BasicBlock (Label "l21") [Assignment (VarName "pending") (Cons (Cons (Constant (StringLiteral "jump")) (Cons (Var (VarName "ppTrueLiveVs")) (Constant (List [])))) (Var (VarName "pending")))] (If (Or (Member (Var (VarName "marked")) (Cons (Constant (StringLiteral "loop")) (Cons (Var (VarName "ppFalseLiveVs")) (Constant (List []))))) (Member (Var (VarName "pending")) (Cons (Constant (StringLiteral "loop")) (Cons (Var (VarName "ppFalseLiveVs")) (Constant (List [])))))) (Label "l22") (Label "l23")),
      BasicBlock (Label "l23") [Assignment (VarName "pending") (Cons (Cons (Constant (StringLiteral "loop")) (Cons (Var (VarName "ppFalseLiveVs")) (Constant (List [])))) (Var (VarName "pending"))), Assignment (VarName "residual") (Cons (Var (VarName "code")) (Var (VarName "residual")))] (If (Eq (Var (VarName "pending")) (Constant (List []))) (Label "l0") (Label "l1")),
      BasicBlock (Label "l22") [Assignment (VarName "residual") (Cons (Var (VarName "code")) (Var (VarName "residual")))] (If (Eq (Var (VarName "pending")) (Constant (List []))) (Label "l0") (Label "l1")),
      BasicBlock (Label "l20") [] (If (Or (Member (Var (VarName "marked")) (Cons (Constant (StringLiteral "loop")) (Cons (Var (VarName "ppFalseLiveVs")) (Constant (List []))))) (Member (Var (VarName "pending")) (Cons (Constant (StringLiteral "loop")) (Cons (Var (VarName "ppFalseLiveVs")) (Constant (List [])))))) (Label "l22") (Label "l23")),
      BasicBlock (Label "l16") [Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "label")) (Eval (Constant (Expr (Hd (Tl (Var (VarName "instruction")))))) (Var (VarName "vs")))), Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "qtail")) (Eval (Constant (Expr (SuffixFrom (Var (VarName "q")) (Var (VarName "label"))))) (Var (VarName "vs"))))] (If (Eval (Constant (Expr (Eq (Var (VarName "qtail")) (Constant (List []))))) (Var (VarName "vs"))) (Label "l8") (Label "l9")),
      BasicBlock (Label "l14") [Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "symbol")) (Eval (Constant (Expr (Hd (Tl (Var (VarName "instruction")))))) (Var (VarName "vs")))), Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "assign")) (Cons (Constant (StringLiteral "right")) (Cons (Reduce (Constant (Expr (Cons (Var (VarName "symbol")) (Tl (Var (VarName "right")))))) (Var (VarName "vs"))) (Constant (List []))))) (Var (VarName "code")))] (If (Eval (Constant (Expr (Eq (Var (VarName "qtail")) (Constant (List []))))) (Var (VarName "vs"))) (Label "l8") (Label "l9")),
      BasicBlock (Label "l12") [Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "assign")) (Cons (Constant (StringLiteral "right")) (Cons (Reduce (Constant (Expr (Cons (Hd (Var (VarName "left"))) (Var (VarName "right"))))) (Var (VarName "vs"))) (Constant (List []))))) (Var (VarName "code"))), Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "assign")) (Cons (Constant (StringLiteral "left")) (Cons (Reduce (Constant (Expr (Tl (Var (VarName "left"))))) (Var (VarName "vs"))) (Constant (List []))))) (Var (VarName "code")))] (If (Eval (Constant (Expr (Eq (Var (VarName "qtail")) (Constant (List []))))) (Var (VarName "vs"))) (Label "l8") (Label "l9")),
      BasicBlock (Label "l10") [Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "assign")) (Cons (Constant (StringLiteral "left")) (Cons (Reduce (Constant (Expr (Cons (Hd (Var (VarName "right"))) (Var (VarName "left"))))) (Var (VarName "vs"))) (Constant (List []))))) (Var (VarName "code"))), Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "assign")) (Cons (Constant (StringLiteral "right")) (Cons (Reduce (Constant (Expr (Tl (Var (VarName "right"))))) (Var (VarName "vs"))) (Constant (List []))))) (Var (VarName "code")))] (If (Eval (Constant (Expr (Eq (Var (VarName "qtail")) (Constant (List []))))) (Var (VarName "vs"))) (Label "l8") (Label "l9")),
      BasicBlock (Label "l8") [Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "return")) (Cons (Reduce (Constant (Expr (Var (VarName "right")))) (Var (VarName "vs"))) (Constant (List [])))) (Var (VarName "code"))), Assignment (VarName "residual") (Cons (Var (VarName "code")) (Var (VarName "residual")))] (If (Eq (Var (VarName "pending")) (Constant (List []))) (Label "l0") (Label "l1")),
      BasicBlock (Label "l4") [Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "qtail")) (Eval (Constant (Expr (SuffixFrom (Var (VarName "q")) (Var (VarName "label"))))) (Var (VarName "vs"))))] (If (Eval (Constant (Expr (Eq (Var (VarName "qtail")) (Constant (List []))))) (Var (VarName "vs"))) (Label "l8") (Label "l9")),
      BasicBlock (Label "l2") [Assignment (VarName "vs") (Insert (Var (VarName "vs")) (Constant (StringLiteral "qtail")) (Eval (Constant (Expr (Var (VarName "q")))) (Var (VarName "vs")))), Assignment (VarName "code") (Cons (Cons (Constant (StringLiteral "assign")) (Cons (Constant (StringLiteral "left")) (Cons (Reduce (Constant (Expr (Constant (List [])))) (Var (VarName "vs"))) (Constant (List []))))) (Var (VarName "code")))] (If (Eval (Constant (Expr (Eq (Var (VarName "qtail")) (Constant (List []))))) (Var (VarName "vs"))) (Label "l8") (Label "l9")),
      BasicBlock (Label "l0") [] (Return (CompressLabels (ToLabel (Cons (Constant (StringLiteral "init")) (Cons (Var (VarName "vs0")) (Constant (List []))))) (DescrToProg (Constant (Prog (Program [VarName "q", VarName "right"] [BasicBlock (Label "init") [Assignment (VarName "qtail") (Var (VarName "q")), Assignment (VarName "left") (Constant (List []))] (Goto (Label "loop")), BasicBlock (Label "loop") [] (If (Eq (Var (VarName "qtail")) (Constant (List []))) (Label "stop") (Label "cont")), BasicBlock (Label "cont") [Assignment (VarName "instruction") (Hd (Var (VarName "qtail"))), Assignment (VarName "qtail") (Tl (Var (VarName "qtail"))), Assignment (VarName "operator") (Hd (Var (VarName "instruction")))] (If (Eq (Var (VarName "operator")) (Constant (StringLiteral "right"))) (Label "do-right") (Label "cont1")), BasicBlock (Label "cont1") [] (If (Eq (Var (VarName "operator")) (Constant (StringLiteral "left"))) (Label "do-left") (Label "cont2")), BasicBlock (Label "cont2") [] (If (Eq (Var (VarName "operator")) (Constant (StringLiteral "write"))) (Label "do-write") (Label "cont3")), BasicBlock (Label "cont3") [] (If (Eq (Var (VarName "operator")) (Constant (StringLiteral "goto"))) (Label "do-goto") (Label "cont4")), BasicBlock (Label "cont4") [] (If (Eq (Var (VarName "operator")) (Constant (StringLiteral "if"))) (Label "do-if") (Label "error")), BasicBlock (Label "do-right") [Assignment (VarName "left") (Cons (Hd (Var (VarName "right"))) (Var (VarName "left"))), Assignment (VarName "right") (Tl (Var (VarName "right")))] (Goto (Label "loop")), BasicBlock (Label "do-left") [Assignment (VarName "right") (Cons (Hd (Var (VarName "left"))) (Var (VarName "right"))), Assignment (VarName "left") (Tl (Var (VarName "left")))] (Goto (Label "loop")), BasicBlock (Label "do-write") [Assignment (VarName "symbol") (Hd (Tl (Var (VarName "instruction")))), Assignment (VarName "right") (Cons (Var (VarName "symbol")) (Tl (Var (VarName "right"))))] (Goto (Label "loop")), BasicBlock (Label "do-goto") [Assignment (VarName "label") (Hd (Tl (Var (VarName "instruction"))))] (Goto (Label "jump")), BasicBlock (Label "do-if") [Assignment (VarName "symbol") (Hd (Tl (Var (VarName "instruction")))), Assignment (VarName "label") (Hd (Tl (Tl (Var (VarName "instruction")))))] (If (Eq (Var (VarName "symbol")) (Hd (Var (VarName "right")))) (Label "jump") (Label "loop")), BasicBlock (Label "jump") [Assignment (VarName "qtail") (SuffixFrom (Var (VarName "q")) (Var (VarName "label")))] (Goto (Label "loop")), BasicBlock (Label "error") [] (Return (Var (VarName "unknown operator"))), BasicBlock (Label "stop") [] (Return (Var (VarName "right")))]))) (Var (VarName "vs0")) (Var (VarName "residual")))))
    ]
