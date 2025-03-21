# Self-applicable Flowchart partial evaluator

Flowchart partial evaluator written in Flowchart. Partial evaluator is self applicable and is used to generate compiler generator using third Futamura projection. Compiler generator generates compilers written in Flowchart based on interpreters written in Flowchart. As an example a compiler for Turing machine language is generated using its interpreter. 

### Locations of key code:

* Flowcahrt AST: [src/Flowchart/AST.hs](src/Flowchart/AST.hs).
* Flowcahrt interpreter: [src/Flowchart/Interpreter/Interpreter.hs](src/Flowchart/Interpreter/Interpreter.hs).
* Flowcahrt partial evaluator: [src/Flowchart/Mix.hs](src/Flowchart/Mix.hs)
* Turing machine interpreter: [src/Turing/Interpreter.hs](src/Turing/Interpreter.hs).

### Usage

Use `stack test` to run tests. `third proj` fails because it is ignored. You can make it not ignored by changing `xit` to `it`. In this case you can see compiler generator that was generated using third projection (but it is huge).

### Result overview

You can find Turing machine compiler generated using second or third projection in [test/Flowchart/TestPrograms.hs](test/Flowchart/TestPrograms.hs). The compiler is the function `generatedTuringMachineCompilerProgram`.

Compiler generator can be used to generate compiler that compiles the following Turing machine program:
```Haskell
replaceFirstOne :: Expr
replaceFirstOne = list [
    list [s "if", int 0, int 3],
    list [s "right"],
    list [s "goto", int 0],
    list [s "write", int 1]
  ]
```

Into:
```Haskell
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
```
