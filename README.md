# Self-applicable Flowchart partial evaluator

Flowchart partial evaluator written in Flowchart. Partial evaluator is self applicable and is used to generate compiler generator using third Futamura projection. Compiler generator generates compilers written in Flowchart based on interpreters written in Flowchart. As an example a compiler for Turing machine language is generated using its interpreter. 

Implementation based on the book [Partial Evaluation and Automatic Program Generation](https://studwww.itu.dk/~sestoft/pebook/jonesgomardsestoft-a4.pdf).

There is no parser for Flowchart language, all Flowchart programs should be described in DSL. There is also no pretty printer for Flowchart.

### Locations of key code:

* Flowcahrt AST: [src/Flowchart/AST.hs](src/Flowchart/AST.hs).
* Flowcahrt interpreter: [src/Flowchart/Interpreter/Interpreter.hs](src/Flowchart/Interpreter/Interpreter.hs).
* Flowcahrt partial evaluator: [src/Flowchart/Mix.hs](src/Flowchart/Mix.hs)
* Turing machine interpreter: [src/Turing/Interpreter.hs](src/Turing/Interpreter.hs).

### Usage

* Use `stack test` to run tests.
* Use `stack run` to print third Futamura projection compiler generator for Flowchart programming language to standart output (it is huge).

### Result overview

You can find Turing machine compiler generated using second or third projection (both projections output the same compiler) in [test/Flowchart/TestPrograms.hs](test/Flowchart/TestPrograms.hs). The compiler is the function `generatedTuringMachineCompilerProgram`.

All the projections run quite fast: 
* ~0.17s third projection
* ~0.02s second projection

Generated compiler compiles the following Turing machine program:
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
