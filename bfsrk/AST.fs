namespace BF

    module AST =

        type Stmt = 
          | Print
          | Read
          | While of Stmt list
          | Inc
          | Dec
          | Left
          | Right
          | Fork
          | Add of int

        type Program = Prog of Stmt list
