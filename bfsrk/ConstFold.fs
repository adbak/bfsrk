namespace BF
   
    open AST
    
    /// Module for optimizing AST - constant folding.
    module ConstFold =

        let rec private constFold' (lacc:Stmt list) (iacc:int option) (stmts:Stmt list) =
            let appendAdd (i:int option) (l:Stmt list) =
                match i with
                    | None -> l
                    | Some i' -> Add i'::l

            let incOption (opt:int option) (i:int) =
                match opt with
                    | None -> Some i
                    | Some i' -> Some (i + i')

            match stmts with
                | [] -> appendAdd iacc lacc
                | x::xs ->
                    match x with
                        | Inc -> constFold' lacc (incOption iacc 1) xs
                        | Dec -> constFold' lacc (incOption iacc -1) xs
                        | While l ->
                            let lacc' = appendAdd iacc lacc
                            let whileStmts = constFold l
                            constFold' (While whileStmts::lacc') None xs
                        | _ -> constFold' (x::appendAdd iacc lacc) None xs

        and constFold (stmts:Stmt list) = constFold' [] None stmts |> List.rev