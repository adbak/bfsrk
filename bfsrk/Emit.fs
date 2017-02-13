namespace BF
    
    open AST
    open ConstFold
    open Base
    open System.IO
    open System.Reflection
    open System.Reflection.Emit

    module Emit =

        type TFilePath = string
        type TOpt = bool
        type TTapeSize = int

        type CompilationSettings = {
            input: TFilePath;
            output: TFilePath;
            opt: TOpt;
            ts: TTapeSize;
        }

        type CGenState = {
            ilGen: ILGenerator;
            runMeth: MethodInfo;
            leftMeth: MethodInfo;
            rightMeth: MethodInfo;
            argTy: System.Type;
            forkMap: Label[];
            currFork: int;
        }

        let defaultTapeSize = 1 <<< 16
        let defaultOutputFile = "out"

        let getDirectoryName(s:string) = 
            let d = Path.GetDirectoryName(s)
            if System.String.IsNullOrEmpty(d) then
                Directory.GetCurrentDirectory()
            else
                d

        let optimize (flag:TOpt) (prog:Program) =
            let optimize' (prog:Program) =
                match prog with
                    | Prog stmts -> Prog (constFold stmts)
            
            if flag then
                optimize' prog
            else
                prog

        let countForks (prog:Program) =
            let rec f n (stmt:Stmt) =
                match stmt with
                    | Fork -> n + 1
                    | While stmts -> n + countForks' stmts
                    | _ -> n
            and countForks' = List.fold f 0

            match prog with
                | Prog stmts -> countForks' stmts


        /// Emits program using given codegen state.
        let emitProg (st:CGenState) (prog:Program) =
            let mutable genSt = st
            
            let tapeLoc = genSt.ilGen.DeclareLocal(typeof<char[]>)
            let tapeLenLoc = genSt.ilGen.DeclareLocal(typeof<int>)
            let ptrLoc = genSt.ilGen.DeclareLocal(typeof<int>)
            let tapeCpyLoc = genSt.ilGen.DeclareLocal(typeof<char[]>)

            let labelFld = genSt.argTy.GetField("label")
            let tapeFld = genSt.argTy.GetField("tape")
            let ptrFld = genSt.argTy.GetField("ptr")

            let setForkLabel () =
                let curr = genSt.currFork
                genSt.ilGen.MarkLabel(genSt.forkMap.[curr])
                genSt <- { genSt with currFork = curr + 1 }

            let rec emitStmt (stmt:Stmt) =
                
                let pushTapeAndPtr () =
                    genSt.ilGen.Emit(OpCodes.Ldloc, tapeLoc)
                    genSt.ilGen.Emit(OpCodes.Ldloc, ptrLoc)

                let pushElem () = 
                    pushTapeAndPtr ()
                    genSt.ilGen.Emit(OpCodes.Ldelem_I1)

                match stmt with
                    | Inc ->
                        pushTapeAndPtr ()
                        pushElem ()
                        genSt.ilGen.Emit(OpCodes.Ldc_I4_1)
                        genSt.ilGen.Emit(OpCodes.Add)
                        genSt.ilGen.Emit(OpCodes.Stelem_I1)

                    | Dec ->
                        pushTapeAndPtr ()
                        pushElem ()
                        genSt.ilGen.Emit(OpCodes.Ldc_I4_1)
                        genSt.ilGen.Emit(OpCodes.Sub)
                        genSt.ilGen.Emit(OpCodes.Stelem_I1)

                    | Add i ->
                        pushTapeAndPtr ()
                        pushElem ()
                        genSt.ilGen.Emit(OpCodes.Ldc_I4, i)
                        genSt.ilGen.Emit(OpCodes.Add)
                        genSt.ilGen.Emit(OpCodes.Stelem_I1)

                    | Print ->
                        pushElem ()
                        genSt.ilGen.Emit(OpCodes.Call, writeCharMethInfo);

                    | Read ->
                        pushTapeAndPtr ()
                        genSt.ilGen.Emit(OpCodes.Call, readCharMethInfo)
                        genSt.ilGen.Emit(OpCodes.Conv_U2);
                        genSt.ilGen.Emit(OpCodes.Stelem_I1)

                    | Left ->
                        genSt.ilGen.Emit(OpCodes.Ldloc, ptrLoc)
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeLenLoc)
                        genSt.ilGen.Emit(OpCodes.Call, genSt.leftMeth)
                        genSt.ilGen.Emit(OpCodes.Stloc, ptrLoc)

                    | Right ->
                        genSt.ilGen.Emit(OpCodes.Ldloc, ptrLoc)
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeLenLoc)
                        genSt.ilGen.Emit(OpCodes.Call, genSt.rightMeth)
                        genSt.ilGen.Emit(OpCodes.Stloc, ptrLoc)

                    | While stmts ->
                        let condLab = genSt.ilGen.DefineLabel()
                        let beginLab = genSt.ilGen.DefineLabel()                     
                        genSt.ilGen.Emit(OpCodes.Br, condLab)

                        genSt.ilGen.MarkLabel(beginLab)
                        emitStmts stmts

                        genSt.ilGen.MarkLabel(condLab)
                        pushElem ()
                        genSt.ilGen.Emit(OpCodes.Ldc_I4_0)
                        genSt.ilGen.Emit(OpCodes.Bne_Un, beginLab)

                    | Fork ->               
                        // src
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeLoc)
                        // dst
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeLenLoc)
                        genSt.ilGen.Emit(OpCodes.Newarr, typeof<char>)
                        genSt.ilGen.Emit(OpCodes.Dup)
                        genSt.ilGen.Emit(OpCodes.Stloc, tapeCpyLoc)         
                        // len
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeLenLoc)
                        // copy
                        genSt.ilGen.Emit(OpCodes.Call, arrayCopyMethInfo)

                        // zero cell in parent
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeLoc)
                        genSt.ilGen.Emit(OpCodes.Ldloc, ptrLoc)
                        genSt.ilGen.Emit(OpCodes.Ldc_I4_0)
                        genSt.ilGen.Emit(OpCodes.Stelem_I1)

                        // 1 in child
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeCpyLoc)
                        genSt.ilGen.Emit(OpCodes.Ldloc, ptrLoc)
                        genSt.ilGen.Emit(OpCodes.Ldc_I4_1)
                        genSt.ilGen.Emit(OpCodes.Add)
                        genSt.ilGen.Emit(OpCodes.Ldc_I4_1)
                        genSt.ilGen.Emit(OpCodes.Stelem_I1)

                        // create thread
                        genSt.ilGen.Emit(OpCodes.Ldnull)
                        genSt.ilGen.Emit(OpCodes.Ldftn, genSt.runMeth)
                        genSt.ilGen.Emit(OpCodes.Newobj, parameterizedThreadCtorInfo)
                        genSt.ilGen.Emit(OpCodes.Newobj, threadCtorInfo)

                        // arg
                        genSt.ilGen.Emit(OpCodes.Ldloc, ptrLoc)
                        genSt.ilGen.Emit(OpCodes.Ldc_I4_1)
                        genSt.ilGen.Emit(OpCodes.Add)
                        genSt.ilGen.Emit(OpCodes.Ldloc, tapeCpyLoc)
                        genSt.ilGen.Emit(OpCodes.Ldc_I4, genSt.currFork)
                        genSt.ilGen.Emit(OpCodes.Newobj, genSt.argTy.GetConstructors().[0])

                        // start thread
                        genSt.ilGen.Emit(OpCodes.Callvirt, threadStartMethInfo)

                        setForkLabel ()

            and emitStmts = List.iter emitStmt

            match prog with
                | Prog stmts ->
                    genSt.ilGen.Emit(OpCodes.Ldarg_0)
                    genSt.ilGen.Emit(OpCodes.Castclass, genSt.argTy)
                    genSt.ilGen.Emit(OpCodes.Dup)
                    genSt.ilGen.Emit(OpCodes.Dup)

                    // save tape
                    genSt.ilGen.Emit(OpCodes.Ldfld, tapeFld)
                    genSt.ilGen.Emit(OpCodes.Stloc, tapeLoc)

                    // save ptr
                    genSt.ilGen.Emit(OpCodes.Ldfld, ptrFld)
                    genSt.ilGen.Emit(OpCodes.Stloc, ptrLoc)

                    // save tape len
                    genSt.ilGen.Emit(OpCodes.Ldloc, tapeLoc)
                    genSt.ilGen.Emit(OpCodes.Ldlen)
                    genSt.ilGen.Emit(OpCodes.Stloc, tapeLenLoc)

                    // load start label and branch
                    genSt.ilGen.Emit(OpCodes.Ldfld, labelFld)
                    genSt.ilGen.Emit(OpCodes.Switch, genSt.forkMap)

                    setForkLabel ()
                    emitStmts stmts
                    genSt.ilGen.Emit(OpCodes.Ret)          


        /// Performs compilation and saves exe on a disk, given compilation settings.
        let emitCIL (sets:CompilationSettings) (prog:Program) =
            let prog' = optimize sets.opt prog
            
            let asmblyBldr =
                System.AppDomain.CurrentDomain.DefineDynamicAssembly(
                    new AssemblyName("BF"),
                    AssemblyBuilderAccess.Save,
                    getDirectoryName sets.output
                )
            
            let modBldr = asmblyBldr.DefineDynamicModule("BFModule", sets.output)

            let leftM = defineLeftMeth modBldr
            let rightM = defineRightMeth modBldr
            let argT = defineThreadArgType modBldr

            let runmtd = declareThreadMeth modBldr
            let runM = runmtd.GetBaseDefinition()
            let il = runmtd.GetILGenerator()
            emitProg
                { 
                    ilGen = il;
                    runMeth = runM;
                    rightMeth = rightM;
                    leftMeth = leftM;
                    argTy = argT;
                    forkMap = Array.map (fun _ -> il.DefineLabel()) [| 0 .. countForks prog |]
                    currFork = 0;
                }
                prog'

            let mainmtd = defineMainMeth modBldr sets.ts argT runM
            asmblyBldr.SetEntryPoint(mainmtd)

            modBldr.CreateGlobalFunctions()
            asmblyBldr.Save(Path.GetFileName sets.output)
