namespace BF
    
    open AST
    open System.Reflection
    open System.Reflection.Emit

    /// Contains basic runtime info and subroutines.
    module Base =
        
        /// CLR method: Array.Copy.
        let arrayCopyMethInfo =
            typeof<System.Array>.GetMethod (
                "Copy",
                BindingFlags.Public ||| BindingFlags.Static,
                null,
                [| typeof<System.Array>; typeof<System.Array>; typeof<int> |],
                null
            )

        /// CLR method: Write(char).
        let writeCharMethInfo =
            typeof<System.Console>.GetMethod(
                "Write",
                BindingFlags.Public ||| BindingFlags.Static, 
                null, 
                [| typeof<char> |],
                null
            )

        /// CLR method: Read(char).
        let readCharMethInfo =
            typeof<System.Console>.GetMethod(
                "Read",
                BindingFlags.Public ||| BindingFlags.Static, 
                null, 
                [| |],
                null
            )

        /// CLR method: thread.start().
        let threadStartMethInfo =
            typeof<System.Threading.Thread>.GetMethod(
                "Start",
                BindingFlags.Public ||| BindingFlags.Instance,
                null,
                [| typeof<System.Object> |],
                null
            )

        /// Constructor of a ParameterizedThreadStart in CLR.
        let parameterizedThreadCtorInfo =
            typeof<System.Threading.ParameterizedThreadStart>.GetConstructors().[0]

        /// Constructor of a thread in CLR.
        let threadCtorInfo =
            typeof<System.Threading.Thread>.GetConstructor( [| typeof<System.Threading.ParameterizedThreadStart> |] )

        /// Defines method for moving pointer to the right in a given module.
        let defineRightMeth (mb:ModuleBuilder) =
            let rightmtd =
                mb.DefineGlobalMethod(
                    "right",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<int>,
                    [| typeof<int>; typeof<int> |] //ptr, len
                )

            let il = rightmtd.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldc_I4_1)
            il.Emit(OpCodes.Add)
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Ldarg_1)

            let retLab = il.DefineLabel()
            il.Emit(OpCodes.Blt_S, retLab)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Sub)

            il.MarkLabel(retLab)
            il.Emit(OpCodes.Ret)
            rightmtd.GetBaseDefinition()

        /// Defines method for moving pointer to the left in a given module.
        let defineLeftMeth (mb:ModuleBuilder) =
            let leftmtd =
                mb.DefineGlobalMethod(
                    "left",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<int>,
                    [| typeof<int>; typeof<int> |] //ptr, len
                )

            let il = leftmtd.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldc_I4_1)
            il.Emit(OpCodes.Sub)
            il.Emit(OpCodes.Dup)
            il.Emit(OpCodes.Ldc_I4_0)

            let retLab = il.DefineLabel()
            il.Emit(OpCodes.Bge_S, retLab)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Add)

            il.MarkLabel(retLab)
            il.Emit(OpCodes.Ret)
            leftmtd.GetBaseDefinition()

        /// Defines type of thread's argument in a given module.
        let defineThreadArgType (mb:ModuleBuilder) =
            let argT = mb.DefineType("Arg", TypeAttributes.Public)
            let ptrfld = argT.DefineField("ptr", typeof<int>, FieldAttributes.Public)
            let tapefld = argT.DefineField("tape", typeof<char[]>, FieldAttributes.Public)
            let labelfld = argT.DefineField("label", typeof<int>, FieldAttributes.Public)

            let ctorBldr =
                argT.DefineConstructor(
                    MethodAttributes.Public ||| MethodAttributes.HideBySig,
                    CallingConventions.Standard,
                    [| typeof<int>; typeof<char[]>; typeof<int> |] // ptr, tape, label
                )

            let baseCtorInf =
                typeof<System.Object>.GetConstructor([| |])

            let il = ctorBldr.GetILGenerator();
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, baseCtorInf)

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_1)
            il.Emit(OpCodes.Stfld, ptrfld)

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_2)
            il.Emit(OpCodes.Stfld, tapefld)

            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Ldarg_3)
            il.Emit(OpCodes.Stfld, labelfld)

            il.Emit(OpCodes.Ret)

            argT.CreateType()

        /// Declares main thread method with a BF program in a given module. Leaves body to be implemented.
        let declareThreadMeth (mb:ModuleBuilder) =
            let runmtd =
                mb.DefineGlobalMethod(
                    "run",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    null,
                    [| typeof<System.Object> |]
                )

            runmtd

        /// Defines main method in a given module.
        let defineMainMeth (mb:ModuleBuilder) (tapeSize:int) (argT:System.Type) (runM:MethodInfo) =
            let mainmtd =
                mb.DefineGlobalMethod(
                    "main",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    null,
                    null
                )

            let il = mainmtd.GetILGenerator()

            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Ldc_I4, tapeSize)
            il.Emit(OpCodes.Newarr, typeof<char>)
            il.Emit(OpCodes.Ldc_I4_0)
            il.Emit(OpCodes.Newobj, argT.GetConstructors().[0])
            il.Emit(OpCodes.Call, runM)
            il.Emit(OpCodes.Ret)

            mainmtd.GetBaseDefinition()