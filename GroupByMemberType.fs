namespace MSDNifyTypeProvider

open System.Reflection
open FSharp.Quotations
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices

[<TypeProvider>]
type GroupByMemberTypeProvider (config:TypeProviderConfig) as this = 
    inherit TypeProviderForNamespaces ()

    let toParams (m:MethodBase) =
        let ps = m.GetParameters() 
        [for p in ps -> ProvidedParameter(p.Name, p.ParameterType, p.IsOut)]

    let provideFromMethod (def:ProvidedTypeDefinition) (ty:System.Type) =
        let from = ProvidedMethod("From", [ProvidedParameter("instance",ty)], def)
        from.IsStaticMethod <- true
        from.InvokeCode <- fun args -> Expr.Coerce(args.[0],def)
        def.AddMember(from)

    let provideInstanceProperty (def:ProvidedTypeDefinition) (ty:System.Type) =
        let instance = ProvidedProperty("Instance", ty)
        instance.GetterCode <- fun args -> Expr.Coerce(args.[0],ty)
        def.AddMember(instance)

    let provideConstructors (def:ProvidedTypeDefinition) (ty:System.Type) =
        for ci in ty.GetConstructors() do             
            let pc = ProvidedConstructor(toParams ci)
            pc.AddXmlDoc(ty.FullName)
            pc.InvokeCode <- fun args -> Expr.Coerce(Expr.NewObject(ci,args), typeof<obj>)
            def.AddMember(pc)  

    let provideProperties (def:ProvidedTypeDefinition) (ty:System.Type) =
        let propertiesType = ProvidedTypeDefinition(ty.Name + ".Properties", Some typeof<obj>, HideObjectMethods=true)
        let propertiesProp = ProvidedProperty("Properties", propertiesType)            
        propertiesProp.GetterCode <- fun args -> args.[0]
        propertiesType.AddMembersDelayed(fun () ->
            [for pi in ty.GetProperties() ->
                let pp = ProvidedProperty(pi.Name, pi.PropertyType)
                if pi.CanRead then
                    pp.GetterCode <- fun args -> Expr.PropertyGet(Expr.Coerce(args.[0],ty), pi)
                if pi.CanWrite then
                    pp.SetterCode <- fun args -> Expr.PropertySet(Expr.Coerce(args.[0],ty), pi, args.[1])                
                pp
            ])
        def.AddMember(propertiesProp)
        def.AddMember(propertiesType)

    let provideMethods (def:ProvidedTypeDefinition) (ty:System.Type) =
        let methodsType = ProvidedTypeDefinition(ty.Name + ".Methods", Some typeof<obj>, HideObjectMethods=true)
        let methodsProp = ProvidedProperty("Methods", methodsType)            
        methodsProp.GetterCode <- fun args -> args.[0]
        methodsType.AddMembersDelayed(fun () ->
            [for mi in ty.GetMethods() |> Seq.filter (fun mi -> not mi.IsSpecialName) ->
                let pm = ProvidedMethod(mi.Name, toParams mi, mi.ReturnType)
                pm.InvokeCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty), mi, args.Tail)                
                pm
            ])
        def.AddMember(methodsProp)
        def.AddMember(methodsType)

    let provideFields (def:ProvidedTypeDefinition) (ty:System.Type) =
        let fieldsType = ProvidedTypeDefinition(ty.Name + ".Fields", Some typeof<obj>, HideObjectMethods=true)
        let fieldsProp = ProvidedProperty("Fields", fieldsType)            
        fieldsProp.GetterCode <- fun args -> args.[0]
        fieldsType.AddMembersDelayed(fun () ->
            [for fi in ty.GetFields() -> ProvidedField(fi.Name, fi.FieldType)]
        ) 
        def.AddMember(fieldsProp)
        def.AddMember(fieldsType)

    let provideEvents (def:ProvidedTypeDefinition) (ty:System.Type) =
        let eventsType = ProvidedTypeDefinition(ty.Name + ".Events", Some typeof<obj>, HideObjectMethods=true)                        
        let eventsProp = ProvidedProperty("Events", eventsType)            
        eventsProp.GetterCode <- fun args -> args.[0]
        eventsType.AddMembersDelayed(fun () ->
            [for ei in ty.GetEvents() ->
                let pe = ProvidedEvent(ei.Name, ei.EventHandlerType) 
                pe.AdderCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty),ei.GetAddMethod(), args.Tail)
                pe.RemoverCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty), ei.GetRemoveMethod(), args.Tail)
                pe
            ])        
        def.AddMember(eventsProp)
        def.AddMember(eventsType)
       
    let referenceSource (ty:System.Type) =
        let readLines () =
            let url =
                "http://referencesource.microsoft.com/" +
                ty.Assembly.GetName().Name.Replace(".","/") +
                "/src/Framework/" + 
                ty.FullName.Replace(".", "/") + ".cs.html"
            let html = Html.download url
            html.Split('\n') |> Array.map Html.decode        
        let sourceProp = ProvidedProperty("Source", typeof<string>)            
        sourceProp.IsStatic <- true
        sourceProp.GetterCode <- fun args -> 
            let source = readLines () |> Array.map System.Web.HttpUtility.HtmlDecode |> String.concat "\r\n"
            <@@ source @@>
        sourceProp.AddXmlDocDelayed(fun () ->
            let ns = System.Text.RegularExpressions.Regex(@"using\s+[\w?.]+;")
            readLines ()
            |> Seq.filter (fun line -> ns.Match(line).Success |> not)
            |> Seq.map (fun line -> "<para>" + line + "</para>")
            |> Seq.take 30
            |> String.concat "\r\n"
            |> fun summary -> "<summary>" + summary + "</summary>"
        )
        sourceProp
        
    let provideTypes (types:System.Type seq) =
        [for ty in types ->            
            let def = ProvidedTypeDefinition(ty.Name, baseType=Some typeof<obj>, HideObjectMethods=true)
            def.AddXmlDoc(ty.FullName)
            def.AddMemberDelayed(fun () -> referenceSource ty)
            provideFromMethod def ty
            provideInstanceProperty def ty
            provideConstructors def ty              
            provideProperties def ty
            provideMethods def ty
            provideFields def ty
            provideEvents def ty
            def]

    let rec referencedAssemblies (asm:System.Reflection.Assembly) =
        let refsType = ProvidedTypeDefinition("__References", baseType=Some typeof<obj>, HideObjectMethods=true)
        for refAsm in asm.GetReferencedAssemblies() do
            let refType = ProvidedTypeDefinition(refAsm.Name, baseType=Some typeof<obj>, HideObjectMethods=true)
            refType.AddXmlDoc(refAsm.FullName)
            refType.AddMembersDelayed(fun () ->
                let loadedAssembly = Assembly.Load(refAsm)
                let types = loadedAssembly.GetExportedTypes()
                referencedAssemblies loadedAssembly::provideTypes types                   
            )
            refsType.AddMember(refType)
        refsType

    let ns = "MSDNify"
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let providedType = ProvidedTypeDefinition(asm, ns, "GroupByMemberType", Some typeof<obj>)
    do  providedType.DefineStaticParameters(
            staticParameters=[ProvidedStaticParameter("assemblyName", typeof<string>)],
            apply=(fun typeName parameterValues ->
                let assemblyName = parameterValues.[0] :?> string
                let ty = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
                let loadedAssembly = Assembly.Load(assemblyName)     
                let types = loadedAssembly.ExportedTypes
                ty.AddMembersDelayed(fun () ->
                    referencedAssemblies loadedAssembly::provideTypes types)
                ty
            )
        )
    do  this.AddNamespace(ns, [providedType])
