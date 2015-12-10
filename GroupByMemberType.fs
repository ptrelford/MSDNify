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

    let fromMethod (ty:System.Type) (def:ProvidedTypeDefinition) =
        let from = ProvidedMethod("From", [ProvidedParameter("instance",ty)], def)
        from.IsStaticMethod <- true
        from.InvokeCode <- fun args -> Expr.Coerce(args.[0],def)
        from

    let instanceProperty (ty:System.Type) =
        let instance = ProvidedProperty("Instance", ty)
        instance.GetterCode <- fun args -> Expr.Coerce(args.[0],ty)
        instance

    let constructors (ty:System.Type) : MemberInfo list =
        [for ci in ty.GetConstructors() ->             
            let pc = ProvidedConstructor(toParams ci)
            pc.AddXmlDoc(ty.FullName)
            pc.InvokeCode <- fun args -> Expr.Coerce(Expr.NewObject(ci,args), typeof<obj>)
            upcast pc]
                       
    let propertiesType (ty:System.Type) =
        let propertiesType = ProvidedTypeDefinition(ty.Name + ".Properties", Some typeof<obj>, HideObjectMethods=true)
        propertiesType.AddMembersDelayed(fun () ->
            [for pi in ty.GetProperties() ->
                let pp = ProvidedProperty(pi.Name, pi.PropertyType)
                if pi.CanRead then
                    pp.GetterCode <- fun args -> Expr.PropertyGet(Expr.Coerce(args.[0],ty), pi)
                if pi.CanWrite then
                    pp.SetterCode <- fun args -> Expr.PropertySet(Expr.Coerce(args.[0],ty), pi, args.[1])                
                pp
            ])        
        propertiesType

    let properties (ty:System.Type) : MemberInfo list =
        let propertiesType = propertiesType ty
        let propertiesProp = ProvidedProperty("Properties", propertiesType)
        propertiesProp.GetterCode <- fun args -> args.[0]
        [propertiesType; propertiesProp]

    let methodsType (ty:System.Type) =
        let methodsType = ProvidedTypeDefinition(ty.Name + ".Methods", Some typeof<obj>, HideObjectMethods=true)
        methodsType.AddMembersDelayed(fun () ->
            [for mi in ty.GetMethods() |> Seq.filter (fun mi -> not mi.IsSpecialName) ->
                let pm = ProvidedMethod(mi.Name, toParams mi, mi.ReturnType)
                pm.InvokeCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty), mi, args.Tail)            
                pm
            ])
        methodsType

    let methods (ty:System.Type) : MemberInfo list =
        let methodsType = methodsType ty
        let methodsProp = ProvidedProperty("Methods", methodsType)    
        methodsProp.GetterCode <- fun args -> args.[0]
        [methodsProp; methodsType]

    let fieldsType (ty:System.Type) =
        let fieldsType = ProvidedTypeDefinition(ty.Name + ".Fields", Some typeof<obj>, HideObjectMethods=true)
        fieldsType.AddMembersDelayed(fun () ->
            [for fi in ty.GetFields() -> ProvidedField(fi.Name, fi.FieldType)]
        )
        fieldsType

    let fields (ty:System.Type) : MemberInfo list =
        let fieldsType = fieldsType ty
        let fieldsProperty = ProvidedProperty("Fields", fieldsType)
        fieldsProperty.GetterCode <- fun args -> args.[0]
        [fieldsType; fieldsProperty]

    let eventsType (ty:System.Type) =
        let eventsType = ProvidedTypeDefinition(ty.Name + ".Events", Some typeof<obj>, HideObjectMethods=true)                        
        eventsType.AddMembersDelayed(fun () ->
            [for ei in ty.GetEvents() ->
                let pe = ProvidedEvent(ei.Name, ei.EventHandlerType)   
                pe.AdderCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty),ei.GetAddMethod(), args.Tail)
                pe.RemoverCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty), ei.GetRemoveMethod(), args.Tail)
                pe
            ])
        eventsType

    let events (ty:System.Type) : MemberInfo list =
        let eventsType = eventsType ty
        let eventsProperty eventsType =
            ProvidedProperty("Events", eventsType, GetterCode = fun args -> args.[0])
        [eventsType; eventsProperty eventsType]
       
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
        
    let typeMembers (ty:System.Type) (def:ProvidedTypeDefinition) : MemberInfo list = [   
        yield upcast fromMethod ty def
        yield upcast instanceProperty ty
        yield upcast referenceSource ty
        yield! constructors ty
        yield! properties ty
        yield! methods ty
        yield! fields ty
        yield! events ty
        ]      

    let provideTypes (types:System.Type seq) =
        [for ty in types ->
            let def = ProvidedTypeDefinition(ty.Name, baseType=Some typeof<obj>, HideObjectMethods=true)
            def.AddXmlDoc(ty.FullName)
            def.AddMembersDelayed(fun () -> typeMembers ty def)
            def
        ]

    let rec referencedAssemblies (assembly:Assembly) =
        let refsType = ProvidedTypeDefinition("__References", baseType=Some typeof<obj>, HideObjectMethods=true)
        for refAsm in assembly.GetReferencedAssemblies() do
            let refType = ProvidedTypeDefinition(refAsm.Name, baseType=Some typeof<obj>, HideObjectMethods=true)
            refType.AddXmlDoc(refAsm.FullName)
            refType.AddMembersDelayed(fun () -> Assembly.Load(refAsm) |> assemblyMembers)
            refsType.AddMember(refType)
        refsType
    and assemblyMembers (assembly:Assembly) =        
        let types = assembly.GetExportedTypes()
        referencedAssemblies assembly :: provideTypes types

    let ns = "MSDNify"
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let providedType = ProvidedTypeDefinition(asm, ns, "GroupByMemberType", Some typeof<obj>)
    do  providedType.DefineStaticParameters(
            staticParameters=[ProvidedStaticParameter("assemblyName", typeof<string>)],
            apply=(fun typeName parameterValues ->
                let assemblyString = parameterValues.[0] :?> string
                let ty = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)                                
                ty.AddMembersDelayed(fun () -> Assembly.Load(assemblyString) |> assemblyMembers)
                ty
            )
        )
    do  this.AddNamespace(ns, [providedType])
