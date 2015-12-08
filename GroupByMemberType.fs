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
        let instance = ProvidedProperty("__Instance", ty)
        instance.GetterCode <- fun args -> Expr.Coerce(args.[0],ty)
        def.AddMember(instance)

    let provideConstructors (def:ProvidedTypeDefinition) (ty:System.Type) =
        for ci in ty.GetConstructors() do             
            let pc = ProvidedConstructor(toParams ci)
            pc.InvokeCode <- fun args -> Expr.Coerce(Expr.NewObject(ci,args), typeof<obj>)
            def.AddMember(pc)  

    let provideProperties (def:ProvidedTypeDefinition) (ty:System.Type) =
        let propertiesType = ProvidedTypeDefinition(ty.Name + ".Properties", Some typeof<obj>, HideObjectMethods=true)
        let propertiesProp = ProvidedProperty("Properties", propertiesType)            
        propertiesProp.GetterCode <- fun args -> args.[0]
        for pi in ty.GetProperties() do             
            let pp = ProvidedProperty(pi.Name, pi.PropertyType)
            if pi.CanRead then
                pp.GetterCode <- fun args -> Expr.PropertyGet(Expr.Coerce(args.[0],ty), pi)
            if pi.CanWrite then
                pp.SetterCode <- fun args -> Expr.PropertySet(Expr.Coerce(args.[0],ty), pi, args.[1])
            propertiesType.AddMember(pp)
        def.AddMember(propertiesProp)
        def.AddMember(propertiesType)

    let provideMethods (def:ProvidedTypeDefinition) (ty:System.Type) =
        let methodsType = ProvidedTypeDefinition(ty.Name + ".Methods", Some typeof<obj>, HideObjectMethods=true)
        let methodsProp = ProvidedProperty("Methods", methodsType)            
        methodsProp.GetterCode <- fun args -> args.[0]
        for mi in ty.GetMethods() |> Seq.filter (fun mi -> not mi.IsSpecialName) do            
            let pm = ProvidedMethod(mi.Name, toParams mi, mi.ReturnType)
            pm.InvokeCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty), mi, args.Tail)
            methodsType.AddMember(pm)
        def.AddMember(methodsProp)
        def.AddMember(methodsType)

    let provideFields (def:ProvidedTypeDefinition) (ty:System.Type) =
        let fieldsType = ProvidedTypeDefinition(ty.Name + ".Fields", Some typeof<obj>, HideObjectMethods=true)
        let fieldsProp = ProvidedProperty("Fields", fieldsType)            
        fieldsProp.GetterCode <- fun args -> args.[0]
        for fi in ty.GetFields() do 
            let field = ProvidedField(fi.Name, fi.FieldType)
            fieldsType.AddMember(field)
        def.AddMember(fieldsProp)
        def.AddMember(fieldsType)

    let provideEvents (def:ProvidedTypeDefinition) (ty:System.Type) =
        let eventsType = ProvidedTypeDefinition(ty.Name + ".Events", Some typeof<obj>, HideObjectMethods=true)                        
        let eventsProp = ProvidedProperty("Events", eventsType)            
        eventsProp.GetterCode <- fun args -> args.[0]
        for ei in ty.GetEvents() do                     
            let pe = ProvidedEvent(ei.Name, ei.EventHandlerType) 
            pe.AdderCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty),ei.GetAddMethod(), args.Tail)
            pe.RemoverCode <- fun args -> Expr.Call(Expr.Coerce(args.Head,ty), ei.GetRemoveMethod(), args.Tail)
            eventsType.AddMember(pe)
        def.AddMember(eventsProp)
        def.AddMember(eventsType)

    let provideTypes (types:System.Type[]) =
        [for ty in types |> Seq.where(fun x -> x.IsPublic) ->            
            let def = ProvidedTypeDefinition(ty.Name, baseType=Some typeof<obj>, HideObjectMethods=true)
            def.AddXmlDocDelayed(fun () -> ty.FullName)
            provideFromMethod def ty
            provideInstanceProperty def ty
            provideConstructors def ty              
            provideProperties def ty
            provideMethods def ty
            provideFields def ty
            provideEvents def ty
            def]

    let ns = "MSDNify"
    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let providedType = ProvidedTypeDefinition(asm, ns, "GroupByMemberType", Some typeof<obj>)
    do  providedType.DefineStaticParameters(
            staticParameters=[ProvidedStaticParameter("assemblyName", typeof<string>)],
            apply=(fun typeName parameterValues ->
                let assemblyName = parameterValues.[0] :?> string                
                let ty = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
                let types = Assembly.Load(assemblyName).GetTypes()
                ty.AddMembersDelayed(fun () -> provideTypes types)
                ty
            )
        )
    do  this.AddNamespace(ns, [providedType])
