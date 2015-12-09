#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "./bin/debug/MSDNify.dll"

open MSDNify

[<Literal>]
let name = @"PresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35"
type WPF = GroupByMemberType< name >
let button = WPF.Button()
button.Events

let doc = WPF.__References.``System.Xml``.XmlDocument()
doc.Methods.LoadXml("<root/>")
doc.Properties.OuterXml