#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "./bin/debug/MSDNify.dll"

open MSDNify

[<Literal>]
let name = @"PresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35"
type WPF = GroupByMemberType< name >
let button = WPF.Button()
button.Methods.BeginInit()
button.Properties.Height <- 10.0
button.Properties.Height
let source = WPF.Button.Source // Get reference source code

let doc = WPF.__References.``System.Xml``.XmlDocument()
doc.Methods.LoadXml("<root/>")
doc.Properties.OuterXml
