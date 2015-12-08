#r "WindowsBase.dll"
#r "PresentationCore.dll"
#r "PresentationFramework.dll"
#r "./bin/debug/MSDNify.dll"

open MSDNify

[<Literal>]
let name = @"PresentationFramework, Version=4.0.0.0, Culture=neutral, PublicKeyToken=31bf3856ad364e35"
type WPF = GroupByMemberType< name >
let button = WPF.Button()
button.Events.Click.Add(fun x -> ())
button.Properties.ActualHeight
button.Properties.MinWidth <- 10.0
button.Methods.BeginInit()
button.Methods.EndInit()

