module Html

open System.Web
open System.Net

let decode (text:string) =
    let result = System.Text.StringBuilder()
    let rec append index =
        if index < text.Length then
            let c = text.[index]
            if c = '<' then skip (index+1)
            else
                if c <> '\r' && c <> '\n' then
                    result.Append(c) |> ignore
                append (index+1)
    and skip index =
        if index < text.Length then
            let c = text.[index]
            if c = '>' then append (index+1)
            else skip (index+1)
    append 0
    result.ToString()

let download (url:string) =
    use client = new WebClient()
    client.DownloadString(url)

