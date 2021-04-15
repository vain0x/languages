module rec MiloneSyntaxV2.Program

open MiloneSyntaxV2.Cli

[<EntryPoint>]
let main _ = cliMain (stdin.ReadToEnd ())
