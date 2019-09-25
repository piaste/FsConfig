namespace FsConfig

open FsConfig.Core
open System

type EnvConfig =
  static member private configReader (log) = {
    new IConfigReader with
      member __.GetValue name =
        let v = Environment.GetEnvironmentVariable name
        log (sprintf "Reading environment variable '%s'" name)
        if v = null then None else Some v
  }
  static member private fieldNameCanonicalizer customPrefix (Separator separator) : FieldNameCanonicalizer = 
    fun prefix name -> 
      let actualPrefix =
        findActualPrefix customPrefix (Separator separator) prefix
      let subStrings =
        fieldNameSubstrings name
        |> Array.map (fun v -> v.ToUpperInvariant())
      String.Join(separator, subStrings)
      |> sprintf "%s%s" actualPrefix
  static member private defaultPrefix = Prefix ""
  static member private defaultSeparator = Separator "_"
  static member private defaultFieldNameCanonicalizer =
    EnvConfig.fieldNameCanonicalizer EnvConfig.defaultPrefix EnvConfig.defaultSeparator

  static member Get<'T when 'T :> IConvertible> (envVarName : string, ?log : string -> unit) =     
    let log = log |> Option.defaultValue ignore
    parse<'T> (EnvConfig.configReader log) EnvConfig.defaultFieldNameCanonicalizer envVarName

  static member Get<'T when 'T : not struct> (?log : string -> unit) =
    
    let log = log |> Option.defaultValue ignore

    let fieldNameCanonicalizer = 
      let (prefix, separator) = 
        getPrefixAndSeparator<'T> EnvConfig.defaultPrefix EnvConfig.defaultSeparator        

      log <| sprintf "Environment variable reading rules: %A, %A" prefix separator

      EnvConfig.fieldNameCanonicalizer prefix separator

    parse<'T> (EnvConfig.configReader log) fieldNameCanonicalizer ""

  static member Get<'T when 'T : not struct> (fieldNameCanonicalizer : FieldNameCanonicalizer, ?log : string -> unit) =
    let log = log |> Option.defaultValue ignore

    parse<'T> (EnvConfig.configReader log) fieldNameCanonicalizer ""
  