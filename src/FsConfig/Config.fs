namespace FsConfig
open System.Text.RegularExpressions
open System.Runtime.Remoting.Messaging

type ConfigParseError =
| BadValue of (string * string)
| NotFound of string
| NotSupported of string

type ConfigParseResult<'T> = Result<'T, ConfigParseError>

type IConfigNameCanonicalizer = 
  abstract member Canonicalize: string -> string

module internal Core =

  open TypeShape
  open System

  type IConfigReader =
    abstract member GetValue : string -> string option

  [<NoEquality;NoComparison>]
  type ParseRecordConfig = {
    ConfigReader : IConfigReader
    ConfigNameCanonicalizer : IConfigNameCanonicalizer
  }

  type TryParse<'a> = string -> bool * 'a

  let tryParseWith name value tryParseFunc  = 
    match value with
    | None -> NotFound name |> Error
    | Some value -> 
      match tryParseFunc value with
      | true, v -> Ok v
      | _ -> BadValue (name, value) |> Error

  let getTryParseFunc<'T> targetTypeShape =
    let wrap(p : 'a) = Some (unbox<TryParse<'T>> p) 
    match targetTypeShape with
    | Shape.Byte -> wrap Byte.TryParse 
    | Shape.SByte -> wrap SByte.TryParse
    | Shape.Int16 -> wrap Int16.TryParse
    | Shape.Int32 -> wrap Int32.TryParse
    | Shape.Int64 -> wrap Int64.TryParse
    | Shape.UInt16 -> wrap UInt16.TryParse 
    | Shape.UInt32 -> wrap UInt32.TryParse 
    | Shape.UInt64 -> wrap UInt64.TryParse 
    | Shape.Single -> wrap Single.TryParse 
    | Shape.Double -> wrap Double.TryParse 
    | Shape.Decimal -> wrap Decimal.TryParse 
    | Shape.Char -> wrap Char.TryParse 
    | Shape.String -> wrap (fun (s : string) -> (true,s))
    | Shape.Bool -> wrap Boolean.TryParse
    | Shape.DateTimeOffset -> wrap DateTimeOffset.TryParse 
    | Shape.DateTime -> wrap DateTime.TryParse 
    | Shape.TimeSpan -> wrap TimeSpan.TryParse 
    | Shape.Char -> wrap Char.TryParse 
    | Shape.String -> wrap (fun (s : string) -> (true,s)) 
    | _ -> None

  let parseFsharpOption<'T> name value (fsharpOption : IShapeFSharpOption) =
    let wrap (p : unit -> 'a) =
      try 
        ()
        |> unbox<unit -> ConfigParseResult<'T>> p 
      with
      | ex -> 
          printfn ">>>>>>>"
          printfn "%A" ex
          printfn ">>>>>>>"
          NotSupported "" |> Error
    match value with
    | None -> (fun () -> Ok None) |> wrap
    | Some v -> 
      fsharpOption.Accept {
        new IFSharpOptionVisitor<ConfigParseResult<'T>> with
          member __.Visit<'t>() = 
            match getTryParseFunc<'t> shapeof<'t> with
            | Some tryParseFunc -> 
              match tryParseWith name (Some v) tryParseFunc with
              | Ok value -> (fun () -> value |> Some |> Ok) |> wrap
              | Error x -> (fun () -> Error x) |> wrap
            | None -> (fun () -> NotSupported "unknown target type" |> Error) |> wrap
      } 

  let parse<'T> name value =
    let targetTypeShape = shapeof<'T>
    match getTryParseFunc<'T> targetTypeShape with
    | Some tryParseFunc -> 
      tryParseWith name value tryParseFunc
    | None -> 
      match targetTypeShape with
      | Shape.FSharpOption fsharpOption -> 
        parseFsharpOption<'T> name value fsharpOption
      | _ -> NotSupported "unknown target type" |> Error

  let parsePrimitive<'T> (configReader : IConfigReader) (envVarName : string) =
    configReader.GetValue envVarName
    |> parse<'T> envVarName 

  let private parseRecordField 
    (configReader : IConfigReader) (configNameCanonicalizer : IConfigNameCanonicalizer) (shape : IShapeWriteMember<'RecordType>) = 
    let configName = 
      configNameCanonicalizer.Canonicalize shape.Label
    shape.Accept {
      new IWriteMemberVisitor<'RecordType, 'RecordType -> ConfigParseResult<'RecordType>> with
        member __.Visit (shape : ShapeWriteMember<'RecordType, 'FieldType>) =
          match parsePrimitive<'FieldType> configReader configName with
            | Ok fieldValue -> fun record -> shape.Inject record fieldValue |> Ok
            | Error e -> fun _ -> Error e
      }

  let private foldParseRecordFieldResponse (configReader : IConfigReader) (configNameCanonicalizer : IConfigNameCanonicalizer) record parseRecordErrors field =
    match parseRecordField configReader configNameCanonicalizer field record with
    | Ok _ -> parseRecordErrors
    | Error e -> e :: parseRecordErrors
  
  
  let parseRecord<'T> (configReader : IConfigReader) (configNameCanonicalizer : IConfigNameCanonicalizer)  =
    match shapeof<'T> with
    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
      let record = shape.CreateUninitialized()
      let parseRecordErrors =
        shape.Fields
        |> Seq.fold (foldParseRecordFieldResponse configReader configNameCanonicalizer record) []
      match List.isEmpty parseRecordErrors with 
      | true -> Ok record 
      |_  -> Error parseRecordErrors
    | _ -> failwith "not supported"