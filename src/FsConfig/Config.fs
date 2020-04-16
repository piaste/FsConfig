namespace FsConfig
open System
open System.Text.RegularExpressions
open System.ComponentModel

type ConfigParseError =
| BadValue of (string * string)
| NotFound of string
| NotSupported of string

type Prefix = Prefix of string
type Separator = Separator of string
type SplitCharacter = SplitCharacter of char

type ConfigParseResult<'T> = Result<'T, ConfigParseError>

type FieldNameCanonicalizer = Prefix -> string -> string

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type CustomNameAttribute(name : string) =
  inherit Attribute ()
  member __.Name = name

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type DefaultValueAttribute(value : string) =
  inherit Attribute ()
  member __.Value = value

[<AttributeUsage(AttributeTargets.Property, AllowMultiple = false)>]
type ListSeparatorAttribute(splitCharacter : char) =
  inherit Attribute ()
  member __.SplitCharacter = SplitCharacter splitCharacter

[<AttributeUsage(AttributeTargets.Class, AllowMultiple = false)>]
type ConventionAttribute(prefix : string) =
  inherit Attribute ()
  member val Prefix = prefix with get,set
  member val Separator = "" with get, set

[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Property, AllowMultiple = false)>]
type CultureInfoAttribute(name: string) =
  inherit Attribute ()
  member val Name = name with get,set


type IConfigReader =
  abstract member GetValue : string -> string option


module internal Core =

  open TypeShape.Core
  open System.Globalization


  let notSupported name =
    sprintf """The target type of "%s" is currently not supported""" name
    |> NotSupported

  let defaultSplitCharacter = SplitCharacter ','

  type TryParse<'a> = string -> 'a option
  
  type FieldValueParseArgs = {
    Name : string
    ListSplitChar : SplitCharacter
    DefaultValue : string option
    CultureInfo : CultureInfo option
  }

  let getPrefixAndSeparator<'T> defaultPrefix defaultSeparator =
    let conventionAttribute =
      typeof<'T>.GetCustomAttributes(typeof<ConventionAttribute>, true)
      |> Seq.tryHead
      |> Option.map (fun a -> a :?> ConventionAttribute)
    match conventionAttribute with
    | Some attr -> 
        let prefix = 
          if (isNull attr.Prefix) then defaultPrefix else Prefix attr.Prefix
        let separator = 
          if (String.IsNullOrEmpty(attr.Separator)) then 
            defaultSeparator 
          else Separator attr.Separator
        (prefix,separator)
    | None -> (defaultPrefix,defaultSeparator)

  let findActualPrefix (Prefix customPrefix) (Separator separator) (Prefix prefix) =
    match (String.IsNullOrEmpty customPrefix, String.IsNullOrEmpty prefix) with
    | true, true -> ""
    | true, false | false, false  -> sprintf "%s%s" prefix separator 
    | false, true -> sprintf "%s%s" customPrefix separator

  let private fieldNameRegex : Regex =
    Regex("([^A-Z]+|[A-Z][^A-Z]+|[A-Z]+)", RegexOptions.Compiled)

  let fieldNameSubstrings fieldName =
    fieldNameRegex.Matches fieldName
    |> Seq.cast
    |> Seq.map (fun (m : Match) -> m.Value)
    |> Seq.toArray

  let tryParseWith name value tryParseFunc  = 
    match tryParseFunc value with
    | Some v -> Ok v
    | _ -> BadValue (name, value) |> Error
      

  let tryParse tryParseFunc value =
    match tryParseFunc value with
    | true, v -> Some v
    | _ -> None

  let tryParseDateTime args value = 
    match args.CultureInfo with
    | Some culture -> tryParse (fun d -> DateTime.TryParse(value, culture, DateTimeStyles.None))
    | None -> tryParse DateTime.TryParse

  let inline tryParseNumber< ^number 
                              when ^number : (static member TryParse: string * byref< ^number> -> bool)  
                              and ^number : (static member TryParse: string * NumberStyles * IFormatProvider * byref< ^number> -> bool)  
                           >
            args 
            value = 

    let mutable result = Unchecked.defaultof< ^number >
    
    match args.CultureInfo with
    | Some culture ->
        // culture specified, culture-sensitive parsing
        let success = 
            (^number : (static member TryParse: string * NumberStyles * IFormatProvider * byref< ^number> -> bool)
                (value, NumberStyles.Any, culture, &result)
            ) 
        if success then Some result else None

    | None ->
        // culture unspecified
        let success = 
            (^number : (static member TryParse: string * byref< ^number> -> bool)
                (value, &result)
            ) 
        if success then Some result else None

  
  let tryParseFSharpDU (shape : ShapeFSharpUnion<'T>) value =
    shape.UnionCases 
    |> Seq.tryFind (fun c -> c.CaseInfo.Name = value)
    |> Option.map (fun c -> c.CreateUninitialized ())


  let tryParseEnum<'T> (enumShape : IShapeEnum) value = 
    let wrap (p : Option<'a>) =
      unbox<Option<'T>> p
    enumShape.Accept {
      new IEnumVisitor<'T option> with
        member __.Visit<'Enum, 'U when 'Enum : enum<'U>
                                    and 'Enum : struct
                                    and 'Enum :> ValueType
                                    and 'Enum : (new : unit -> 'Enum)> () =
          tryParse System.Enum.TryParse<'Enum> value |> wrap
    }

  let getTryParseFunc<'T> (args : FieldValueParseArgs) targetTypeShape =
    let wrap(p : 'a) = 
      Some (unbox<TryParse<'T>> p) 
    match targetTypeShape with
    | Shape.Byte -> wrap (tryParseNumber<Byte> args) 
    | Shape.SByte -> wrap (tryParseNumber<SByte> args)
    | Shape.Int16 -> wrap (tryParseNumber<Int16> args)
    | Shape.Int32 -> wrap (tryParseNumber<Int32> args)
    | Shape.Int64 -> wrap (tryParseNumber<Int64> args)
    | Shape.UInt16 -> wrap (tryParseNumber<UInt16> args) 
    | Shape.UInt32 -> wrap (tryParseNumber<UInt32> args) 
    | Shape.UInt64 -> wrap (tryParseNumber<UInt64> args) 
    | Shape.Single -> wrap (tryParseNumber<Single> args) 
    | Shape.Double -> wrap (tryParseNumber<Double> args) 
    | Shape.Decimal -> wrap (tryParseNumber<Decimal> args) 
    | Shape.Char -> wrap (tryParse Char.TryParse)
    | Shape.String -> wrap (tryParse (fun (s : string) -> (true,s)))
    | Shape.Bool -> wrap (tryParse Boolean.TryParse)
    | Shape.DateTimeOffset -> wrap (tryParse DateTimeOffset.TryParse)
    | Shape.DateTime -> wrap (tryParse DateTime.TryParse) 
    | Shape.TimeSpan -> wrap (tryParse TimeSpan.TryParse) 
    | Shape.Char -> wrap (tryParse Char.TryParse) 
    | Shape.Guid -> wrap (tryParse Guid.TryParse)
    | Shape.Enum enumShape ->
      wrap (tryParseEnum<'T> enumShape)
    | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as shape) ->
      wrap (tryParseFSharpDU shape)
    | _ -> None

  let parseFSharpOption<'T> args value (fsharpOption : IShapeFSharpOption) =
    let wrap (p : ConfigParseResult<'a>) =
      unbox<ConfigParseResult<'T>> p
    fsharpOption.Element.Accept {
      new ITypeVisitor<ConfigParseResult<'T>> with
        member __.Visit<'t>() =
          match value with
          | None -> 
            let result : ConfigParseResult<'t option> = None |> Ok 
            wrap result
          | Some v ->
            match getTryParseFunc<'t> args fsharpOption.Element with
            | Some tryParseFunc ->
              match shapeof<'t> with
              | Shape.String -> 
                if String.IsNullOrWhiteSpace v then 
                  let result : ConfigParseResult<'t option> = None |> Ok 
                  wrap result
                else
                  tryParseWith args.Name v tryParseFunc 
                  |> Result.bind (Some >> Ok >> wrap) 
              | _ ->
                tryParseWith args.Name v tryParseFunc 
                |> Result.bind (Some >> Ok >> wrap) 
            | None -> notSupported args.Name |> Error 
    }

  let parseListReducer name tryParseFunc acc element = 
    acc
    |> Result.bind 
        (fun xs ->
          tryParseWith name element tryParseFunc
          |> Result.map (fun v -> v :: xs)
        )

  let parseFSharpList<'T> args value (fsharpList: IShapeFSharpList) (SplitCharacter splitCharacter) =
    let wrap (p : ConfigParseResult<'a>) =
      unbox<ConfigParseResult<'T>> p
    fsharpList.Element.Accept {
      new ITypeVisitor<ConfigParseResult<'T>> with
        member __.Visit<'t>() =
          match value with
          | None -> 
            let result : ConfigParseResult<'t list> = [] |> Ok 
            wrap result
          | Some (v : string) -> 
            match getTryParseFunc<'t> args fsharpList.Element with
            | Some tryParseFunc -> 
              v.Split(splitCharacter) 
              |> Array.map (fun s -> s.Trim())
              |> Array.filter (String.IsNullOrWhiteSpace >> not)
              |> Array.fold (parseListReducer args.Name tryParseFunc) (Ok [])
              |> Result.bind (List.rev >> Ok >> wrap)
            | None -> notSupported args.Name |> Error 
    }

  
  let rec parseInternal<'T> (configReader : IConfigReader) (fieldNameCanonicalizer : FieldNameCanonicalizer) args =
    let value = 
      match configReader.GetValue args.Name with
      | Some v -> Some v
      | None -> args.DefaultValue
    let targetTypeShape = shapeof<'T>
    match targetTypeShape with
    | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as shape) ->
      parseFSharpRecord configReader fieldNameCanonicalizer (Prefix args.Name) shape
    | Shape.FSharpOption fsharpOption -> 
      parseFSharpOption<'T> args value fsharpOption
    | Shape.FSharpList fsharpList ->
      parseFSharpList<'T> args value fsharpList args.ListSplitChar
    | _ ->
      match getTryParseFunc<'T> args targetTypeShape with
      | Some tryParseFunc -> 
        match value with
        | Some v -> tryParseWith args.Name v tryParseFunc
        | None -> NotFound args.Name |> Error
      | None -> notSupported args.Name |> Error
  and parseFSharpRecord (configReader : IConfigReader) (fieldNameCanonicalizer : FieldNameCanonicalizer) prefix shape =
    let record = shape.CreateUninitialized()
    shape.Fields
    |> Seq.fold 
      (fun acc field ->
        match acc with
        | Error x -> Error x 
        | Ok xs ->

          let customNameAttribute =
            field.MemberInfo.GetCustomAttributes(typeof<CustomNameAttribute>, true)
            |> Seq.tryHead
            |> Option.map (fun a -> a :?> CustomNameAttribute)

          let configName = 
            match customNameAttribute with
            | Some attr -> attr.Name
            | None -> fieldNameCanonicalizer prefix field.Label

          let splitCharacter = 
            field.MemberInfo.GetCustomAttributes(typeof<ListSeparatorAttribute>, true)
            |> Seq.tryHead
            |> Option.map (fun sc -> sc :?> ListSeparatorAttribute)
            |> function None -> defaultSplitCharacter | Some c -> c.SplitCharacter

          let defaultValueAttribute =
            field.MemberInfo.GetCustomAttributes(typeof<DefaultValueAttribute>, true)
            |> Seq.tryHead
            |> Option.map (fun a -> a :?> DefaultValueAttribute)
            |> Option.map (fun attr -> attr.Value)
            
          let cultureInfoAttribute =
            
            let recordCultureInfo = 
                record.GetType().GetCustomAttributes(typeof<CultureInfoAttribute>, true)
                |> Seq.tryHead
           
            let fieldCultureInfo = 
                field.MemberInfo.GetCustomAttributes(typeof<CultureInfoAttribute>, true)
                |> Seq.tryHead

            fieldCultureInfo
            |> Option.orElse recordCultureInfo
            |> Option.map (fun a -> a :?> CultureInfoAttribute)
            |> Option.map (fun attr -> CultureInfo(attr.Name))


          let args = {
            Name = configName
            ListSplitChar = splitCharacter
            DefaultValue = defaultValueAttribute
            CultureInfo = cultureInfoAttribute
          }

          field.Accept {
            new IMemberVisitor<'T, ConfigParseResult<('T -> 'T) list>> with
              member __.Visit (shape : ShapeMember<'T, 'FieldType>) =
                match parseInternal<'FieldType> configReader fieldNameCanonicalizer args with
                | Ok fieldValue -> (fun record -> shape.Set record fieldValue) :: xs |> Ok
                | Error e -> Error e
          }
       ) (Ok []) 
    |> Result.map (List.fold (fun acc f -> f acc) record)

[<AutoOpen>]
module Config =
  open Core

  let parse<'T> (configReader : IConfigReader) (fieldNameCanonicalizer : FieldNameCanonicalizer) name =
    let args = {
      Name = name
      ListSplitChar = defaultSplitCharacter
      DefaultValue = None
      CultureInfo = None
    }
    parseInternal<'T> (configReader : IConfigReader) (fieldNameCanonicalizer : FieldNameCanonicalizer) args
