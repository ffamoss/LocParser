namespace Loc
module LocParser =     
    open FParsec
    open System
    open System.Collections.Generic
    open System.IO
    
    type Id = string
    
    type Value =
        | Literal of string
        | KeyRef of Id
        | Param of Id
        | ParamPluralForm of Id * string list
    
    type Loc = | Data of Id * Value list
               | Comment of string
    
    let skipSpaces: Parser<unit,'u> =
        fun stream ->
            stream.Skip(' ') |> ignore
            Reply(())
    
    let ws = spaces
    
    let str_ws s = pstring s .>> ws
    
    let identifier : Parser<string,unit> =
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"  .>> ws 
    
    
    let stringLiteral : Parser<string,unit> =
        let normalChar = satisfy (fun c -> c <> '\\' && c <> ''')
        let unescape c = match c with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | c   -> c
        let escapedChar = pstring "\\" >>. (anyOf "\\nrt'\"" |>> unescape)
        
        between (pstring "'") (pstring "'")
                (manyChars (normalChar <|> escapedChar))
    
    let parameterKey : Parser<string,unit> =    
        pstring "$" >>. identifier  .>> ws <?> "paramKey"
    
    let parameterKeyPluralForm : Parser<(string*string list),unit> =
        tuple2 
            (str_ws "{" >>. parameterKey) 
            (str_ws ":" >>. (sepBy (stringLiteral .>> ws) (str_ws "," )) .>> ws .>> str_ws "}" ) <?> "plural form"
    
    let vparam = parameterKey |>> Value.Param    
    let vparamPluralForm = parameterKeyPluralForm |>> Value.ParamPluralForm    
    let vkeyRef = identifier |>> Value.KeyRef
    let vliteral = stringLiteral |>> Value.Literal
    
    let value : Parser<Value list,unit> =
        sepBy1 ( (vliteral .>> ws ) <|> vkeyRef <|> vparam <|> vparamPluralForm ) (skipSpaces >>.  str_ws "+")
        
        
    let loc : Parser<(string*Value list),unit> =
        tuple2 (ws >>. identifier .>> ws) ( str_ws "=" >>. value .>> ws )
    
    let locData = loc |>> Loc.Data
    
    let comment : Parser<string,unit> =
        let commentStart c = c = ';'
        let allExceptNewLine c =  c <> '\n'
    
        many1Satisfy2L commentStart allExceptNewLine "comment" .>> ws 
    
    let locComment =
            comment |>> Loc.Comment
    
    let locFile : Parser<Loc list, unit> =
        many (locComment <|> locData)
        
    
    let pluralFormRules = dict [
                            ("fa", []);
                            ("id", []);
                            ("ja", []);
                            ("kk", []);
                            ("ko", []);
                            ("zh", []);
                            ("tr", [fun n -> n < 2]);
                            ("vi", []);
                            ("th", []);
                            
                            ("bg", [fun n -> n = 1]);
                            ("da", [fun n -> n = 1]);
                            ("de", [fun n -> n = 1]);
                            ("el", [fun n -> n = 1]);
                            ("en", [fun n -> n = 1]);
                            ("es", [fun n -> n = 1]);
                            ("et", [fun n -> n = 1]);
                            ("fi", [fun n -> n = 1]);
                            ("hu", [fun n -> n = 1]);
                            ("it", [fun n -> n = 1]);
                            ("nl", [fun n -> n = 1]);
                            ("no", [fun n -> n = 1]);
                            ("nb", [fun n -> n = 1]);
                            ("pt", [fun n -> n = 1]);
                            ("sq", [fun n -> n = 1]);
                            ("sv", [fun n -> n = 1]);
                            ("ar", [fun n -> n = 0;
                                    fun n -> n = 1;
                                    fun n -> n = 2;
                                    fun n -> (n % 100 >= 3) && (n % 100 <= 10);
                                    fun n -> (n % 100 >= 11) && (n % 100 <= 99)]);
                            ("fr",    [fun n -> n < 2]);
                            ("pt-BR", [fun n -> n < 2]);    
                            ("cs", [fun n -> n = 1;
                                    fun n -> n >= 2 && n <= 4]);
                            ("ga", [fun n -> n = 1;
                                    fun n -> n = 2;
                                    fun n -> n % 10 >= 3 && n % 10 <= 6;
                                    fun n -> n % 10 >= 7 && n % 10 <= 9]);
    
                            ("lt", [fun n -> (n % 10 = 1) && (n % 100 <> 11);
                                    fun n -> (n % 10 = 0) || (n % 100 >= 11 && n % 100 <= 19)]);
                            
                            ("lv", [fun n -> (n % 10 = 1) && (n % 100 <> 11);
                                    fun n -> (n % 10 = 0) || (n % 100 >= 11 && n % 100 <= 19)]);
                            
                            ("mk", [fun n -> n = 1]);
                            
                            ("pl", [fun n -> n = 1;
                                    fun n -> (n % 10 >= 2) && (n % 10 <= 4) && (n % 100 < 10  || n % 100 > 20)]);
    
                            ("ro", [fun n -> n = 1;
                                    fun n -> n = 0 || (n % 100 > 0 && n % 100 < 20)]);
    
                            ("ru", [fun n -> n % 10 = 1 && n % 100 <> 11;
                                    fun n -> n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 10 || n % 100 > 20)]);
    
                            ("uk", [fun n -> n % 10 = 1 && n % 100 <> 11;
                                    fun n -> n % 10 >= 2 && n % 10 <= 4 && (n % 100 < 10 || n % 100 > 20)]);
                             
                            ("sr", [fun n -> (n % 10 = 1) && (n % 100 <> 11);
                                    fun n -> (n % 10 >= 2) && (n % 10 <= 4) && (n % 100 < 10 || n % 100 >= 20)]);
                            
                            ("sl", [fun n -> n = 1;
                                    fun n -> n % 100 = 2;
                                    fun n -> (n % 100 = 3) || (n % 100 = 4)])
        ]
    
    
    let paramResolver key culture (paramValueGetter : string -> Object) (pluralforms : string list) =
        match paramValueGetter key with
        | :? int as v  when pluralforms.Length > 0 ->                
                    let form = match pluralforms with
                               | [] -> ""
                               | _ -> match pluralFormRules.TryGetValue culture  with
                                      | true, rules -> pluralforms.[rules |> Seq.takeWhile (fun rule -> not (rule v)) |> Seq.length]
                                      | _ -> ""
                    Value.Literal (form)
        | v -> Value.Literal (v.ToString())
        
    let keyResolver key (existLocData: IDictionary<string,Value list>)  =
        match existLocData.TryGetValue key with
        | true, value -> value
        | _ -> failwithf "KeyRef %s not found" key
    
    let evalLocValue locValue culture paramValueGetter (existLocData: IDictionary<string,Value list>) =
       let rec resolve v acc=
            match v with
            |h::xs -> match h with
                      |Value.Literal x -> resolve xs (x::acc)
                      |Value.Param x -> resolve ((paramResolver x culture paramValueGetter [])::xs) acc
                      |Value.KeyRef k -> resolve ((keyResolver k existLocData)@xs) acc
                      |Value.ParamPluralForm(key, lst) -> resolve ((paramResolver key culture paramValueGetter lst)::xs) acc
            |[] -> acc  
    
       resolve locValue [] |> List.rev |> List.reduce (+)
    
    
    let loadLocFile (fileStream : Stream) (encoding:  System.Text.Encoding)  =
        runParserOnStream locFile () "" fileStream encoding
         