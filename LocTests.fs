namespace Loc
module LocTests = 
    
    open LocParser
    
    open FsUnit.Xunit
    open Xunit
    open FParsec
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text
    
    
    let testParser p str =
        match run p str with
        | Success(result, _, _)   -> Some result
        | Failure(_, _, _) -> None
    
    [<Fact>]
    let ``Quotated string must be Literal`` () =
        let str = "'abc'"
        let expected = Some(Value.Literal "abc")
        testParser vliteral str |> should equal expected
    
    [<Fact>]
    let ``Quotated string with escaped symbols must be Literal`` () =
        let expectedStr = "a b s ' sas 123 __ !@#$%^ \n 02980"
        let str = "'a b s \\' sas 123 __ !@#$%^ \\n 02980'"
        let expected = Some(Value.Literal expectedStr)
        testParser vliteral str |> should equal expected
    
    [<Fact>]
    let ``Not quotated string is not Literal`` () =
        let str = "'abc"
        testParser vliteral str |> should equal None
    
    [<Fact>]
    let ``DollarSign names is Param`` () =
        let str = "$abc"
        let expected = Some(Value.Param "abc")
        testParser vparam str |> should equal expected
    
    [<Fact>]
    let ``Without dollarSign names is not Param`` () =
        let str = "abc"
        testParser vparam str |> should equal None
    
    [<Fact>]
    let ``DollarSign names with plural forms must be ParamPluralForm`` () =
        let str = "{$abc: 'p1', 'p2'}"
        let expected = Some(Value.ParamPluralForm("abc",["p1";"p2"]))
        testParser vparamPluralForm str |> should equal expected
    
    [<Fact>]
    let ``Without dollarSign names with plural forms Is't ParamPluralForm`` () =
        let str = "{abc: 'p1', 'p2'}"
        testParser vparam str |> should equal None
    
    [<Fact>]
    let ``Without quots and with dollarSign names with plural forms Is't ParamPluralForm`` () =
        let str = "{$abc: p1, 'p2'}"
        testParser vparam str |> should equal None
    
    [<Fact>]
    let ``Literal must be Value`` () =
        let str = "'abc'"
        let expected = Some([Value.Literal "abc"])
        testParser value str |> should equal expected
    
    [<Fact>]
    let ``Param must be Value`` () =
        let str = "$abc"
        let expected = Some([Value.Param "abc"])
        testParser value str |> should equal expected
    
    [<Fact>]
    let ``ParamPluralForm  must be Value`` () =
        let str = "{$abc: 'p1', 'p2'}"
        let expected = Some([Value.ParamPluralForm("abc",["p1";"p2"])])
        testParser value str |> should equal expected
    
    [<Fact>]
    let ``KeyRef must be Value`` () =
        let str = "abc"
        let expected = Some([Value.KeyRef "abc"])
        testParser value str |> should equal expected
    
    [<Fact>]
    let ``Literal + Param + ParamPluralForm + KeyRef must be Value`` () =
        let str = "'literal' + $param + {$param: 'p1', 'p2'} + keyref"
        let expected =Some [Literal "literal"; Param "param"; ParamPluralForm ("param",["p1"; "p2"]); KeyRef "keyref"]
        testParser value str |> should equal expected
    
    [<Fact>]
    let ``Id equal value must be LocData`` () =
        let str = "a = 'abc'"
        let expected = Some(Data("a",[Value.Literal "abc"]))
        testParser locData str |> should equal expected
    
    [<Fact>]
    let ``Id empty value Isn't LocData`` () =
        let str = "a  'abc'"
        testParser locData str |> should equal None
    
    [<Fact>]
    let ``Comment is LocData`` () =
        let str = "; comment"
        let expected = Some( Loc.Comment "; comment")
        testParser locComment str |> should equal expected
    
    [<Fact>]
    let ``Eval Literal LocData`` () =
        let data = [Value.Literal "test"]
        let emptyDict : IDictionary<string,Value list> = dict[]
        evalLocValue data "ru" (fun v -> v :> Object) emptyDict |> should equal "test" 
        
    [<Fact>]
    let ``Eval Param LocData`` () =
        let data = [Value.Param "param"]
        let emptyDict : IDictionary<string,Value list> = dict[]
        evalLocValue data "ru" (fun v -> (v+"1") :> Object) emptyDict |> should equal "param1" 
    
    [<Fact>]
    let ``Eval ParamPluralForm select p1 LocData`` () =
        let data = [ParamPluralForm ("param",["p1"; "p2"; "p3"])]
        let emptyDict : IDictionary<string,Value list> = dict[]
        evalLocValue data "ru" (fun v -> 1 :> Object) emptyDict |> should equal "p1" 
    
    [<Fact>]
    let ``Eval ParamPluralForm select p2 LocData`` () =
        let data = [ParamPluralForm ("param",["p1"; "p2"; "p3"])]
        let emptyDict : IDictionary<string,Value list> = dict[]
        evalLocValue data "ru" (fun v -> 2 :> Object) emptyDict |> should equal "p2" 
    
    [<Fact>]
    let ``Eval ParamPluralForm select p3 LocData`` () =
        let data = [ParamPluralForm ("param",["p1"; "p2"; "p3"])]
        let emptyDict : IDictionary<string,Value list> = dict[]
        evalLocValue data "ru" (fun v -> 10 :> Object) emptyDict |> should equal "p3" 
    
    [<Fact>]
    let ``Eval KeyRef LocData`` () =
        let data = [Value.KeyRef "key1"]
        let emptyDict : IDictionary<string,Value list> = dict[("key1", [Value.Literal "Key1Data"])]
        evalLocValue data "ru" (fun v -> v :> Object) emptyDict |> should equal "Key1Data" 
    
    [<Fact>]
    let ``Eval Literals LocData must be in correct order`` () =
        let data = [Value.Literal "test1"; Value.Literal "test2"; Value.Literal "test3"]
        let emptyDict : IDictionary<string,Value list> = dict[]
        evalLocValue data "ru" (fun v -> v :> Object) emptyDict |> should equal "test1test2test3" 
    
    [<Fact>]
    let ``Eval nested KeyRef LocData`` () =
        let data = [Value.KeyRef "key1"]
        let emptyDict : IDictionary<string,Value list> = dict[("key1", [Value.KeyRef "Key2"]); ("Key2", [Value.Literal "Data"])]
        evalLocValue data "ru" (fun v -> v :> Object) emptyDict |> should equal "Data" 
    
    
    [<Fact>]
    let ``Loc file is LocData list`` () =
        let str = @"; comment
        key1 = 'data1'
        key2 = 'data2' + $param"
        let expected =Some( [Comment "; comment"; 
                             Data ("key1",[Literal "data1"]); 
                             Data ("key2",[Literal "data2"; Param "param"])])
        testParser locFile str |> should equal expected
    
    
    [<Fact(Skip = "Fill correct path and keys")>]
    let ``Real loc file is LocData list`` () =
        let path = @"ru\locs\Common.txt"
        use stream = File.OpenRead(path) 
        
        let parserResult = match loadLocFile stream Encoding.UTF8  with
                            | Success(result, _, _)   -> Some result
                            | Failure(_, _, _) -> None
        
        do (parserResult.IsSome |> should equal true)
    
        let locDataMap = parserResult.Value
                        |> List.filter (fun v -> match v with
                                                    | Loc.Comment x -> false
                                                    | _ -> true) 
                        |> List.map (fun v -> match v with 
                                                |Loc.Data(key,value) -> (key, value)
                                                |_ -> failwith "unexpected")
                        |> dict
        
        evalLocValue locDataMap.["Key1"] "ru"  (fun v -> v :> Object) locDataMap |> should equal "Fill correct data for Key1"
    
        evalLocValue locDataMap.["Key2"] "ru"  (fun v -> 1 :> Object) locDataMap |> should equal "Fill correct data for Key2"
        