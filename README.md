# LocParser

LocParser is a localization parser base on [FParsec](https://github.com/stephan-tolksdorf/fparsec)

# Supported localization gramma

- key = 'literal value'
- key = $parameterName
- key = {$parameterName: 'pluralform1', 'pluralform2'} 
	<br><sup>Plural forms count depends on localization, supported localization you can find in LocParser.fs file</sup>
- key = AnotherKey
- key = 'literal' + $parameterName + {$parameterName: 'pluralform1', 'pluralform2'} + anotherKey
- ; comment
	
# Using
## Example file localization.txt
	key1 = 'key 1 data'
	key2 = 'key 2 data ' + $param + {$param: ' one', ' many'}
	key3 = 'key 3 data ' + key1
	
## Example code
	
	let path = @"localization.txt"
        use stream = File.OpenRead(path) 
        
        let parserResult = match loadLocFile stream Encoding.UTF8  with
                            | Success(result, _, _)   -> Some result
                            | Failure(_, _, _) -> None
        
        parserResult.IsSome |> should equal true
    
        let locDataMap = parserResult.Value
                        |> List.filter (fun v -> match v with
                                                    | Loc.Comment x -> false
                                                    | _ -> true) 
                        |> List.map (fun v -> match v with 
                                                |Loc.Data(key,value) -> (key, value)
                                                |_ -> failwith "unexpected")
                        |> dict
        
        evalLocValue locDataMap.["key1"] "en"  (fun v -> v :> Object) locDataMap |> should equal "key 1 data"
        evalLocValue locDataMap.["Key2"] "en"  (fun v -> 1 :> Object) locDataMap |> should equal "key 2 data 1 one"
        evalLocValue locDataMap.["Key3"] "en"  (fun v -> v :> Object) locDataMap |> should equal "key 3 data key 1 data"
