#r "nuget: Unquote, 6.1.0"

open Swensen.Unquote

module List =
    let splitOn (predicate: 'a -> bool) (list: 'a list) : 'a list list =
        let result, remaining =
            List.foldBack
                (fun value (result: 'a list list, current: 'a list) ->
                    if predicate value then
                        if List.isEmpty current then
                            (result, [])
                        else
                            (current :: result, [])
                    else
                        (result, value :: current))
                list
                ([], [])

        if List.isEmpty remaining then
            result
        else
            remaining :: result

let tests =
    test <@ List.splitOn (fun v -> v % 3 = 0) [ 1; 2; 3; 4; 5; 6; 7 ] = [ [ 1; 2 ]; [ 4; 5 ]; [ 7 ] ] @>
    test <@ List.splitOn (fun _ -> true) [ 1; 2; 3; 4; 5; 6; 7 ] = [] @>
    test <@ List.splitOn (fun v -> v % 3 = 0) [ 1; 2; 3; 3; 3; 6; 7 ] = [ [ 1; 2 ]; [ 7 ] ] @>
