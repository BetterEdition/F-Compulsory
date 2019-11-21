open System

// Learn more about F# at http://fsharp.org


let list2 = [ 1;1;1;1;2;2;2;3;3;3;3;3;4;4;4;4;4;4;5;5;6;6;7;7;7;7;7;7;7;8;8;8;8;8;9;9;9;9;9;10; ]


// 4.11 - 1. Find Occurrences 
let rec count weakList item =
    match weakList with
    | [] -> 0
    | x::[] -> if x = item then 1 else 0
    | x::xs -> (if x = item then 1 else 0) + count xs item

// 4.11 - 2. Insert
let rec insert weakList item =
    match weakList with
    | [] -> [item]
    | x::[] when item <= x -> [item] @ [x]
    | x::[] when item > x -> [x] @ [item]
    | x::xs when item <= x -> item::[x] @ xs
    | x::xs when item > x -> x::(insert xs item)
    | _ -> failwith "Incomplete match on %A" weakList


// 4.11 - 3. Comparing Lists

// 4.11 - 4. Plus
let plus (list1, list2) =
    let rec plus' list resultlist =
        match list with
        | [] -> resultlist
        | x::xs -> insert resultlist x |> plus' xs 
    plus' list2 list1

 // 4.11 - 5. Minus
let minus (minuendList, subtrahendList) =
    let rec removeItem list itemToRemove resultList =
        match list with
        | [] -> resultList
        | x::[] when x = itemToRemove -> resultList 
        | x::xs when x = itemToRemove -> resultList @ xs
        | x::xs -> resultList @ [x] @ removeItem xs itemToRemove resultList
    let rec removeList listToRemove resultlist =
        match listToRemove with
        | [] -> resultlist
        | x::xs -> removeList xs (removeItem resultlist x [])
    removeList subtrahendList minuendList
