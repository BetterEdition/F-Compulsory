open System

// Learn more about F# at http://fsharp.org

let list1 = [ 3;4;5;12;13;15 ] 
let list2 = [ 1;1;1;1;2;2;2;3;3;3;3;3;4;4;4;4;4;4;5;5;6;6;7;7;7;7;7;7;7;8;8;8;8;8;9;9;9;9;9;10; ]
let number = 3
// 4.11 - 1. Find Occurrences 
let rec count item = function
    | [] -> 0
    | x::[] -> (if x = item then 1 else 0)
    | x::xs -> (if x = item then 1 else 0) + count item xs

// 4.11 - 2. Insert
let rec insert item = function
    | [] -> [item]
    | x::[] -> if item <= x then [item] @ [x] else [x] @ [item]
    | x::xs -> if item <= x then item::[x] @ xs else x::(insert item xs)

// 4.11 - 3. Comparing Lists
let rec isMember num = function
  | [] -> false
  | x :: xs -> if num = x then true elif num < x then false else isMember num xs

let rec compare list2 = function
  | [] -> []
  | x :: xs -> 
      let rest = compare xs list2
      if isMember x list2 then x::rest
      else rest
  

// 4.11 - 4. Plus
let plus (list1, list2) =
    let rec plus' list resultlist = function
      | [] -> resultlist
      | x::xs -> insert resultlist x |> plus'
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
