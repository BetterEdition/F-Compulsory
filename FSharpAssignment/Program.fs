open System

// Learn more about F# at http://fsharp.org

//let list1 = [ 1;1;1;1;2;2] 
//let list2 = [ 1;1;1;1;2;2;2;3;3;3;3;3;4;4;4;4;4;4;5;5;6;6;7;7;7;7;7;7;7;8;8;8;8;8;9;9;9;9;9;10; ]

// 4.11 - 1. Find Occurrences 
let rec count (list:int list, number:int) = 
    match list with
    | [] -> 0
    | x::[] -> (if x number then 1 else 0)
    | x::xs -> (if x = number then 1 else 0) + count (xs,number)

// 4.11 - 2. Insert
let rec insert (list:int list, number:int) = 
    match list with
    | [] -> [number]
    | x::[] -> (if number <= x then [number] @ [x] else [x] @ [number])
    | x::xs -> (if number <= x then number::[x] @ xs else x::(insert (xs, number)))

// 4.11 - 3. Comparing Lists
let rec mem (list:int list, number:int) = 
  match list with
  | [] -> false
  | head :: tail -> 
    if number = head then true else mem (tail, number) 

let rec intersect (list1:int list, list2: int list)  =
    match (list1, list2) with
    | x::xs', y::ys' ->
        if   x = y then x :: intersect (xs', ys')
        elif x < y then intersect (xs', list2)
        else            intersect (list1 , ys')
    | _ -> []
       

// 4.11 - 4. Plus
let rec plus (list1:int list, list2:int list) = 
  match list1,list2 with
  | [],l | l,[] -> l
  | x::xs', y::ys' -> 
     if x < y then x :: (plus (xs', list2))
     else y :: (plus (list1, ys'))

 // 4.11 - 5. Minus
let rec removeItem list itemToRemove resultList =

        match list with
        | [] -> resultList
        | x::[] when x = itemToRemove ->  resultList 
        | x::xs  when x = itemToRemove -> resultList @ xs
        | x::xs -> resultList @ [x] @ removeItem xs itemToRemove resultList 

let rec removeList itemsToBeRemoved listToRemove =
   // printfn "list: %A" listToRemove
    match itemsToBeRemoved with
    | [] -> listToRemove
    | x::xs -> removeList xs (removeItem listToRemove x [])

    