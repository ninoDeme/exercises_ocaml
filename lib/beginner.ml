(* Tail of a List *)
let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: xs -> last xs

(* Last Two Elements of a List  *)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: xs -> last_two xs

(* N'th Element of a List *)
let rec list_nth x y =
  match (x, y) with
  | [], _ -> None
  | _, y when y < 0 -> None
  | x :: _, 0 -> Some x
  | _ :: xs, y -> list_nth xs (y - 1)

(* Length of a List *)
let rec length = function
  | [] -> 0
  | _ :: xs -> length xs

let length' list =
  let rec aux n = function
    | [] -> n
    | _ :: xs -> aux (n + 1) xs
  in
  aux 0 list

(* Reverse a List *)
let rev list =
  let rec aux n = function
    | [] -> n
    | x :: xs -> aux (x :: n) xs
  in
  aux [] list

(* Palindrome *)
let is_palindrome list =
  let rec aux = function
    | [], _ | _, [] -> true
    | x :: _, y :: _ when x != y -> false 
    | _ :: xs, _ :: ys -> aux (xs, ys)
  in
  aux (list, rev list)

(* Duplicate the Elements of a List *)
let duplicate list =
  let rec aux n = function
    | [] -> n
    | x :: xs -> aux (x :: x :: n) xs
  in
  aux [] list |> List.rev

let duplicate' list =
  let rec aux n = function
    | [] -> n
    | x :: xs -> aux (n @ [x; x]) xs
  in
  aux [] list

(* Remove the K'th Element From a List  *)
let remove_at n list =
  let rec aux acc i = function
    | _ :: xs when i = n -> List.rev_append acc xs
    | x :: xs -> aux (x :: acc) (i + 1) xs
    | [] -> list
  in
  aux [] 0 list

(* Insert an Element at a Given Position Into a List *)
let insert_at input n list =
  let rec aux acc i = function
    | x :: xs when i = n -> List.rev_append (x :: input :: acc) xs
    | x :: xs -> aux (x :: acc) (i + 1) xs
    | [] -> List.rev (input :: acc)
  in
  aux [] 0 list

(* Create a List Containing All Integers Within a Given Range *)
let range first last =
  let rec aux acc i =
    if i < first then
      acc
    else
      aux (i :: acc) (i - 1)
  in
  aux [] last

(* Lotto: Draw N Different Random Numbers From the Set 1..M *)
let lotto_select n limit =
  let rec aux acc i =
    if i <= 0 then
      acc
    else
      aux (Random.int limit + 1 :: acc) (i - 1)
  in
  aux [] n

(* Generate a Random Permutation of the Elements of a List *)
let permutation list = 
  let () = Random.init 0
  in
  let extract_at n list =
    let rec aux acc i = function
      | x :: xs when i = n -> (x, List.rev_append acc xs)
      | x :: xs -> aux (x :: acc) (i + 1) xs
      | [] -> failwith "unreachable"
    in
    aux [] 0 list
  in
  let rec aux acc list =
    let random = Random.int (List.length list)
    in
    match list with
    | [] -> acc
    | x -> match extract_at random x with n, v -> aux (n :: acc) v
  in
  aux [] list

