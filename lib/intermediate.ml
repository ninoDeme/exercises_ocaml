(* Flatten a List *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten tree = 
  let aux = function
    | One x -> [x]
    | Many x -> flatten x
  in
  List.map aux tree |> List.flatten

let rec flatten' = 
  let aux = function
    | One x -> [x]
    | Many x -> flatten' x
  in
  function
  | [] -> []
  | x :: xs -> (aux x) @ (flatten' xs)

let flatten'' tree = 
  let rec aux acc = function
      | [] -> acc
      | One y :: xs -> aux (y :: acc) xs
      | Many y :: xs -> aux (aux acc y) xs
  in
  aux [] tree |> List.rev

let flatten_tcr tree = 
  let rec aux acc stack list = match stack, list with
      | [], [] -> acc
      | x :: xs, [] -> aux acc xs x
      | stack, One y :: xs -> aux (y :: acc) stack xs
      | stack, Many y :: xs -> aux acc (xs :: stack) y 
  in
  aux [] [] tree |> List.rev

(* Eliminate Duplicates *)
let compress list = 
  let rec aux acc list = match (acc, list) with 
    | acc, [] -> acc
    | acc_hd :: _ as acc, x :: xs when acc_hd = x -> aux acc xs
    | acc, x :: xs -> aux (x :: acc) xs
  in
  aux [] list |> List.rev

(* Pack Consecutive Duplicates *)
let pack list = 
  let rec aux acc list = match (acc, list) with
    | (acc, []) -> acc
    | ((f :: _) as t :: ts, x :: xs) when f = x -> aux ((x :: t) :: ts) xs
    | (acc, x :: xs) -> aux ([x] :: acc) xs
  in
  aux [] list |> List.rev

(* Run-Length Encoding *)
let encode list = 
  let rec aux = function
    | (acc, []) -> acc
    | ((n, f) :: acc, x :: xs) when f = x -> aux ((n + 1, f) :: acc, xs)
    | (acc, x :: xs) -> aux ((1, x) :: acc, xs)
  in
  aux ([], list) |> List.rev

(* Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode_rle list = 
  let rec aux acc list = match (acc, list) with
    | (acc, []) -> acc
    | (Many (n, f) :: acc, x :: xs) when f = x -> aux (Many (n + 1, f) :: acc) xs
    | (One f :: acc, x :: xs) when f = x -> aux (Many (2, f) :: acc) xs
    | (acc, x :: xs) -> aux (One x :: acc) xs
  in
  aux [] list |> List.rev

(* Decode a Run-Length Encoded List *)
let decode_rle list = 
  let rec aux acc list = match (acc, list) with
    | (acc, []) -> acc
    | (acc, One f :: xs) | (acc, Many (1, f) :: xs) -> aux (f :: acc) xs
    | (acc, (Many (n, f) :: xs)) -> aux (f :: acc) (Many (n - 1, f):: xs)
  in
  aux [] list |> List.rev

let decode_rle_2 list = 
  let rec unpack_rle = function
    | One f | Many (1, f) -> [f]
    | Many (n, f) -> f :: (unpack_rle (Many (n - 1, f)))
  in
  let rec aux acc list = match (acc, list) with
    | (acc, []) -> acc
    | (acc, x :: xs) -> aux (unpack_rle x :: acc) xs
  in
  aux [] list |> List.rev

(* Replicate the Elements of a List a Given Number of Times *)
let replicate list number_reps = 
  let rec list_fill acc f = function
    | 0 -> acc
    | n -> list_fill (f :: acc) f (n - 1)
  in
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (list_fill acc x number_reps) xs
  in
  aux [] list

(* Drop Every N'th Element From a List *)
let drop list n = 
  let rec aux acc i = function
    | [] -> acc
    | _ :: xs when i = 1 -> aux acc n xs
    | x :: xs -> aux (x :: acc) (i - 1) xs
  in
  aux [] n list |> List.rev

(* Split a List Into Two Parts; The Length of the First Part Is Given *)
let split list n = 
  let rec aux acc i = function
    | x :: xs when i > 0 -> aux (x :: acc) (i - 1) xs
    | list -> (List.rev acc, list)
  in
  if n >= 0 then aux [] n list else ([], list)

let split' list n = 
  let rec aux acc i = function
    | x :: xs when i < n -> aux (x :: acc) (i + 1) xs
    | list -> (List.rev acc, list)
  in
  if n >= 0 then aux [] 0 list else ([], list)

(* Extract a Slice From a List *)
let slice list first last = 
  let rec take_n acc i = function
    | x :: xs when i > 1 -> take_n (x :: acc) (i - 1) xs
    | _ -> List.rev acc 
  in
  let rec drop_n i = function
    | _ :: xs when i > 0 -> drop_n (i - 1) xs
    | list -> take_n [] last list
  in
  drop_n first list

(* Rotate a List N Places to the Left *)
let rotate list i = 
  let rec aux acc i = function
    | x :: xs when i > 0 -> aux (x :: acc) (i - 1) xs
    | list -> list @ List.rev acc
  in
  if i >= 0 then aux [] i list else list

(* Extract a Given Number of Randomly Selected Elements From a List *)
let rand_select list n = 
  let () = Random.init 0
  in
  let list_length = List.length list
  in
  let rec get n = function
    | [] -> failwith "unreacheable"
    | x :: xs -> if n <= 0 then x else get (n - 1) xs
  in
  let rec aux acc i =
    let random = Random.int list_length
    in
    if i = n then acc else aux ((get random list) :: acc) (i + 1)
  in
  aux [] 0

(* Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List *)
let rec extract max list = 
  let rec combine acc stack i list = match list with
    | _ when i = 0 -> (List.rev stack) :: acc
    | [] -> acc
    | h :: t -> combine (combine acc (h :: stack) (i - 1) t) stack i t
  in
  combine [] [] (max) list |> List.rev


let enumerate list = 
  let rec aux acc i = function
    | [] -> acc
    | h :: t -> aux ((i, h) :: acc) (i + 1) t
  in
  aux [] 0 list |> List.rev

(* Group the Elements of a Set Into Disjoint Subsets *)
let group l1 l2 = 
  let remove_at i list =
    let rec aux acc = function
      | (n, _) :: xs when i = n -> List.rev_append acc xs
      | x :: xs -> aux (x :: acc) xs
      | [] -> list
    in
    aux [] list
  in
  let rec filter_l1 l1 = function
    | [] -> []
    | (n, _) :: t -> filter_l1 (remove_at n l1) t
  in
  let rec get_combs acc l1 l2 = match l2 with
    | [] -> acc
    | h :: t ->
      let temp = extract h l1 in
      List.flatten (List.map (fun (x) -> (get_combs (List.rev_append temp acc) (filter_l1 l1 x) t)) temp)
  in
  get_combs [] (enumerate l1) l2


