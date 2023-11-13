module rec R : sig
  module type Set = sig
    val union : (module R.Set) -> (module R.Set)
    val contains : int -> bool
    val inspections : unit -> int
  end
end = R

module type Set = R.Set

let base_set cond = (module struct
  let i = ref 0
  let inspections _ = !i
  let contains x = (i := !i + 1; cond x)
  let union other = failwith "abstract"
end : Set)

let empty () = (module struct
  let prototype = base_set (fun c -> false)
  module P = (val prototype)
  let union other = other
  let contains = P.contains
  let inspections = P.inspections
end : Set)

let rec insert x (module B : Set) = (module struct
  let prototype = base_set (fun y -> x == y || B.contains y)
  module P = (val prototype)
  let union other = insert x (B.union other)
  let contains = P.contains
  let inspections = P.inspections
end : Set)

let s1 = insert 3 (empty ())
let s2 = insert 4 s1
let s3 = empty ()

let sets = [s1; s2; s3]

let test_one (module S : Set) = S.contains 4

let test_all : (module Set) list -> bool list =
  fun xs -> List.map (fun (module S : Set) -> S.contains 4) xs

let test_all2 : (module Set) list -> bool list =
  fun xs -> List.map test_one xs

let sum_inspections : (module Set) list -> int =
  fun xs -> List.fold_right (fun (module S : Set) acc -> S.inspections () + acc) xs 0

let _ = print_endline (string_of_int (sum_inspections sets))
let _ = List.map print_endline (List.map string_of_bool (test_all sets))
let _ = print_endline (string_of_int (sum_inspections sets))
let _ = List.map print_endline (List.map string_of_bool (test_all sets))
let _ = print_endline (string_of_int (sum_inspections sets))
