(******************************************************************************)

type 'a arg =
  | S of 'a
  | L of 'a Lazy.t

let l x = L x
let s x = S x
let force = Lazy.force

module rec R : sig
  module type Obj = sig
    val handle :
      (module R.Obj) -> string * (module R.Obj) arg list -> (module R.Obj)
  end
end = R

module type Obj = R.Obj
type obj = (module Obj)

(* Dynamic dispatch *)
let ( * ) : obj -> string -> obj arg list -> obj =
  fun o head args ->
    let (module O : Obj) = o in
    O.handle o (head, args)

(* Message arguments *)
let ( / ) : ('a -> 'b) -> 'a -> 'b =
  fun f x -> f x

(******************************************************************************)

let base = (module struct
  let handle self = function
    | "if", [yes; no] -> self*"truthy"/[]*"if"/[yes; no]
    | "or", [otherwise] -> self*"if"/[l@@lazy self; otherwise]
    | msg, args ->
        failwith
          ( "Unimplemented: " ^ msg
          ^ "/["
          ^ ( args
              |> List.map (fun a -> match a with S _ -> "S" | L _ -> "L")
              |> String.concat ";"
            )
          ^ "]"
          )
end : Obj)

let rec nil () = (module struct
  let handle _ = function
    | "toString", [] -> string "nil"
    | "truthy", [] -> ff ()
    | _ -> nil ()
end : Obj)

and string s = (module struct
  module Super = (val base : Obj)

  let handle self = function
    | "toString", [] -> self
    | "print", [] -> print_endline s; self
    | "truthy", [] -> if String.length s = 0 then ff () else tt ()
    | msg ->  Super.handle self msg
end : Obj)

and tt () = (module struct
  module Super = (val base : Obj)

  let handle self = function
    | "toString", [] -> string "true"
    | "not", [] -> ff ()
    | "if", [L yes; _] -> force yes
    | "truthy", [] -> tt ()
    | msg ->  Super.handle self msg
end : Obj)

and ff () = (module struct
  module Super = (val base : Obj)

  let handle self = function
    | "toString", [] -> string "false";
    | "not", [] -> tt ()
    | "if", [_; L no] -> force no
    | "truthy", [] -> ff ()
    | msg ->  Super.handle self msg
end : Obj)

let rec int = (module struct
  module Super = (val base : Obj)

  let handle self = function
    | "equals", [S y] ->
        self*"isZero"/[]*"if"/
          [ l@@lazy (y*"isZero"/[])
          ; l@@lazy (self*"isPositive"/[]*"if"/
              [ l@@lazy (self*"pred"/[]*"equals"/[s@@ y*"pred"/[]])
              ; l@@lazy (self*"negate"/[]*"equals"/[s@@ y*"negate"/[]])
              ])
          ]
    | "add", [S y] ->
        self*"isZero"/[]*"if"/
          [ l@@lazy y
          ; l@@lazy (self*"isPositive"/[]*"if"/
              [ l@@lazy (self*"pred"/[]*"add"/[s@@ y*"succ"/[]])
              ; l@@lazy (self*"negate"/[]*"add"/[s@@ y*"negate"/[]]*"negate"/[])
              ])
          ]
    | msg -> Super.handle self msg
end : Obj)

let rec ocaml_int x = (module struct
  module Super = (val int : Obj)

  let handle self = function
    | "toString", [] -> string (string_of_int x)
    | "isZero", [] -> if x == 0 then tt () else ff ()
    | "isPositive", [] -> if x > 0 then tt () else ff ()
    | "pred", [] -> ocaml_int (x - 1)
    | "succ", [] -> ocaml_int (x + 1)
    | "negate", [] -> ocaml_int (-x)
    | msg -> Super.handle self msg
end : Obj)

let rec psucc p = (module struct
  module Super = (val int : Obj)

  let handle self = function
    | "toString", [] -> string "S(?)" (* (string "S")*"append"/[s@@ p*"toString"/[]] *)
    | "isZero", [] -> ff ()
    | "isPositive", [] -> tt ()
    | "pred", [] -> p
    | "succ", [] -> p*"succ"/[]*"succ"/[]
    | "negate", [] -> nil ()
    | msg -> Super.handle self msg
end : Obj)

let pzero () = (module struct
  module Super = (val int : Obj)

  let handle self = function
    | "toString", [] -> string "Z"
    | "isZero", [] -> tt ()
    | "isPositive", [] -> ff ()
    | "pred", [] -> nil ()
    | "succ", [] -> psucc self
    | "negate", [] -> self
    | msg -> Super.handle self msg
end : Obj)

let empty () = (module struct
  module Super = (val base : Obj)

  let inspection_count : obj ref = ref (ocaml_int 0)

  let handle self = function
    | "contains", [_] ->
        inspection_count := !inspection_count*"succ"/[];
        ff ()
    | "union", [S other] ->
        other
    | "inspections", [] ->
        !inspection_count
    | "truthy", [] -> ff ()
    | msg -> Super.handle self msg
end : Obj)

let rec insert x inner = (module struct
  module Super = (val empty () : Obj)

  let handle self = function
    | "contains", [S y] as msg ->
        ignore (Super.handle self msg);
        x*"equals"/[S y]*"or"/[l@@lazy (inner*"contains"/[s@@ y])]
    | "union", [other] ->
        insert x (inner*"union"/[other])
    | "truthy", [] -> tt ()
    | msg -> Super.handle self msg
end : Obj)

let s1 = insert (ocaml_int 3) (empty ())
let s2 = insert (psucc (psucc (psucc (psucc (pzero ()))))) s1
let s3 = empty ()

let sets = [s1; s2; s3]

let test_one x = x*"contains"/[s@@ ocaml_int 4]

let test_all : obj list -> obj list =
  fun xs -> List.map test_one xs

let sum_inspections : obj list -> obj =
  fun xs ->
    List.fold_right
      (fun x acc -> (x*"inspections"/[]*"add"/[s@@ acc]))
      xs (ocaml_int 0)

let _ = (sets |> sum_inspections)*"toString"/[]*"print"/[]
let _ = sets |> test_all |> List.map (fun x -> x*"toString"/[]*"print"/[])
let _ = (sets |> sum_inspections)*"toString"/[]*"print"/[]
let _ = sets |> test_all |> List.map (fun x -> x*"toString"/[]*"print"/[])
let _ = (sets |> sum_inspections)*"toString"/[]*"print"/[]
