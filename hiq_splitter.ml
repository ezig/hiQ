open Hiq_ast
open Hiq_compile 

type mapping = (int * int) list

let parse_file () =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv > 4
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse] [low?] [high?]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Hiq_parse.program Hiq_lex.lexer (Lexing.from_channel ch)

let count_gates ({prog; _}) : int =
    let rec aux s =
        match s with
        | Exp e -> 1
        | Seq (s1, s2) -> (aux s1) + (aux s2)
        | If (e, s1, s2o) ->
            match s2o with
            | None -> aux s1
            | Some s2 -> (aux s1) + (aux s2)
    in aux prog

let split ({prog; _} as p) (start : int) (e : int) : program =
    let rec flatten_stmt s =
        match s with
        | Exp e -> [e]
        | Seq (s1, s2) -> (flatten_stmt s1) @ (flatten_stmt s2)
        | _ -> failwith "Can't flatten if"
    in
    let unflatten_stmt elst =
        match elst with
        | h :: t -> List.fold_left (fun acc e -> Seq (acc, Exp e)) (Exp h) t
        | [] -> failwith "can't reduce empty list"
    in
    let rec take l n =
        if n = 0 then [] else
        match l with
        | [] -> failwith "empty"
        | hd :: tl -> hd :: (take tl (n - 1)) in
    let rec drop l n =
        if n = 0 then l else
        match l with
        | [] -> failwith "empty"
        | hd :: tl -> (drop tl (n - 1)) in
    let sublist l s e = take (drop l s) (e - s + 1) in
    {p with prog = unflatten_stmt (sublist (flatten_stmt prog) start e)}

let _ =
    let prog = parse_file () in
    if Array.length Sys.argv = 2 then Printf.printf "%d\n" (count_gates prog)
    else let split_prog =
        split prog (int_of_string Sys.argv.(2)) (int_of_string Sys.argv.(3)) in
        Printf.printf "%s" (ast_to_string split_prog)
