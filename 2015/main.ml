(*Module dependencies : Core, Re2*)

module Day1 = struct
  let file = "input/day1-input.txt"

  let file_to_num file =
    let ic = open_in file in
    let rec aux acc =
      try
        match (input_char ic) with
        | '(' -> aux (acc+1)
        | ')' -> aux (acc-1)
        | _ -> aux acc
      with End_of_file -> close_in ic; acc
    in
    aux 0

  let find_pos file =
    let ic = open_in file in
    let rec aux floor pos =
      try
        match (input_char ic),floor with
        | ')',0 -> pos+1
        | '(',_ -> aux (floor+1) (pos+1)
        | ')',_ -> aux (floor-1) (pos+1)
        | _ -> aux floor pos
      with End_of_file -> close_in ic; -1
    in
    aux 0 0


  let main () = (file_to_num file, find_pos file)
end

module Day2 = struct

  open Core

  let box l b h =
    let x = l*b in
    let y = b*h in
    let z = l*h in

    let m = min (min x y) z in
    2*(x+y+z)+m

  let ribbon l b h =
    let x = 2*(l+b) in
    let y = 2*(b+h) in
    let z = 2*(h+l) in
    let m = min (min x y) z in
    m+(l*b*h)

  let calc_total calc =
    let file = In_channel.create "input/day2-input.txt" in
    In_channel.fold_lines
      file
      ~init:0
      ~f:(fun acc line ->
          match String.split line 'x' with
            l::b::h::[] -> acc+(calc (int_of_string l)(int_of_string b)(int_of_string h))
          | _ -> assert false
        )

  let main () = (calc_total box),(calc_total ribbon)

end

module Day3 = struct
  let grid1 = Hashtbl.create 64

  let grid2 = Hashtbl.create 64

  let get_char_list () =
    let file = Core.In_channel.create "input/day3-input.txt" in
    let rec aux acc = match (Core.In_channel.input_char file) with
      | Some c -> aux (c::acc)
      | _ -> List.rev acc
    in
    aux []

  let add x table pos=
    let aux pos =
      try
        let count = Hashtbl.find table pos in
        Hashtbl.replace table pos (count+1); pos
      with Not_found -> Hashtbl.add table pos 1; pos
    in
    match x with
    | '^' ->
      let new_pos = ((fst pos),((snd pos)+1)) in
      aux new_pos
    | 'v' ->
      let new_pos = ((fst pos),((snd pos)-1)) in
      aux new_pos
    | '<' ->
      let new_pos = (((fst pos)-1),snd pos) in
      aux new_pos
    | '>' ->
      let new_pos = (((fst pos)+1),snd pos) in
      aux new_pos
    | _ -> assert false

  let populate_santa grid =
    let _ = Hashtbl.add grid (0,0) 1 in
    let char_list = get_char_list () in
    let rec aux l pos = match l with
      | x :: xs ->
        let new_pos = add x grid pos in
        aux xs new_pos
      | [] -> pos
    in
    aux char_list (0,0)

  let populate_robosanta grid =
    let _ = Hashtbl.add grid (0,0) 1 in
    let char_list = get_char_list () in
    let rec aux l pos1 pos2 = match l with
      | x1 :: x2 :: xs ->
        let new_pos1,new_pos2 = (add x1 grid pos1,add x2 grid pos2) in
        aux xs new_pos1 new_pos2
      | x :: [] -> (add x grid pos1, pos2)
      | [] -> (pos1,pos2)
    in
    aux char_list (0,0) (0,0)



  let main () =
    let _ = populate_santa grid1 in
    let _ = populate_robosanta grid2 in
    (Hashtbl.length grid1, Hashtbl.length grid2)

end

module Day4 = struct

  let md5 s = Digest.to_hex (Digest.string s)

  let key = "iwrupvqb"

  let rec find acc x=
    let s = key ^ string_of_int acc in
    if String.sub (md5 s) 0 (String.length x) = x then
      acc
    else
      find (acc+1) x

  let main () = (find 0 "00000",find 0 "000000")

end

module Day5 = struct

  let get_pairs str =
    let cl = Base.String.to_list str in
    let rec aux l = match l with
      | x1 :: x2 :: xs -> (x1,x2) :: aux (x2::xs)
      | _ -> []
    in
    aux cl

  let nice1 str =
    let pairs = get_pairs str in
    List.exists (fun (x,y) -> x=y) pairs &&
    (not (List.exists (fun x ->x = ('a','b') || x = ('c','d') || x = ('p','q') || x = ('x','y') ) pairs)) &&
    (Base.String.count str (fun x -> x = 'a' || x = 'e' || x = 'i' || x = 'o' || x = 'u')  > 2)

  let nice2 str =
    let pairs = get_pairs str in
    let rec aux1 l = match l with
      | x1 :: x2 :: xs -> List.mem x1 xs || aux1 (x2::xs)
      | _ -> false
    in
    let rec aux2 l = match l with
      | (x1,x2)::(x2',x3)::xs -> x1=x3 || aux2 ((x2,x3)::xs)
      | _ -> false
    in

   (aux1 pairs) && (aux2 pairs)


  let check_file valid =
    let file = Core.In_channel.create "input/day5-input.txt" in
    Core.In_channel.fold_lines
      file
      ~init:0
      ~f:(fun acc line ->
          if (valid line) then
            (acc+1)
          else
            acc
        )

  let main () = (check_file nice1,check_file nice2)

end

module Day6 = struct

  let grid1 = Array.make_matrix 1000 1000 false

  let grid2 = Array.make_matrix 1000 1000 0

  type instruction = TurnOn | TurnOff | Toggle

  let parse str =
    let r_cord = Re2.of_string "(\\d{1,3}),(\\d{1,3})" in
    let r_on = Re2.of_string "^turn on" in
    let r_off = Re2.of_string "^turn off" in
    let i =
      if Re2.matches r_on str then
        TurnOn
      else if Re2.matches r_off str then
        TurnOff
      else
        Toggle
    in
    let (p1,p2) =
      let l =
        Re2.find_all r_cord str
        |> (fun x -> match x with Core_kernel__.Result.Ok x -> x | _ -> assert false)
        |> List.map (fun x -> String.split_on_char ',' x)
      in
      match l with [p1;p2] -> (match p1,p2 with (x1::y1::[],x2::y2::[]) -> ((x1,y1),(x2,y2)) | _ -> assert false) | _ -> assert false
    in
    i,p1,p2

  let update1 inst count x y = match inst with
    | TurnOn ->  count := !count+1-Bool.to_int (grid1.(x).(y)); grid1.(x).(y) <- true
    | TurnOff -> count := !count-Bool.to_int (grid1.(x).(y)); grid1.(x).(y) <- false
    | Toggle -> count := !count+1-2*Bool.to_int (grid1.(x).(y)); grid1.(x).(y) <- not (grid1.(x).(y))

  let update2 inst count x y = match inst with
    | TurnOn ->  count := !count+1; grid2.(x).(y) <- 1+grid2.(x).(y)
    | TurnOff ->  count := !count + (max (-1) (-1*grid2.(x).(y))); grid2.(x).(y) <- (max 0 (-1+grid2.(x).(y)))
    | Toggle ->  count := !count+2; grid2.(x).(y) <- 2+grid2.(x).(y)

  let perform update acc (inst,(x1,y1),(x2,y2)) =
    let count = ref acc in
    let x1 = int_of_string x1 in
    let y1 = int_of_string y1 in
    let x2 = int_of_string x2 in
    let y2 = int_of_string y2 in
    for x = x1 to x2 do
      for y = y1 to y2 do
        update inst count x y
      done
    done ;!count

  let perform_all update =
    let file = Core.In_channel.create "input/day6-input.txt" in
    Core.In_channel.fold_lines
      file
      ~init:0
      ~f:(fun acc line -> parse line |> perform update acc)

  let main () = (perform_all update1,perform_all update2)

end

module Day7 = struct

  open Int

  module Signals = Map.Make(String)

  type primop = And | Or | Not | Lshift | Rshift

  type exp = Val of int | Wire of string | Primop of primop * exp list

  exception BadArgs

  let parse acc line=
    let exp_list = String.split_on_char ' ' line in
    let op =
      if Re2.matches (Re2.of_string "AND") line then
        Some And
      else if Re2.matches (Re2.of_string "OR") line then
        Some Or
      else if Re2.matches (Re2.of_string "NOT") line then
        Some Not
      else if Re2.matches (Re2.of_string "LSHIFT") line then
        Some Lshift
      else if Re2.matches (Re2.of_string "RSHIFT") line then
        Some Rshift
      else
        None
    in
    let parse_exp exp =
      if Re2.matches (Re2.of_string "\\d+") exp then
        Val (int_of_string exp)
      else
        Wire exp
    in

    let e1,e2,id = match op with
      | Some And | Some Or ->
        let e1',e2',id = (List.nth exp_list 0),(List.nth exp_list 2),(List.nth exp_list 4) in
        let e1 = parse_exp e1' in
        let e2 = parse_exp e2' in
        Some e1,Some e2,id

      | Some Lshift | Some Rshift ->
        let e1',e2,id = (List.nth exp_list 0),Val (int_of_string (List.nth exp_list 2)),(List.nth exp_list 4) in
        let e1 = parse_exp e1' in
        Some e1,Some e2,id

      | Some Not ->
        let e1',id = (List.nth exp_list 1),(List.nth exp_list 3) in
        let e1 = parse_exp e1' in
        Some e1,None,id

      | None ->
        let e1',id = (List.nth exp_list 0),(List.nth exp_list 2)in
        let e1 = parse_exp e1' in
        Some e1,None,id
    in

    match op,e1,e2 with
    | Some And, Some e1, Some e2 -> Signals.add id (Primop (And,[e1;e2])) acc
    | Some Or, Some e1, Some e2 -> Signals.add id (Primop (Or,[e1;e2])) acc
    | Some Lshift, Some e1, Some e2 -> Signals.add id (Primop (Lshift,[e1;e2])) acc
    | Some Rshift, Some e1, Some e2 -> Signals.add id (Primop (Rshift,[e1;e2])) acc
    | Some Not, Some e1, None -> Signals.add id (Primop (Not,[e1])) acc
    | None, Some e1, None -> Signals.add id e1 acc
    | _ -> assert false

  let build_env () =
    let file = Core.In_channel.create "input/day7-input.txt" in
    Core.In_channel.fold_lines
      file
      ~init:Signals.empty
      ~f:parse

  let eval_primop op arg_l = match op,arg_l with
    | And,[a1;a2] -> logand (logand a1 a2) 0xFFFF
    | Or, [a1;a2] -> logand (logor a1 a2) 0xFFFF
    | Not, [a1] -> logand (lognot a1) 0xFFFF
    | Lshift, [a1;a2] -> logand (shift_left a1 a2) 0xFFFF
    | Rshift, [a1;a2] -> logand (shift_right a1 a2) 0xFFFF
    | _ -> raise BadArgs

  let memo = Core.String.Table.create()

  let rec eval env exp = match exp with
    | Val i -> i
    | Wire id -> Core.String.Table.find_or_add memo id ~default:(fun () -> eval env (Signals.find id env))
    | Primop (op,exp_l) -> let arg_l = List.map (eval env) exp_l in eval_primop op arg_l

  let mapping = build_env ()

  let main () =
    let p1 = eval mapping (Wire "a") in
    let _ = Core.String.Table.clear memo in
    let mapping = Signals.add "b" (Val p1) mapping in
    let p2 = eval mapping (Wire "a") in
    p1,p2

end

module Day8 = struct

  open Core

  let decode l =
    let rec aux l acc = match l with
      | '\\'::'x'::_::_ :: ls-> aux ls (acc+1)
      | '\\'::_::ls -> aux ls (acc+1)
      | _::ls -> aux ls (acc+1)
      | [] -> (acc-2)
    in
    aux l 0

  let encode l =
    let rec aux l acc = match l with
      | '\"'::ls -> aux ls (acc+2)
      | '\\'::ls -> aux ls (acc+2)
      | _::ls -> aux ls (acc+1)
      | [] -> (acc+2)
    in
    aux l 0

  let parse f =
    let file = In_channel.create "input/day8-input.txt" in
    In_channel.fold_lines
      file
      ~init:0
      ~f:(fun acc line ->
          let num_code = (String.length line) in
          num_code - f (String.to_list line)+acc
        )

  let main () = (parse decode,- parse encode)

end

module Day9 = struct

  module CM = Map.Make(String)

  let parse () =
    let update_aux city distance x = match x with
      | Some v -> Some ((city,distance)::v)
      | None -> Some ((city,distance)::[])
    in
    let file = Core.In_channel.create "input/day9-input.txt" in
    Core.In_channel.fold_lines
      file
      ~init:CM.empty
      ~f:(fun acc line ->
          let sl = String.split_on_char ' ' line in
          let city1,city2,distance = (List.nth sl 0),(List.nth sl 2),int_of_string(List.nth sl 4) in
          let acc = CM.update city1 (update_aux city2 distance) acc in
          CM.update city2 (update_aux city1 distance) acc
        )

  let graph = parse ()

  let cities =
    let rec aux l = match l with
      | (k,v)::ls -> k :: (aux ls)
      | [] -> []
    in
    aux (CM.bindings graph)

  let dist path =
    let dist2 x y =
      List.assoc y (CM.find x graph)
    in
    let rec aux path acc = match path with
      | x :: y :: xs -> aux (y::xs) ((dist2 x y)+acc)
      |( _ :: []) | [] -> acc
    in
    aux path 0

  let rec permutations = function
    | [] -> []
    | x::[] -> [[x]]
    | l ->
      List.fold_left
        (fun acc x -> acc @ List.map (fun p -> x::p) (permutations (List.filter ((<>) x) l)))
        []
        l

  let find fpick acc=
    List.fold_left
      (fun acc x -> fpick acc (dist x))
      acc
      (permutations cities)

  let main () = (find min Int.max_int, find max Int.min_int)

end

module Day10 = struct

  let input = [1;1;1;3;1;2;2;1;1;3]

  type group =
    {
      rep : int;
      count : int ref
    }

  let fresh n = {rep = n; count = ref 1}

  let parse seq =
    let rec aux seq cur acc = match seq with
      | x :: xs when x = cur -> incr (List.hd acc).count; aux xs cur acc
      | x :: xs -> aux xs x ((fresh x)::acc)
      | [] -> List.rev acc
    in
    aux seq (-1) []

  let gen gl =
    let rec aux gl acc = match gl with
      | x :: xs -> aux xs ((x.rep)::!(x.count)::acc)
      | [] -> List.rev acc
    in
    aux gl []

  let compute n initial =
    let cur = ref initial in
    for i = 0 to (n-1) do
      cur := (!cur) |> parse |> gen;
    done;
    !cur

  let main () =
    let a1 = (input |> compute 40) in
    let a2 = (a1 |> compute 10) in
    (a1 |> List.length),(a2 |> List.length)

end

module Day11 = struct

  let input = "cqjxjnds"

  exception OutOfBounds

  let increment cl =
    let rec aux cl acc = match cl with
      | x :: xs when (Char.code x < 122) -> List.rev xs @ [Char.chr (1+Char.code x)] @ acc
      | x :: xs when (Char.code x = 122)-> aux xs (acc@['a'])
      | [] -> raise (OutOfBounds)
      | _ -> assert false
    in
    aux (List.rev cl) []

  let satisfy cl =
    let rec check1 cl = match cl with
      | x :: y :: z :: tl -> ((1+Char.code x = Char.code y ) && (1+Char.code y = Char.code z)) || check1 (y::z::tl)
      | _ -> false
    in
    let rec check2 cl = match cl with
      | hd :: tl when (hd = 'i' || hd = 'o' || hd = 'l') -> false
      | hd :: tl -> true && check2 tl
      | [] -> true
    in
    let check3 cl =
      let rec aux cl acc = match cl with
        | x :: y :: tl when (x = y && not(List.mem x acc)) -> aux tl (x::acc)
        | x :: y :: tl -> aux (y::tl) acc
        | _::[] | [] -> (List.length acc) >= 2
      in
      aux cl []
    in
    (check1 cl) && (check2 cl) && (check3 cl)

  let next_pswd pswd =
    let pswd = increment (Core.String.to_list pswd) in
    let rec aux cur = match (satisfy cur) with
      | true -> cur
      | false -> aux (increment cur)
    in
    aux pswd

  let main () =
    let first = input |> next_pswd |> Core.String.of_char_list in
    (first, first |> next_pswd |> Core.String.of_char_list)

end

module Day12 = struct

  let file = "input/day12-input.json"

  let count_all () =
    let r = Re2.create_exn "-?\\d{1,}" in
    Core.In_channel.read_all file
    |> Re2.find_all_exn r
    |> List.fold_left (fun acc x ->acc+int_of_string x ) 0

  let count_all_alt () =
    let json = Core.In_channel.read_all file |> Yojson.Basic.from_string in
    let rec aux acc jtree = match jtree with
      | `Int i -> acc+i
      | `List l -> List.fold_left aux acc l
      | `Assoc l -> List.fold_left aux acc (snd (List.split l))
      | _ -> acc
    in
    aux 0 json

  exception Red

  let count_not_red () =
    let json = Core.In_channel.read_all file |> Yojson.Basic.from_string in
    let rec aux acc jtree = match jtree with
      | `Int i -> acc+i
      | `List l -> List.fold_left aux acc l
      | `Assoc l ->
        let cur_acc = acc in
        begin
          try
            List.fold_left
              (fun acc x ->
                 match x with
                 | `String s when s = "red" -> raise Red
                 | _ -> aux acc x
              )
              acc
              (snd (List.split l))
          with Red -> cur_acc
        end
      | _ -> acc
    in
    aux 0 json

  let main () = (count_all (), count_not_red ())

end

