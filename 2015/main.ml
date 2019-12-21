module Day1 = struct
  let file = "day1-input.txt"

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
    let file = In_channel.create "day2-input.txt" in
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
    let file = Core.In_channel.create "day3-input.txt" in
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
    let file = Core.In_channel.create "day5-input.txt" in
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

