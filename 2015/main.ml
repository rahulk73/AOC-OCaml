open Core

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

