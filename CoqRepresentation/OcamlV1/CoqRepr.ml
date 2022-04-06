(*
ocamlc -g -c CoqRepr.ml
ocamlc -o CoqRepr str.cma CoqRepr.cmo
*)

let read_whole_file filename =
  let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
      close_in ch;
      s;;

let rec read_body1 s l =
  try
    let pattern1 = Str.regexp {|\(Definition\|Theorem\|Corollary\|Lemma\|Global Instance\)[^][]*\(Defined\|Qed\)|} in
      let i = Str.search_forward pattern1 s 0 in  
        let pattern2 = Str.regexp {|Defined\|Qed|} in
          let j = Str.search_forward pattern2 s i in 
            let t = String.sub s i (j - i) in
              let w = String.sub s j ( (String.length s) - j ) in
                  read_body1 w (t::l) 
  with Not_found -> l;;

let clean_thm3 s = let pattern3 = Str.regexp {|[@][^][]*|} in 
  Str.replace_first pattern3 "" s;;

let clean_thm2 s = let pattern2 = Str.regexp {|[ ][^][]*|} in 
clean_thm3 (Str.replace_first pattern2 "" s);;

let clean_thm s = let pattern1 = Str.regexp {|\(Definition\|Theorem\|Corollary\|Lemma\|Global Instance\)[ ]|} in 
  clean_thm2 (Str.replace_first pattern1 "" s);;

let rec clean_proof3 s = let pattern3 = Str.regexp {|[^][]*[.][.]|} in
    if Str.string_match pattern3 s 0 then
      let pattern3A = Str.regexp {|[.][.]|} in
        clean_proof3 (Str.global_replace pattern3A "." s)
    else 
      s;;

let clean_proof2 s = let pattern2 = Str.regexp {|[

  ]\|[(]\|[)]\|[{]\|[}]\|[\[]\|[\]]\|[:]\|[;]\|[,]\|[ ]\|[\\]\|[/]|} in 
  clean_proof3(Str.global_replace pattern2 "." s);;

let clean_proof s = let pattern1 = Str.regexp {|Proof[.]|} in 
  clean_proof2(Str.replace_first pattern1 "" s)

let clean_name s = let pattern1 = Str.regexp {|[^][]*HoTT|} in
  Str.replace_first pattern1 "HoTT" s;;

let thm_proof s input_file = try let pattern1 = Str.regexp {|Proof[^][]*[.]|} in
  let i = Str.search_forward pattern1 s 0 in
    (clean_thm ( String.sub s 0 i ), clean_proof ( String.sub s i ((String.length s) - i) ), clean_name(input_file))
with Not_found -> ("", s, clean_name(input_file));;

let read_body s input_file = List.map (fun x -> thm_proof x input_file) ( read_body1 s [] )

let rec print_list oc = function
| x::xs -> begin 
            let (u, v, w) = x in
              let s = u ^ "," ^ v ^ "," ^ w in
                Printf.fprintf oc "%s\n" s;
              print_list oc xs
          end 
| _ -> Printf.fprintf oc "";;

let dir_contents dir =
  let rec loop result = function
    | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop result
    | f::fs -> loop (f::result) fs
    | []    -> result
   in
    loop [] [dir];;

let v_ext_file s = let n = String.length s in 
   if n >= 2 then
    (s.[n-1] = 'v')&&(s.[n-2] = '.')
   else 
    false;;

let new_name s = let pattern1 = Str.regexp {|[/][^][]*[.]v|} in 
    let n = String.length s in
      let i = Str.search_backward pattern1 s n in
        "Output/" ^ (String.sub s i ( n - i - 2)) ^ ".csv" ;;

let clean_file s = try let pattern1 = Str.regexp {|[(][*][^][]*\(![(][*]\)[^][]*[*][)]|} in 
    Str.global_replace pattern1 "" s
   with Not_found -> s;;

let rec process_file n = function
    | input_file::other_files -> begin
        print_string(input_file^"\n");
        let theory = clean_file (read_whole_file input_file) in
          let l = read_body theory input_file in 
            let oc = open_out (new_name(input_file)) in 
            begin
              Printf.fprintf oc "%s\n" "name,proof,address";
              print_list oc l;
              close_out oc;
              process_file (n+1) other_files
            end
    end
    | _ -> print_string "End\n";;

let files = (dir_contents "/home/josephcmac/HoTT/") in 
    let input_file = List.filter v_ext_file files in
    process_file 0 input_file