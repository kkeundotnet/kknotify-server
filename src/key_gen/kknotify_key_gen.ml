(* http://www.codecodex.com/wiki/Generate_a_random_password_or_random_string#OCaml *)
let gen_rand length =
  Random.self_init () ;
  let gen () =
    match Random.int (26 + 26 + 10) with
    | n when n < 26 -> int_of_char 'a' + n
    | n when n < 26 + 26 -> int_of_char 'A' + n - 26
    | n -> int_of_char '0' + n - 26 - 26
  in
  let gen _ = String.make 1 (char_of_int (gen ())) in
  String.concat "" (Array.to_list (Array.init length gen))

let () =
  let oc = open_out Config.key_path in
  output_string oc (gen_rand 10) ;
  output_char oc '\n' ;
  close_out oc
