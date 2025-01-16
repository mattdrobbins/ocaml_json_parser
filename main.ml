let print_token t = print_endline (Scanner.token_string t)

let print_tokens l = 
  List.iter print_token l

let run_file filename = 
  let channel = open_in filename in 
  try 
      (* read entire file *)
      let line = really_input_string channel (in_channel_length channel) in
      flush stdout;
      close_in channel;
      Scanner.scan_tokens line
  with e -> 
      close_in_noerr channel;
      raise e

      
(* let _ = print_tokens (run_file "json_examples/example.json").tokens *)

let print_json json = print_endline (Parser.json_option_to_string json)

let _ = print_json (Parser.parse (run_file "json_examples/kvp_multi.json").tokens).json