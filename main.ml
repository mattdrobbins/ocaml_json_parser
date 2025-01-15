open Scanner

let get_token_type_string = function
| Scanner.COMMA -> "Comma"
| LEFT_BRACE -> "Left Brace"
| RIGHT_BRACE -> "Right Brace"
| LEFT_SQUARE_BRACKET -> "LEFT_SQUARE_BRACKET"
| RIGHT_SQUARE_BRACKET -> "Right_Square_Bracket"
| TRUE -> "true"
| FALSE-> "false"
| COLON-> "colon"
| NULL-> "null"
| STRING-> "string"
| NUMBER-> "number"
| EOF-> "end of file"

let get_token_type (t : Scanner.token) = t.token_type

let print_token t = get_token_type t |> get_token_type_string |> print_endline

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

      
let _ = print_tokens (run_file "example.json").tokens