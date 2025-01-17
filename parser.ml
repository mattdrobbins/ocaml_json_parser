module StringMap = Map.Make(String)

type json =
| NULL
| OBJECT of json StringMap.t
| STRING of string
| BOOL of bool
| NUMBER of float
| ARRAY of json list
| NONE

type parser_context = {
  current: int;
  tokens: Scanner.token list;
  _end: int;
  kvp: (string * json) list;
  json: json;
}

let rec json_to_string = function 
| NULL -> "NULL"
| OBJECT x -> "{ " ^ StringMap.fold (fun k v init -> init ^ " " ^ k ^ ": " ^ json_to_string v) x " " ^ " }"
| STRING y -> y
| BOOL b -> Printf.sprintf "%B" b
| NUMBER n -> Printf.sprintf "%F" n
| ARRAY a -> "[" ^ List.fold_right (fun v init -> init ^ " " ^ json_to_string v) a " " ^ " ]"
| NONE -> "NONE"


let kvp_list_to_string (l : (string * json) list) = 
  "[" ^ (List.fold_left (fun i (s,j) -> i ^ "(" ^ s ^ " : " ^ json_to_string j ^ ")" ^ ",") " " l) ^ "]"

let print_ctx ctx = Printf.printf "current: %i, end:%i, json:%s kvp:%s" ctx.current ctx._end (json_to_string ctx.json) (kvp_list_to_string ctx.kvp) |> print_newline

let empty_map = StringMap.empty

let empty_object = OBJECT (empty_map)

let empty_array = ARRAY ([])

let context_is_object ctx = match ctx.json with
| OBJECT x -> true
| ARRAY y -> false
| _ -> failwith "no container context"


let add_json_to_ctx ctx = {ctx with json = empty_object}

let add_json_array_to_ctx ctx = {ctx with json = empty_array}

let peek ctx = List.nth ctx.tokens ctx.current

let peek_last ctx = List.nth ctx.tokens (ctx._end - 1)

let advance ctx = {ctx with current = ctx.current + 1}

let advance_to a ctx = {ctx with current = a}

let retreat ctx = {ctx with _end = ctx._end - 1}

let add_kvp_to_map pairs map = 
  map |> StringMap.add_seq pairs

let consume token_type ctx = 
  if (peek ctx).token_type = token_type then (advance ctx)
  else failwith ("Invalid token" ^ Scanner.token_string (peek ctx) ^ " expected: " ^ Scanner.token_type_to_string token_type)

let consume_at_end token_type ctx = 
  if (peek_last ctx).token_type = token_type then (retreat ctx)
  else failwith ("Invalid token" ^ Scanner.token_string (peek ctx) ^ " expected: " ^ Scanner.token_type_to_string token_type)

let get_number ctx = match (peek ctx).token_type with
| NUMBER -> Scanner.literal_type_option_to_number ((peek ctx).literal)
| _ -> failwith "key must be a number"

let get_string ctx = match (peek ctx).token_type with
| STRING -> Scanner.literal_type_option_to_string ((peek ctx).literal)
| _ -> failwith "key must be a string"

let get_bool ctx = match (peek ctx).token_type with
| BOOL -> Scanner.literal_type_option_to_bool ((peek ctx).literal)
| _ -> failwith "key must be a bool"

let new_context ctx = {ctx with
  json = NONE;
  kvp = [];
}

let add_kvp kvp ctx = { ctx with kvp = kvp :: ctx.kvp}

let combine_all_kvp ctx = match ctx.json with
| OBJECT(y) -> { ctx with json = OBJECT(add_kvp_to_map (List.to_seq ctx.kvp) y); kvp = []}
| _ -> failwith "can't add value to context"

let combine_all_values ctx = match ctx.json with
| ARRAY(y) -> { ctx with json = ARRAY((List.map (fun (x,y) -> y) ctx.kvp) @ y); kvp = []}
| _ -> failwith "can't add values to array"

let rec _parse ctx = 
  (* print_ctx ctx; *)
  if (ctx.current = ctx._end) then ctx else
  match (peek ctx).token_type with
  | LEFT_BRACE -> let key = get_string(ctx |> consume LEFT_BRACE) in  
    let value = _parse (ctx |> new_context |> consume LEFT_BRACE |> consume STRING |> consume COLON)
    in add_json_to_ctx ctx |> add_kvp (key, value.json) |> advance_to (value.current + 1) |> _parse 
  | RIGHT_BRACE -> combine_all_kvp ctx
  | RIGHT_SQUARE_BRACKET -> combine_all_values ctx
  | LEFT_SQUARE_BRACKET -> let value = _parse (ctx |> new_context |> consume LEFT_SQUARE_BRACKET)
      in add_json_array_to_ctx ctx |> add_kvp ("none", value.json) |> advance_to (value.current + 1) |> _parse  
  | STRING -> {ctx with json = STRING(get_string ctx)}
  | NUMBER -> {ctx with json = NUMBER (get_number ctx)}
  | BOOL -> {ctx with json = BOOL (get_bool ctx)}
  | NULL -> {ctx with json = NULL}
  | COMMA -> if (context_is_object ctx) then let key = get_string(ctx |> consume COMMA) in 
        (* print_endline ("key " ^ key); *)
        let value = _parse (ctx |> new_context |> consume COMMA |> consume STRING |> consume COLON)
        in add_kvp (key, value.json) ctx |> advance_to (value.current + 1) |> _parse 
      else let value = _parse (ctx |> new_context |> consume COMMA)
    in add_kvp ("none", value.json) ctx |> advance_to (value.current + 1) |> _parse 
  | _ -> failwith "missing case"

let parse input = 
  let context = {
      tokens = input;
      current = 0;
      _end = List.length input;
      kvp = [];
      json = NONE;
} in _parse context;