module StringMap = Map.Make(String)

type json =
| NULL
| OBJECT of json StringMap.t
| STRING of string
| BOOL of bool
| NUMBER of float
| ARRAY of json list

let rec json_to_string = function 
| NULL -> "NULL"
| OBJECT x -> "{" ^ StringMap.fold (fun k v init -> init ^ "key: " ^ k ^ ", value: " ^ json_to_string v) x "" ^ "}"
| STRING y -> y
| BOOL _ -> "BOOL"
| NUMBER _ -> "NUMBER"
| ARRAY _ -> "ARRAY"

let json_option_to_string = function
| Some x -> json_to_string x
| _ -> "none"

type parser_context = {
  current: int;
  _end : int;
  tokens: Scanner.token list;
  json: json option;
}

(* let print_token t = print_string (Scanner.token_string t) *)

let print_ctx ctx = Printf.printf "current: %i end:%i json:%s" ctx.current ctx._end (json_option_to_string ctx.json)
  
let unpack = function
| Some(x) -> x
| _ -> failwith "error"

let empty_map = StringMap.empty

let empty_object = OBJECT (empty_map)

let empty_object_context ctx = { ctx with json = Some(empty_object)}

let peek ctx = List.nth ctx.tokens ctx.current

let tail ctx = List.nth ctx.tokens ctx._end

let advance ctx = {ctx with current = ctx.current + 1}

let retreat ctx = {ctx with _end = ctx._end - 1}

let consume token_type ctx = 
  if (peek ctx).token_type = token_type then (advance ctx)
  else failwith ("Invalid token" ^ Scanner.token_string (peek ctx) ^ " expected: " ^ Scanner.token_type_to_string token_type)

let consume_at_end token_type ctx = 
  if (tail ctx).token_type = token_type then (retreat ctx)
  else failwith ("Invalid token" ^ Scanner.token_string (tail ctx) ^ " expected: " ^ Scanner.token_type_to_string token_type)

let get_number ctx = match (peek ctx).token_type with
| NUMBER -> Scanner.literal_type_option_to_number ((peek ctx).literal)
| _ -> failwith "key must be a string"

let get_string ctx = match (peek ctx).token_type with
| STRING -> Scanner.literal_type_option_to_string ((peek ctx).literal)
| _ -> failwith "key must be a string"

let get_bool ctx = match (peek ctx).token_type with
| BOOL -> Scanner.literal_type_option_to_bool ((peek ctx).literal)
| _ -> failwith "key must be a bool"

let check_token index token_type ctx =
  (List.nth ctx.tokens index).token_type = token_type

let add_kvp_to_map key value map = 
  map|> StringMap.add key value

let add_kvp_to_context key value ctx = match ctx.json with
| Some (OBJECT(x)) -> { ctx with json = Some(OBJECT(add_kvp_to_map key value x))}
| _ -> failwith "can't add value to context"

let j_null ctx = if (check_token ctx.current NULL ctx) then {ctx with json = Some(NULL)}
else {ctx with json = None} 

let j_bool ctx = if (check_token ctx.current BOOL ctx) then {ctx with json = Some(BOOL(get_bool ctx)) }
else {ctx with json = None} 

let j_number ctx = if (check_token ctx.current NUMBER ctx) then {ctx with json = Some(NUMBER(get_number ctx)) }
else {ctx with json = None} 

let j_string ctx = if (check_token ctx.current STRING ctx) then {ctx with json = Some(STRING(get_string ctx)) }
else {ctx with json = None}

let rec j_object ctx =
  let string = j_string ctx
in match string.json with
| Some _ -> string
| None -> let number = j_number ctx
in match number.json with
| Some _ -> number
| None -> let bool = j_bool ctx
in match bool.json with
| Some _ -> bool
| None -> let null = j_null ctx
in match null.json with
| Some _ -> null
| None -> if (check_token ctx.current LEFT_BRACE ctx) 
    then add_kvp_to_context (get_string (advance ctx)) (unpack (j_object (ctx |> consume LEFT_BRACE |> consume STRING |> consume COLON |> consume_at_end RIGHT_BRACE)).json) (empty_object_context ctx)
  else failwith "missing"

let parse input = 
  let context = {
      tokens = input;
      current = 0;
      _end = (List.length input) - 1;
      json = None;
  } in j_object context;
