module StringMap = Map.Make(String)

type json =
| NULL
| OBJECT of json StringMap.t
| STRING of string
| BOOL of bool
| NUMBER of float
| ARRAY of json list

type parser_context = {
  current: int;
  tokens: Scanner.token list;
  json: json list;
}

let empty_map = StringMap.empty

let empty_object = OBJECT (empty_map)

let peek ctx = List.nth ctx.tokens ctx.current

let is_at_end ctx = (peek ctx).token_type = EOF

let advance ctx = {ctx with current = ctx.current + 1}

let consume token_type ctx = 
  if (peek ctx).token_type = token_type then (advance ctx)
  else failwith ("Invalid token" ^ Scanner.token_string (peek ctx) ^ " expected: " ^ Scanner.token_type_to_string token_type)

let get_key ctx = match (peek ctx).token_type with
| STRING -> Scanner.literal_type_option_to_string ((peek ctx).literal)
| _ -> failwith "key must be a string"

let match_token token_type ctx = 
  if (is_at_end ctx) then false
  else if (peek ctx).token_type = token_type then true
  else false

let new_ctx ctx = {ctx with json = [empty_object]}

let add_json_to_ctx ctx = {ctx with json = (empty_object :: ctx.json) }

let add_key_to_ctx ctx =
  (List.nth ctx.json 0) |>

let rec j_object ctx = 
   if (match_token LEFT_BRACE ctx) 
    then (consume LEFT_BRACE ctx 
    |> add_json_to_ctx
    |> (ct)
    |> consume RIGHT_BRACE)

else ctx

let rec _parse ctx = 
  if (is_at_end ctx) then ctx
  else if (match_token LEFT_BRACE ctx) then _parse (j_object ctx)
  else ctx

let parse tokens  = 
  let context = {
      current = 0;
      json = None;
      tokens = tokens
  } in _parse context