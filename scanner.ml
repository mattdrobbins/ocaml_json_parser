module StringMap = Map.Make(String)

type literal_type = STRING_LITERAL of string | NUMBER_LITERAL of float | BOOL_LITERAL of bool

type token_type = 
| COMMA
| LEFT_BRACE
| RIGHT_BRACE
| LEFT_SQUARE_BRACKET
| RIGHT_SQUARE_BRACKET
| BOOL
| COLON
| NULL
| STRING
| NUMBER

type token = {
    literal: literal_type option;
    line: int;
    token_type: token_type;
}

type scanner_context = {
    source: string;
    start: int;
    current: int;
    line: int;
    tokens: token list;
}

let token_type_to_string = function
| COMMA -> "COMMA"
| LEFT_BRACE -> "LEFT_BRACE"
| RIGHT_BRACE -> "RIGHT_BRACE"
| LEFT_SQUARE_BRACKET -> "LEFT_SQUARE_BRACKET"
| RIGHT_SQUARE_BRACKET -> "RIGHT_SQUARE_BRACKET"
| BOOL -> "BOOL"
| COLON-> "COLON"
| NULL-> "NULL"
| STRING-> "STRING"
| NUMBER-> "NUMBER"

let literal_type_to_number = function 
| NUMBER_LITERAL y -> y
| _ -> failwith "not a number"

let literal_type_to_bool = function 
| BOOL_LITERAL b -> b
| _ -> failwith "not a bool"

let literal_type_to_string = function 
| STRING_LITERAL x -> Printf.sprintf "%s" x
| NUMBER_LITERAL y -> Printf.sprintf "%F" y
| BOOL_LITERAL b -> Printf.sprintf "%B" b

let literal_type_option_to_string = function 
| Some x -> literal_type_to_string x
| None -> "None"

let literal_type_option_to_string_exn = function 
| Some x -> literal_type_to_string x
| None -> failwith "can't unpack string"

let literal_type_option_to_number = function 
| Some x -> literal_type_to_number x
| None -> failwith "not a number 2"

let literal_type_option_to_bool = function 
| Some x -> literal_type_to_bool x
| None -> failwith "not a bool"

let token_string (t : token) = Printf.sprintf "line: %i, token_type: %s, literal: %s" t.line (token_type_to_string t.token_type) (literal_type_option_to_string t.literal)

let identifiers = StringMap.empty 
|> StringMap.add "false" { literal = Some(BOOL_LITERAL(false)); line = 0; token_type = BOOL} 
|> StringMap.add "true" { literal = Some(BOOL_LITERAL(true)); line = 0; token_type = BOOL} 
|> StringMap.add "null" { literal = None; line = 0; token_type = NULL} 

let compare_char_option o1 o2 = match (o1, o2) with
| (Some x, Some y) -> Char.equal x y
| (_, _) -> false

let is_at_end ctx = ctx.current >= (String.length ctx.source)

let advance context = { context with current = context.current + 1}

let current_char context = try Some (String.get context.source context.current) with Invalid_argument _ -> None

let create_token_literal token_type literal line =
    {
        literal = literal;
        line = line;
        token_type = token_type 
    }

let add_token token ctx =
{ctx with tokens = {token with line = ctx.line} :: ctx.tokens}
    
let add_token_literal token_type literal ctx =
{ctx with tokens = create_token_literal token_type literal ctx.line :: ctx.tokens}

let add_token_with_type token_type = add_token_literal token_type None

let get_string start length ctx = String.sub ctx.source start length

let rec string_literal ctx =
    if (is_at_end ctx) then failwith "unterminated string"
    else if (compare_char_option (current_char ctx) (Some '"')) then add_token_literal STRING (Some(STRING_LITERAL(get_string (ctx.start + 1) (ctx.current - ctx.start - 1) ctx))) ctx
    else string_literal (advance ctx)

let rec number_literal ctx =
    match current_char ctx with
    | Some '.' -> number_literal (advance ctx)
    | Some '0'..'9' -> number_literal (advance ctx)
    | _ -> (advance ctx) |> add_token_literal NUMBER (Some(NUMBER_LITERAL(float_of_string (get_string ctx.start (ctx.current - ctx.start) ctx))))

let rec identifier ctx = 
    match current_char ctx with
    | Some 'a'..'z' -> identifier (advance ctx)
    | Some 'A'..'Z' -> identifier (advance ctx)
    | _ -> let iden = StringMap.find (get_string ctx.start (ctx.current - ctx.start) ctx) identifiers
        in add_token iden ctx

let scan_token context =   
    match (current_char context) with
    | Some ' ' | Some '\r'| Some '\t' -> advance context
    | Some '\n' -> {(advance context) with line = context.line + 1}
    | Some ',' -> advance context |> add_token_with_type COMMA
    | Some '{' -> advance context |> add_token_with_type LEFT_BRACE
    | Some '}' -> advance context |> add_token_with_type RIGHT_BRACE
    | Some ':' -> advance context |> add_token_with_type COLON
    | Some '[' -> advance context |> add_token_with_type LEFT_SQUARE_BRACKET
    | Some ']' -> advance context |> add_token_with_type RIGHT_SQUARE_BRACKET
    | Some '"' -> advance context |> string_literal |> advance
    | Some '1'..'9' -> number_literal context
    | Some _ -> identifier context
    | None -> failwith "Dissalowed character None"    

let rec _scan_tokens context = 
    if (is_at_end context) then context
    else let next_context = (scan_token context) 
in _scan_tokens {next_context with start = next_context.current}

let reverse_tokens ctx = { ctx with tokens = List.rev ctx.tokens }

let scan_tokens input = 
    let context = {
        source = input;
        start = 0;
        current = 0;
        line = 1;
        tokens = []
    } in _scan_tokens context |> reverse_tokens
