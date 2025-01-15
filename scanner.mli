module Scanner :
  sig
    type literal_type = STRING_LITERAL of string | NUMBER_LITERAL of float
    type token_type =
        COMMA
      | LEFT_BRACE
      | RIGHT_BRACE
      | LEFT_SQUARE_BRACKET
      | RIGHT_SQUARE_BRACKET
      | TRUE
      | FALSE
      | COLON
      | NULL
      | STRING
      | NUMBER
      | EOF
    type token = {
      literal : literal_type option;
      line : int;
      token_type : token_type;
    }
    type scanner_context = {
      source : string;
      start : int;
      current : int;
      line : int;
      tokens : token list;
    }
    val token_type_to_string : token_type -> string
    val literal_type_to_string : literal_type -> string
    val literal_type_option_to_string : literal_type option -> string
    val token_string : token -> string
    val compare_char_option : char option -> char option -> bool
    val is_at_end : scanner_context -> bool
    val advance : scanner_context -> scanner_context
    val current_char : scanner_context -> char option
    val create_token_literal : token_type -> literal_type option -> int -> token
    val add_token_literal :
      token_type -> literal_type option -> scanner_context -> scanner_context
    val add_token : token_type -> scanner_context -> scanner_context
    val get_string : int -> int -> scanner_context -> string
    val string_literal : scanner_context -> scanner_context
    val number_literal : scanner_context -> scanner_context
    val identifier : scanner_context -> scanner_context
    val scan_token : scanner_context -> scanner_context
    val _scan_tokens : scanner_context -> scanner_context
    val reverse_tokens : scanner_context -> scanner_context
    val scan_tokens : string -> scanner_context
  end
