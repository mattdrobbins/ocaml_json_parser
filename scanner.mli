module Scanner :
  sig
    module StringMap :
      sig
        type key = string
        type 'a t = 'a Map.Make(String).t
        val empty : 'a t
        val is_empty : 'a t -> bool
        val mem : key -> 'a t -> bool
        val add : key -> 'a -> 'a t -> 'a t
        val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
        val singleton : key -> 'a -> 'a t
        val remove : key -> 'a t -> 'a t
        val merge :
          (key -> 'a option -> 'b option -> 'c option) ->
          'a t -> 'b t -> 'c t
        val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
        val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
        val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
        val iter : (key -> 'a -> unit) -> 'a t -> unit
        val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
        val for_all : (key -> 'a -> bool) -> 'a t -> bool
        val exists : (key -> 'a -> bool) -> 'a t -> bool
        val filter : (key -> 'a -> bool) -> 'a t -> 'a t
        val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
        val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
        val cardinal : 'a t -> int
        val bindings : 'a t -> (key * 'a) list
        val min_binding : 'a t -> key * 'a
        val min_binding_opt : 'a t -> (key * 'a) option
        val max_binding : 'a t -> key * 'a
        val max_binding_opt : 'a t -> (key * 'a) option
        val choose : 'a t -> key * 'a
        val choose_opt : 'a t -> (key * 'a) option
        val split : key -> 'a t -> 'a t * 'a option * 'a t
        val find : key -> 'a t -> 'a
        val find_opt : key -> 'a t -> 'a option
        val find_first : (key -> bool) -> 'a t -> key * 'a
        val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val find_last : (key -> bool) -> 'a t -> key * 'a
        val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
        val map : ('a -> 'b) -> 'a t -> 'b t
        val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
        val to_seq : 'a t -> (key * 'a) Seq.t
        val to_rev_seq : 'a t -> (key * 'a) Seq.t
        val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
        val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
        val of_seq : (key * 'a) Seq.t -> 'a t
      end
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
      lexeme : string;
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
    val identifiers : token_type StringMap.t
    val compare_char_option : char option -> char option -> bool
    val is_at_end : scanner_context -> bool
    val advance : scanner_context -> scanner_context
    val current_char : scanner_context -> char option
    val create_token_literal : token_type -> literal_type option -> token
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
