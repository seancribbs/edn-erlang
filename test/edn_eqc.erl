%% -------------------------------------
%% QuickCheck properties and generators
%% for the edn grammar
%% -------------------------------------
-module(edn_eqc).

-ifdef(EQC).
-include("edn.hrl").
-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").
-import(edn, [ unescape/1, parse/1 ]).

-compile(export_all).

%% -------------------------------------
%% Generators
%% -------------------------------------

%% Generates a list
gen_list() ->
    ?LET({Open, Elements, Close},
         {ws_wrap("("), ws_joined_list(gen_element()), ws_wrap(")")},
         ?to_utf8([Open, Elements, Close])).

%% Generates an arbitrary edn term
gen_element() ->
   oneof([
          gen_float(),
          gen_integer(),
          gen_char(),
          gen_keyword(),
          gen_string(),
          gen_bool(),
          gen_nil(),
          gen_symbol()
         ]).

%% Generates a floating point number
gen_float() ->
    ?LET({Float, Mark},
         {real(), elements([[], [$M]])},
         list_to_binary(float_to_list(Float) ++ Mark)).

%% Generates an integer
gen_integer() ->
    ?LET(I, elements([gen_int(), gen_arb_int()]), I).

%% Generates an integer with the arbitrary precision marker
gen_arb_int() ->
    ?LET(I, int(), list_to_binary(integer_to_list(I) ++ [$N])).

%% Generates an integer without the arbitrary precision marker
gen_int() ->
    ?LET(I, int(), list_to_binary(integer_to_list(I))).

%% Generates a character
gen_char() ->
    frequency([{1, <<"\\newline">>},
               {1, <<"\\space">>},
               {1, <<"\\tab">>},
               {10, ?LET(Char,
                         choose(33,126), %% TODO: Use characters other than US-ASCII printable
                         ?to_utf8([$\\, Char]))}]).

%% Generates a double-quoted string
gen_string() ->
    ?LET(Str, list(gen_string_char()),
         ?to_utf8([$", Str, $"])).

%% Generates a character that is valid within a string
gen_string_char() ->
    oneof([?SUCHTHAT(C, utf8_char(), C =/= $" andalso C =/= $\\), gen_escape_char()]).

%% Generates a valid escape character within a string
gen_escape_char() ->
    oneof([<<"\\t">>, <<"\\r">>, <<"\\n">>, <<"\\\"">>]).

%% Borrowed from erlang_protobuffs, generates a valid UTF8 char
utf8_char() ->
    ?LET(C, elements([choose(0, 36095),
                      choose(57344, 65533),
                      choose(65536, 1114111)]), C).

%% Generates a keyword
gen_keyword() ->
    ?LET(Sym, gen_symbol(), ?to_utf8([$:, Sym])).

%% Generates a symbol
gen_symbol() ->
    frequency([ {1, gen_leading_slash_symbol()}, {5, gen_normal_symbol()} ]).

%% Generates a symbol with a leading slash
gen_leading_slash_symbol() ->
    ?LET(Chars, list(gen_symbol_char()), ?to_utf8([<<"/">>, Chars])).

%% Generates a normal symbol, i.e. one without a leading slash
gen_normal_symbol() ->
    frequency([{2, gen_simple_symbol()},
               {1, gen_trailing_slash_symbol()},
               {2, gen_embedded_slash_symbol()}]).

%% Generates a symbol that doesn't contain a slash
gen_simple_symbol() ->
    ?LET({H,T},
         {oneof([
                 [ oneof([$., $-]), non_empty(list(gen_non_numeric_with_punct())) ],
                 [ gen_non_numeric_non_minus_plus(), non_empty(list(gen_non_numeric())) ]
                ]),
          list(gen_symbol_char())},
         ?to_utf8([H,T])).

%% Generates a symbol with a trailing slash
gen_trailing_slash_symbol() ->
    ?LET(Sym, gen_simple_symbol(), ?to_utf8([Sym, $/])).

%% Generates a symbol with a slash in the middle
gen_embedded_slash_symbol() ->
    ?LET({H,T},
         {gen_simple_symbol(), list(gen_symbol_char())},
         ?to_utf8([H, $/, T])).

%% Generates a non-numeric character that is valid as the character of a symbol
gen_non_numeric_non_minus_plus() ->
    oneof([$., $$, $!, $_, $?] ++
              lists:seq($A, $Z) ++
              lists:seq($a, $z)).

%% Generates a non-numeric character that is valid within a symbol
gen_non_numeric() ->
    oneof([$., $$, $+, $!, $-, $_, $?] ++
              lists:seq($A, $Z) ++
              lists:seq($a, $z)).

%% Generates a non-numeric character that is valid within a symbol,
%% but with the addition of # and : which are valid when not in first
%% position.
gen_non_numeric_with_punct() ->
    oneof([$., $$, $+, $!, $-, $_, $?, $#, $:] ++
              lists:seq($A, $Z) ++
              lists:seq($a, $z)).

%% Generates a character that is valid within a symbol after the first
%% and second positions.
gen_symbol_char() ->
    oneof([$., $$, $+, $!, $-, $_, $?, $#, $:] ++
              lists:seq($A, $Z) ++
              lists:seq($a, $z) ++
              lists:seq($0, $9)).

%% Generates 'nil'.
gen_nil() ->
    <<"nil">>.

%% Generates a boolean value
gen_bool() ->
    oneof([<<"true">>, <<"false">>]).

%% Generates whitespace.
gen_ws() ->
    ?LET(X,
         list(oneof([9, 10, 11, 12, 13, 32, $,])),
         ?to_utf8(X)).

%% Generates random whitespace around another generator
ws_wrap(Gen) ->
    ?LET({LWS, V, TWS},
         {gen_ws(), Gen, gen_ws()},
         ?to_utf8([LWS, V, TWS])).

%% Generates a list of the input type, joined with non-empty
%% whitespace.
ws_joined_list(Type) ->
    ?LET({LWS, List}, 
         {gen_ws(), list({ws_wrap(Type), non_empty(gen_ws())})},
         case length(List) of
             0 -> 
                 ?to_utf8([LWS]);
             _ ->
                 ?to_utf8([LWS | [ tuple_to_list(Pair) || Pair <- List ]])
         end).

%% ---------------------------------------------------
%% Parser properties (edn docs copy-pasted above each)
%% ---------------------------------------------------

%% Elements are generally separated by whitespace. Whitespace is not
%% otherwise significant, nor need redundant whitespace be preserved
%% during transmissions. Commas "," are also considered whitespace,
%% other than within strings.
prop_whitespace() ->
    ?FORALL(Spaces, gen_ws(),
            ok == ?assertThrow({edn,empty}, parse(Spaces))).

%% true and false should be mapped to booleans.
prop_bool() ->
    ?FORALL(Boolean, ws_wrap(gen_bool()),
            lists:member(parse(Boolean), [true, false])).

%% nil represents nil, null or nothing. It should be read as an object
%% with similar meaning on the target platform.
prop_nil() ->
    ?FORALL(Nil, ws_wrap(gen_nil()),
            nil == parse(Nil)).

prop_unescape() ->
    ?FORALL(Char, gen_escape_char(),
            lists:member(unescape(Char), [<<"\t">>, <<"\r">>, <<"\n">>, <<"\"">>])).

%% Strings are enclosed in "double quotes". May span multiple lines.
%% Standard C/Java escape characters \t \r \n are supported.
prop_string() ->
    ?FORALL(String, ws_wrap(gen_string()),
            is_binary(parse(String))).

%% Symbols are used to represent identifiers, and should map to
%% something other than strings, if possible.
%%
%% Symbols begin with a non-numeric character and can contain
%% alphanumeric characters and . * + ! - _ ?. If - or . are the first
%% character, the second character must be non-numeric. Additionally, :
%% # are allowed as constituent characters in symbols but not as the
%% first character.
%%
%% / has special meaning in symbols. It can be used once only in the
%% middle of a symbol to separate the prefix (often a namespace) from
%% the name, e.g. my-namespace/foo. / by itself is a legal symbol.
prop_symbol() ->
    ?FORALL(Symbol, ws_wrap(non_empty(gen_symbol())),
            is_atom(parse(Symbol))).

%% Characters are preceded by a backslash: \c. \newline, \space and
%% \tab yield the corresponding characters.
prop_character() ->
    ?FORALL(Char, ws_wrap(gen_char()),
            begin
                {char, Result} = parse(Char),
                is_integer(Result) andalso
                    is_binary(catch ?to_utf8([Result]))
            end).

%% Integers consist of the digits 0 - 9, optionally prefixed by - to
%% indicate a negative number. An integer can have the suffix N to
%% indicate that arbitrary precision is desired.
prop_integer() ->
    ?FORALL(Int, ws_wrap(gen_int()),
            is_integer(parse(Int))).

%% floating point numbers: 64-bit (double) precision is expected.
prop_float() ->
    ?FORALL(Float, ws_wrap(gen_float()),
            is_float(parse(Float))).

%% Keywords are identifiers that typically designate themselves. They
%% are semantically akin to enumeration values. Keywords follow the
%% rules of symbols, except they can (and must) begin with a colon,
%% e.g. :fred or :my/fred. If the target platform does not have a
%% keyword type distinct from a symbol type, the same type can be used
%% without conflict, since the mandatory leading : of keywords is
%% disallowed for symbols.
%%
%% If the target platform supports some notion of interning, it is a
%% further semantic of keywords that all instances of the same keyword
%% yield the identical object.
prop_keyword() ->
    ?FORALL(Keyword, 
            ws_wrap(gen_keyword()),
            begin
                Parsed = parse(Keyword),
                is_atom(Parsed) andalso hd(atom_to_list(Parsed)) =:= $:
            end).


%% A list is a sequence of values. Lists are represented by zero or
%% more elements enclosed in parentheses (). Note that lists can be
%% heterogeneous.
prop_list() ->
    ?FORALL(List, ws_wrap(gen_list()),
            is_list(parse(List))).

%% A vector is a sequence of values that supports random access.
%% Vectors are represented by zero or more elements enclosed in square
%% brackets []. Note that vectors can be heterogeneous.

%% prop_vector() ->
%%     ?FORALL(Vector, ws_wrap(gen_vector()),
%%             is_tuple(parse(Vector))).
-endif.
