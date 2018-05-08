%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2009-2018
%% @doc Miscellaneous functions for string processing.
%% @end
%%------------------------------------------------------------------------------
-module(string_utils).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Function exports
%%------------------------------------------------------------------------------
-export([
    binary_to_hex_string/1,
    list_to_hex_string/1,
    hex_string_to_binary/1,
    hex_string_to_list/1,
    list_to_string/1,
    get_random_string/2,
    get_random_hex_bytes/1,
    convert_case/2,
    find_longest_prefix/1
]).

%%------------------------------------------------------------------------------
%% Macros
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Records
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

%% =============================================================================
%% Exported functions
%% =============================================================================

%% @doc Convert bytes to binary string with hexadecimal characters.
-spec binary_to_hex_string(Binary :: binary()) -> HexString :: binary().
binary_to_hex_string(Binary) ->
    list_to_binary([byte_to_hex(Byte) || Byte <- binary_to_list(Binary)]).

%% @doc Convert list of bytes to string with hexadecimal characters.
-spec list_to_hex_string(List :: [byte()]) -> HexString :: string().
list_to_hex_string(List) ->
    lists:flatten([byte_to_hex(X) || X <- List]).

%% @doc Convert binary string with hexadecimal characters to bytes.
-spec hex_string_to_binary(HexString :: binary()) -> binary().
hex_string_to_binary(HexString) ->
    list_to_binary(hex_string_to_list(binary_to_list(HexString), "")).

%% @doc Convert string with hexadecimal characters to list of bytes.
-spec hex_string_to_list(HexString :: string()) -> List :: [byte()].
hex_string_to_list(HexString) ->
    hex_string_to_list(HexString, "").

-spec hex_string_to_list(HexString :: string(), ByteList :: [byte()]) -> List :: [byte()].
hex_string_to_list([], List) ->
    lists:reverse(List);
hex_string_to_list([N1, N2 | RestOfHexString], List) ->
    hex_string_to_list(RestOfHexString, [hex_to_byte([N1, N2]) | List]).

%% @doc Convert list of elements to string e.g. [1,2,a] -> "1,2,a"
-spec list_to_string(List :: [integer() | atom() | string()]) -> String :: string().
list_to_string(List) ->
    list_to_string(List, "").
list_to_string([], "") ->
    "";
list_to_string([], [$, | RestOfString]) ->
    RestOfString;
list_to_string([Elem | Rest], String) when is_integer(Elem) ->
    list_to_string(Rest, String ++ "," ++ integer_to_list(Elem));
list_to_string([Elem | Rest], String) when is_atom(Elem) ->
    list_to_string(Rest, String ++ "," ++ atom_to_list(Elem));
list_to_string([Elem | Rest], String) when is_list(Elem) ->
    list_to_string(Rest, String ++ "," ++ Elem).

%% @doc Generates random string of given size from characters set.
-spec get_random_string(non_neg_integer(), nonempty_string()) -> nonempty_string().
get_random_string(Length, AllowedChars) ->
    lists:foldl(fun(_, Acc) ->
        [lists:nth(rand:uniform(length(AllowedChars)),
            AllowedChars)]
        ++ Acc
     end, [], lists:seq(1, Length)).

-spec get_random_hex_bytes(integer()) -> binary().
get_random_hex_bytes(N) ->
    binary_to_hex_string(crypto:strong_rand_bytes(N)).

-spec convert_case(String :: string(), Type :: camel | pascal | lower) -> {ok, OutputString :: string()}.
convert_case(String, Type) ->
    convert_case_local(String, Type, true, "").

%%------------------------------------------------------------------------------
%% @spec find_longest_prefix(Elems) -> LongestPrefix
%% where
%%		Elems = [list()]
%%		LongestPrefix = list()
%% @doc Find longest prefix among list of strings.
%% @end
%%------------------------------------------------------------------------------
-spec find_longest_prefix(Elems:: [string()]) -> LongestPrefix :: string().
find_longest_prefix([]) ->
    "";
find_longest_prefix(MatchingElems) ->
    [FirstElem | RestOfElems] = MatchingElems,
    if RestOfElems =:= [] ->
        FirstElem;
        true ->
            find_longest_prefix_local(FirstElem, 1, RestOfElems)
    end.

%% =============================================================================
%% Local functions
%% =============================================================================

%% @doc Convert byte to hexadecimal character (one or two [0-9,A-F,a-f] characters).
-spec byte_to_hex(Byte:: byte()) -> Hex :: nonempty_string().
byte_to_hex(Byte) when (Byte >= 0) andalso (Byte < 10) ->
    [$0, $0 + Byte];
byte_to_hex(Byte) when (Byte >= 10) andalso (Byte < 16) ->
    [$0, $A + (Byte - 10)];
byte_to_hex(Byte) when (Byte >= 16) andalso (Byte < 256) ->
    [$0, N1] = byte_to_hex(Byte bsr 4),
    [$0, N2] = byte_to_hex(Byte rem 16),
    [N1, N2].

%% @doc Convert hexadecimal character (one or two [0-9,A-F,a-f] characters) to byte.
-spec hex_to_byte(Hex :: nonempty_string()) -> Byte :: byte().
hex_to_byte([Hex]) ->
    hex_to_byte([$0, Hex]);
hex_to_byte([$0, Hex]) when (Hex >= $0) andalso (Hex =< $9) ->
    Hex - $0;
hex_to_byte([$0, Hex]) when (Hex >= $A) andalso (Hex =< $F) ->
    Hex - $A + 10;
hex_to_byte([$0, Hex]) when (Hex >= $a) andalso (Hex =< $f) ->
    Hex - $a + 10;
hex_to_byte([$0, _Hex]) ->
    erlang:error(badarg);
hex_to_byte([N1, N2]) ->
    (hex_to_byte([N1]) bsl 4) + hex_to_byte([N2]).

convert_case_local([], _Type, _FirstLetter, Output) ->
    {ok, Output};
convert_case_local([Char | RestOfString], lower, true, Output) when Char >= $A andalso Char =< $Z ->
    NewOutput = Output ++ [Char + 32],
    convert_case_local(RestOfString, lower, false, NewOutput);
convert_case_local([Char | RestOfString], lower, false, Output) when Char >= $A andalso Char =< $Z ->
    NewOutput = Output ++ [$_, Char + 32],
    convert_case_local(RestOfString, lower, false, NewOutput);

convert_case_local([Char | RestOfString], camel, true, Output) when Char >= $A andalso Char =< $Z ->
    NewOutput = Output ++ [Char + 32],
    convert_case_local(RestOfString, camel, false, NewOutput);
convert_case_local([$_, Char | RestOfString], camel, false, Output) when Char >= $a andalso Char =< $z ->
    NewOutput = Output ++ [Char - 32],
    convert_case_local(RestOfString, camel, false, NewOutput);

convert_case_local([Char | RestOfString], pascal, true, Output) when Char >= $a andalso Char =< $z ->
    NewOutput = Output ++ [Char - 32],
    convert_case_local(RestOfString, pascal, false, NewOutput);
convert_case_local([$_, Char | RestOfString], pascal, false, Output) when Char >= $a andalso Char =< $z ->
    NewOutput = Output ++ [Char - 32],
    convert_case_local(RestOfString, pascal, false, NewOutput);

convert_case_local([Char | RestOfString], Type, _FirstLetter, Output) ->
    NewOutput = Output ++ [Char],
    convert_case_local(RestOfString, Type, false, NewOutput).

-spec find_longest_prefix_local(Elem :: string(), N :: pos_integer(), Elems :: [string()]) -> LongestPrefix :: string().
find_longest_prefix_local(Elem, N, []) ->
    {LongestPrefix, _} = lists:split(N - 1, Elem),
    LongestPrefix;
find_longest_prefix_local(Elem, N, TestedElems) ->
    ElemLength = length(Elem),
    if N > ElemLength ->
        find_longest_prefix_local(Elem, N,  []);
        true ->
            ElemNth = lists:nth(N, Elem),
            NthPrefixIsOk = lists:foldl(fun(TestedElem, Acc) ->
                TestedElemLength = length(TestedElem),
                % any elem expands N
                if N > TestedElemLength ->
                    false;
                    true ->
                        TestedElemNth = lists:nth(N, TestedElem),
                        if ElemNth =/= TestedElemNth ->
                            false;
                        true ->
                            Acc
                        end
                end
                                        end, true, TestedElems),
            if NthPrefixIsOk =:= true ->
                find_longest_prefix_local(Elem, N + 1, TestedElems);
            true ->
                find_longest_prefix_local(Elem, N, [])
            end
    end.