%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2019
%%------------------------------------------------------------------------------
-module(prop_string_utils).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(GEN_BYTES, list(choose(0,255))).
-define(GEN_PRINTABLE_CHARS, list(choose(32,126))).

-define(PRINTABLE_CHARS, lists:seq(32,126)).
-define(HEX_CHARS, lists:seq($0,$9) ++ lists:seq($A,$F)).

%% =============================================================================
%% Property-based tests
%% =============================================================================

prop_list_hex_string() ->
    proper:numtests(1000,
        ?FORALL(
            ListOfBytes,
            ?GEN_BYTES,
            begin
                HexList = string_utils:list_to_hex_string(ListOfBytes),
                ok = assert_hex_characters(HexList),
                ?assertEqual(2 * length(ListOfBytes), length(HexList)),
                ?assertEqual(ListOfBytes, string_utils:hex_string_to_list(HexList)),
                true
            end
        )
    ).

prop_binary_hex_string() ->
    proper:numtests(1000,
        ?FORALL(
            Bin,
            ?LET(ListOfBytes,
                ?GEN_BYTES,
                list_to_binary(ListOfBytes)
            ),
            begin
                HexBin = string_utils:binary_to_hex_string(Bin),
                ok = assert_hex_characters(binary_to_list(HexBin)),
                ?assertEqual(2 * byte_size(Bin), byte_size(HexBin)),
                ?assertEqual(Bin, string_utils:hex_string_to_binary(HexBin)),
                true
            end
        )
    ).

prop_get_random_string() ->
    proper:numtests(1000,
        ?FORALL(
            {Size, AllowedChars},
            {non_neg_integer(), non_empty(?GEN_PRINTABLE_CHARS)},
            begin
                RandomString = string_utils:get_random_string(Size, AllowedChars),
                ok = assert_allowed_characters(RandomString, ?PRINTABLE_CHARS),
                ?assertEqual(Size, length(RandomString)),
                true
            end
        )
    ).

prop_get_random_hex_bytes() ->
    proper:numtests(1000,
        ?FORALL(
            Size,
            non_neg_integer(),
            begin
                HexString = string_utils:get_random_hex_bytes(Size),
                ok = assert_allowed_characters(binary_to_list(HexString), ?HEX_CHARS),
                ?assertEqual(Size * 2, byte_size(HexString)),
                true
            end
        )
    ).

prop_convert_case_to_lower() ->
    proper:numtests(1000,
        ?FORALL(
            String,
            ?GEN_PRINTABLE_CHARS,  % contains also A-Z and a-z
            begin
                % convert 'someString' to 'some_string'
                {ok, LowerCaseString} = string_utils:convert_case(String, lower),
                AllowedChars = ?PRINTABLE_CHARS -- lists:seq($A,$Z),    % lower case string shouldn't contain A-Z
                ok = assert_allowed_characters(LowerCaseString, AllowedChars),
                % resulting string will have appended as much '_' characters as there are upper case letters
                ?assertEqual(length(String) + count_uppercase_letters(String), length(LowerCaseString)),
                true
            end
        )
    ).

prop_find_longest_prefix() ->
    proper:numtests(1000,
        ?FORALL(
            Strings,
            list(?GEN_PRINTABLE_CHARS),
            begin
                LongestPrefix = string_utils:find_longest_prefix(Strings),
                ?assert(lists:all(   % all strings should be at least the size of longest prefix
                    fun(Str) ->
                        length(LongestPrefix) =< length(Str)
                    end,
                    Strings
                )),
                if LongestPrefix =/= "" ->
                    % if longest prefix was found
                    ok = lists:foreach(
                        fun(Str) ->
                            % prefix should always begin at 1st character
                            ?assertEqual(1, string:str(Str, LongestPrefix))
                        end,
                        Strings
                    );
                true ->
                    % if longest prefix wasn't found
                    ok
                end,
                true
            end
        )
    ).

%% =============================================================================
%% Local functions
%% =============================================================================

assert_hex_characters(HexList) ->
    assert_allowed_characters(HexList, ?HEX_CHARS).

assert_allowed_characters(List, AllowedChars) ->
    ok = lists:foreach(
        fun(Chr) ->
            ?assert(lists:member(Chr, AllowedChars))
        end,
        List
    ).

count_uppercase_letters("") ->
    0;
count_uppercase_letters(String) ->
    lists:foldl(
        fun(Chr, Acc) ->
            case lists:member(Chr, lists:seq($A,$Z)) of
                true ->
                    Acc + 1;
                false ->
                    Acc
            end
        end,
        0,
        % skip first character (if it is upper case it will be converted to lower case without prepending '_')
        string:substr(String, 2)
    ).

-endif.