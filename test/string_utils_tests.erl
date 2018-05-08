%%------------------------------------------------------------------------------
%% @author kuba.odias
%% @copyright relayr 2009-2018
%%------------------------------------------------------------------------------
-module(string_utils_tests).

%%------------------------------------------------------------------------------
%% Include files
%%------------------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

%% =============================================================================
%% Unit tests
%% =============================================================================

list_hex_string_test() ->
    L1 = string_utils:hex_string_to_list("010203ff0a"),
    ?assertEqual([1,2,3,255,10], L1),
    ?assertEqual("010203FF0A", string_utils:list_to_hex_string(L1)),
    L2 = string_utils:hex_string_to_list("0aA01eFf"),
    ?assertEqual([10,160,30,255], L2),
    ?assertEqual("0AA01EFF", string_utils:list_to_hex_string(L2)),
    % invalid characters
    ?assertException(error, badarg, string_utils:hex_string_to_list("##0102")),
    % wrong length
    ?assertException(error, function_clause, string_utils:hex_string_to_list("123")).

list_to_string_test() ->
    Res1 = string_utils:list_to_string([]),
    ?assertEqual("", Res1),
    Res2 = string_utils:list_to_string([1]),
    ?assertEqual("1", Res2),
    Res3 = string_utils:list_to_string([-1, 2]),
    ?assertEqual("-1,2", Res3),
    Res4 = string_utils:list_to_string([3, a, 4]),
    ?assertEqual("3,a,4", Res4),
    Res5 = string_utils:list_to_string(["ab", c, "d", 12]),
    ?assertEqual("ab,c,d,12", Res5).

get_random_string_test() ->
    Str1 = string_utils:get_random_string(0, "abc"),
    ?assertEqual("", Str1),
    Str2 = string_utils:get_random_string(5, "abc"),
    ?assertEqual(5, length(Str2)),
    ?assertEqual("", re:replace(Str2, "[abc]", "", [{return, list}, global])).

get_random_hex_bytes_test() ->
    R1 = string_utils:get_random_hex_bytes(0),
    ?assertEqual("", R1),
    R2 = string_utils:get_random_hex_bytes(3),
    ?assertEqual(6, length(R2)),
    lists:foreach(fun(HexValue) ->
        ?assert((HexValue >= $A andalso HexValue =< $F) orelse (HexValue >= $0 andalso HexValue =< $9))
     end, R2).

convert_case_test() ->
    ?assertEqual({ok, ""}, string_utils:convert_case("", lower)),
    ?assertEqual({ok, ""}, string_utils:convert_case("", camel)),
    ?assertEqual({ok, ""}, string_utils:convert_case("", pascal)),

    ?assertEqual({ok, "_"}, string_utils:convert_case("_", lower)),
    ?assertEqual({ok, "_"}, string_utils:convert_case("_", camel)),
    ?assertEqual({ok, "_"}, string_utils:convert_case("_", pascal)),

    ?assertEqual({ok, "ab_cd1_ef"}, string_utils:convert_case("ab_cd1_ef", lower)),
    ?assertEqual({ok, "ab_cd_e_f2"}, string_utils:convert_case("abCdEF2", lower)),
    ?assertEqual({ok, "a_b1c"}, string_utils:convert_case("aB1c", lower)),
    ?assertEqual({ok, "__a1_b2"}, string_utils:convert_case("_A1B2", lower)),
    ?assertEqual({ok, "a"}, string_utils:convert_case("A", lower)),

    ?assertEqual({ok, "abCd"}, string_utils:convert_case("abCd", camel)),
    ?assertEqual({ok, "abCd1Ef"}, string_utils:convert_case("AbCd1Ef", camel)),
    ?assertEqual({ok, "abCdEF2"}, string_utils:convert_case("ab_cd_e_f2", camel)),
    ?assertEqual({ok, "aB1c"}, string_utils:convert_case("a_b1c", camel)),
    ?assertEqual({ok, "_A1B2C3"}, string_utils:convert_case("_A1_b2C3", camel)),
    ?assertEqual({ok, "_a"}, string_utils:convert_case("_a", camel)),

    ?assertEqual({ok, "AbCd"}, string_utils:convert_case("AbCd", pascal)),
    ?assertEqual({ok, "AbCd1Ef"}, string_utils:convert_case("abCd1Ef", pascal)),
    ?assertEqual({ok, "AbCdEF2"}, string_utils:convert_case("ab_cd_e_f2", pascal)),
    ?assertEqual({ok, "AB1c"}, string_utils:convert_case("a_b1c", pascal)),
    ?assertEqual({ok, "_A1B2C3"}, string_utils:convert_case("_A1_b2C3", pascal)),
    ?assertEqual({ok, "_a"}, string_utils:convert_case("_a", pascal)).

find_longest_prefix_test() ->
    ?assertEqual("abc", string_utils:find_longest_prefix(["abcdef", "abc12"])),
    ?assertEqual("ab", string_utils:find_longest_prefix(["ababc", "abc"])),
    ?assertEqual("", string_utils:find_longest_prefix(["abc123", "dabc12", "abc"])),
    ?assertEqual("", string_utils:find_longest_prefix(["1abc", "abc1"])),
    ?assertEqual("", string_utils:find_longest_prefix([])),
    ?assertEqual("abc", string_utils:find_longest_prefix(["abc"])).

-endif.
