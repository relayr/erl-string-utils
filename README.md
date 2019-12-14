# string_utils

[![Build Status](https://travis-ci.org/relayr/erl-string-utils.svg?branch=master)](https://travis-ci.org/relayr/erl-string-utils) [![Coverage Status](https://coveralls.io/repos/github/relayr/erl-string-utils/badge.svg?branch=master)](https://coveralls.io/github/relayr/erl-string-utils?branch=master)

Various functions operating with Erlang strings and binaries.

## Examples

Conversion of string (list of characters) to string containing hexadecimally encoded ASCII characters of the input string
i.e. 'a' => 0x61 (97 decimally), '1' => 0x31 (49 decimally).

#### list_to_hex_string/1, hex_string_to_list/1
```
1> string_utils:list_to_hex_string("abc123").
"616263313233"
2> string_utils:hex_string_to_list("616263313233").
"abc123"
```

#### list_to_string/1
Concatenate list of atoms, integers and strings into comma-separated string with list of values.
```
3> string_utils:list_to_string([atom, 1, "str"]).
"atom,1,str"
```

#### get_random_string/2
Generate random string of given size from characters set.
```
4> string_utils:get_random_string(8, lists:seq($a,$f) ++ lists:seq($0,$9) ++ [$_, $/]).
"b/138_6f"
```

#### get_random_hex_bytes/1
Generate random binary string with given number of hexadecimal ASCII characters.
```
5> string_utils:get_random_hex_bytes(4).
<<"A01327EB">>
```

#### convert_case/2
Conversion of strings into different formats (camelCase, PascalCase, lower_case).
```
6> string_utils:convert_case("this_is_erlang", camel).
"thisIsErlang"
7> string_utils:convert_case("this_is_erlang", pascal).
"ThisIsErlang"
8> string_utils:convert_case("HelloWorld", lower).
"hello_world"
```

