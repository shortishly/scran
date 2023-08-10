%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


-module(scran_tests).



-define(T(Parser), fun ({Expected, Input} = Test) -> {nm(Test), ?_assertEqual(Expected, (Parser)(Input))} end).
-import(scran_bits, [into_boolean/0]).
-import(scran_branch, [alt/1]).
-import(scran_branch, [permutation/1]).
-import(scran_character_complete, [alpha0/0]).
-import(scran_character_complete, [alpha1/0]).
-import(scran_character_complete, [alphanumeric0/0]).
-import(scran_character_complete, [alphanumeric1/0]).
-import(scran_character_complete, [digit0/0]).
-import(scran_character_complete, [digit1/0]).
-import(scran_character_complete, [hex_digit0/0]).
-import(scran_character_complete, [hex_digit1/0]).
-import(scran_character_complete, [multispace0/0]).
-import(scran_character_complete, [multispace1/0]).
-import(scran_character_complete, [none_of/1]).
-import(scran_character_complete, [one_of/1]).
-import(scran_character_complete, [re/1]).
-import(scran_character_complete, [re_no_case/1]).
-import(scran_character_complete, [tag/1]).
-import(scran_character_complete, [tag_no_case/1]).
-import(scran_combinator, [all_consuming/1]).
-import(scran_combinator, [condition/2]).
-import(scran_combinator, [eof/0]).
-import(scran_combinator, [is_not/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [opt/1]).
-import(scran_combinator, [peek/1]).
-import(scran_combinator, [rest/0]).
-import(scran_combinator, [value/2]).
-import(scran_multi, [many1/1]).
-import(scran_multi, [separated_list0/2]).
-import(scran_multi, [separated_list1/2]).
-import(scran_result, [ignore/1]).
-import(scran_result, [into_atom/1]).
-import(scran_result, [into_bits/2]).
-import(scran_result, [into_existing_atom/1]).
-import(scran_result, [into_integer/1]).
-import(scran_result, [into_map/1]).
-import(scran_result, [into_snake_case/1]).
-import(scran_result, [into_tuple/1]).
-import(scran_result, [kv/2]).
-import(scran_sequence, [delimited/3]).
-import(scran_sequence, [pair/2]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [separated_pair/3]).
-import(scran_sequence, [sequence/1]).
-import(scran_sequence, [terminated/2]).
-include_lib("eunit/include/eunit.hrl").


one_of_test_() ->
    lists:map(
      ?T(one_of("abc")),
      [{{[], "b"}, "b"},
       {{"cc", "c"}, "ccc"},
       {{<<"cc">>, <<"c">>}, <<"ccc">>},
       {nomatch, "d"},
       {nomatch, ""}]).


tag_test_() ->
    lists:map(
      ?T(tag("Hello")),
      [{{", World!", "Hello"}, "Hello, World!"},
       {{<<", World!">>, <<"Hello">>}, <<"Hello, World!">>},
       {nomatch, "Something"},
       {nomatch, ""}]).

tag_no_case_test_() ->
    lists:map(
      ?T(tag_no_case("hello")),
      [{{", World!", "Hello"}, "Hello, World!"},
       {{<<", World!">>, <<"Hello">>}, <<"Hello, World!">>},
       {{", World!", "hello"}, "hello, World!"},
       {{", World!", "HeLlO"}, "HeLlO, World!"},
       {nomatch, "Something"},
       {nomatch, ""}]).

re_b_plus_test_() ->
    lists:map(
      ?T(re("b+")),
      [{nomatch, "aaabbb"},
       {{"aaa", "bbb"}, "bbbaaa"},
       {{<<"aaa">>, <<"bbb">>}, <<"bbbaaa">>}]).

re_az_star_test_() ->
    lists:map(
      ?T(re("[a-z_]*")),
      [{{"123", ""}, "123"},
       {{"", "bbbaaa"}, "bbbaaa"},
       {{<<"">>, <<"bbbaaa">>}, <<"bbbaaa">>},
       {{"", ""}, ""}]).

re_no_case_test_() ->
    lists:map(
      ?T(re("inner( join)?")),
      [{{"abc", "inner"}, "innerabc"},
       {{"abc", "inner join"}, "inner joinabc"},
       {nomatch, "outer join"}]).

alpha0_test_() ->
    lists:map(
      ?T(alpha0()),
      [{{"1c", "ab"}, "ab1c"},
       {{<<"1c">>, <<"ab">>}, <<"ab1c">>},
       {{"1c", []}, "1c"},
       {{[], []}, []}]).

alpha1_test_() ->
    lists:map(
      ?T(alpha1()),
      [{{"1c", "aB"}, "aB1c"},
       {{<<"1c">>, <<"aB">>}, <<"aB1c">>},
       {nomatch, "1c"},
       {nomatch, ""}]).

alphanumeric0_test_() ->
    lists:map(
      ?T(alphanumeric0()),
      [{{"%1", "21cZ"}, "21cZ%1"},
       {{<<"%1">>, <<"21cZ">>}, <<"21cZ%1">>},
       {{"&Z21c", ""}, "&Z21c"},
       {{"", ""}, ""}]).

alphanumeric1_test_() ->
    lists:map(
      ?T(alphanumeric1()),
      [{{"%1", "21cZ"}, "21cZ%1"},
       {{<<"%1">>, <<"21cZ">>}, <<"21cZ%1">>},
       {nomatch, "&H2"},
       {nomatch, ""}]).

digit0_test_() ->
    lists:map(
      ?T(digit0()),
      [{{"c", "21"}, "21c"},
       {{<<"c">>, <<"21">>}, <<"21c">>},
       {{"", "21"}, "21"},
       {{"a21c", ""}, "a21c"},
       {{"", ""}, ""}]).

digit1_test_() ->
    lists:map(
      ?T(digit1()),
      [{{"c", "21"}, "21c"},
       {{<<"c">>, <<"21">>}, <<"21c">>},
       {{"", "21"}, "21"},
       {nomatch, "a21c"},
       {nomatch, ""}]).

multispace0_test_() ->
    lists:map(
      ?T(multispace0()),
      [{{"21c", " \t\n\r"}, " \t\n\r21c"},
       {{<<"21c">>, <<" \t\n\r">>}, <<" \t\n\r21c">>},
       {{"Z21c", ""}, "Z21c"},
       {{"", ""}, ""}]).


multispace1_test_() ->
    lists:map(
      ?T(multispace1()),
      [{{"21c", " \t\n\r"}, " \t\n\r21c"},
       {{<<"21c">>, <<" \t\n\r">>}, <<" \t\n\r21c">>},
       {nomatch, "Z21c"},
       {nomatch, ""}]).


alt_test_() ->
    lists:map(
      ?T(alt([alpha1(), digit1()])),
      [{{"", "abc"}, "abc"},
       {{"123", "abc"}, "abc123"},
       {{<<"123">>, <<"abc">>}, <<"abc123">>},
       {{"", "123456"}, "123456"},
       {{"abc", "123456"}, "123456abc"},
       {nomatch, " "}]).

permutation_test_() ->
    lists:map(
      ?T(permutation([alpha1(), digit1()])),
      [{{"", ["abc", "123"]}, "abc123"},
       {{"", ["123", "abc"]}, "123abc"},
       {{<<"">>, [<<"123">>, <<"abc">>]}, <<"123abc">>},
       {nomatch, "abc"}]).

sequence_test_() ->
    lists:map(
      ?T(sequence([alpha1(), digit1()])),
      [{{"", ["abc", "123"]}, "abc123"},
       {{<<"">>, [<<"abc">>, <<"123">>]}, <<"abc123">>},
       {nomatch, "123abc"},
       {nomatch, "abc"}]).

flat_sequence_test_() ->
    lists:map(
      ?T(sequence([alpha1(),
                   sequence([digit1(), alpha1()]),
                   digit1()])),
      [{{"", ["abc", "123", "def", "456"]}, "abc123def456"},
       {{<<"">>,
         [<<"abc">>,
          <<"123">>,
          <<"def">>,
          <<"456">>]},
        <<"abc123def456">>},
       {nomatch, "123abc"},
       {nomatch, "abc"}]).

all_consuming_test_() ->
    lists:map(
      ?T(all_consuming(alpha1())),
      [{{"", "abcd"}, "abcd"},
       {{<<"">>, <<"abcd">>}, <<"abcd">>},
       {nomatch, "abcd;"},
       {nomatch, "123abcd;"}]).

delimited_test_() ->
    lists:map(
      ?T(delimited(tag("("), tag("abc"), tag(")"))),
      [{{"", "abc"}, "(abc)"},
       {{"def", "abc"}, "(abc)def"},
       {{<<"def">>, <<"abc">>}, <<"(abc)def">>},
       {nomatch, ""},
       {nomatch, "123"}]).

pair_test_() ->
    lists:map(
      ?T(pair(tag("abc"), tag("def"))),
      [{{"", ["abc", "def"]}, "abcdef"},
       {{"ghi", ["abc", "def"]}, "abcdefghi"},
       {{<<"ghi">>, [<<"abc">>, <<"def">>]}, <<"abcdefghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).

preceded_test_() ->
    lists:map(
      ?T(preceded(tag("abc"), tag("def"))),
      [{{"", "def"}, "abcdef"},
       {{"ghi", "def"}, "abcdefghi"},
       {{<<"ghi">>, <<"def">>}, <<"abcdefghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).

separated_pair_test_() ->
    lists:map(
      ?T(separated_pair(tag("abc"), tag("|"), tag("def"))),
      [{{"", ["abc", "def"]}, "abc|def"},
       {{"ghi", ["abc", "def"]}, "abc|defghi"},
       {{<<"ghi">>, [<<"abc">>, <<"def">>]}, <<"abc|defghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).

terminated_test_() ->
    lists:map(
      ?T(terminated(tag("abc"), tag("def"))),
      [{{"", "abc"}, "abcdef"},
       {{"ghi", "abc"}, "abcdefghi"},
       {{<<"ghi">>, <<"abc">>}, <<"abcdefghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).


map_result_test_() ->
    lists:map(
      ?T(map_result(digit1(), fun string:length/1)),
      [{{"", 6}, "123456"},
       {{<<"">>, 6}, <<"123456">>},
       {nomatch, "abc"}]).

map_parser_test_() ->
    lists:map(
      ?T(map_parser(scran_character_complete:take(5), digit1())),
      [{{"", "12345"}, "12345"},
       {{"abc", "12345"}, "12345abc"},
       {{<<"abc">>, <<"12345">>}, <<"12345abc">>},
       {{"", "123"}, "123ab"},
       {{"c", "123"}, "123abc"},
       {nomatch, "123"}]).

opt_test_() ->
    lists:map(
      ?T(opt(alpha1())),
      [{{";", "abcd"}, "abcd;"},
       {{<<";">>, <<"abcd">>}, <<"abcd;">>},
       {{"123;", none}, "123;"}]).

value_test_() ->
    lists:map(
      ?T(value(1234, alpha1())),
      [{{";", 1234}, "abcd;"},
       {{<<";">>, 1234}, <<"abcd;">>},
       {nomatch, "123;"}]).

condition_true_test_() ->
    lists:map(
      ?T(condition(true, alpha1())),
      [{{";", "abcd"}, "abcd;"},
       {{<<";">>, <<"abcd">>}, <<"abcd;">>},
       {nomatch, "123;"}]).

condition_fun_true_test_() ->
    lists:map(
      ?T(condition(
           fun
               () ->
                   true
           end,
           alpha1())),
      [{{";", "abcd"}, "abcd;"},
       {{<<";">>, <<"abcd">>}, <<"abcd;">>},
       {nomatch, "123;"}]).

condition_false_test_() ->
    lists:map(
      ?T(condition(false, alpha1())),
      [{{"abcd;", none}, "abcd;"},
       {{<<"abcd;">>, none}, <<"abcd;">>}]).

condition_fun_false_test_() ->
    lists:map(
      ?T(condition(
           fun
               () ->
                   false
           end,
           alpha1())),
      [{{"abcd;", none}, "abcd;"},
       {{<<"abcd;">>, none}, <<"abcd;">>}]).


peek_test_() ->
    lists:map(
      ?T(peek(alpha1())),
      [{{"abcd;", "abcd"}, "abcd;"},
       {nomatch, "123;"}]).


eof_test_() ->
    lists:map(
      ?T(eof()),
      [{nomatch, "abc"},
       {{"", ""}, ""},
       {{<<>>, <<>>}, <<>>}]).


rest_test_() ->
    lists:map(
      ?T(rest()),
      [{{"", "abc"}, "abc"},
       {{"", ""}, ""},
       {{<<>>, <<"abc">>}, <<"abc">>},
       {{<<>>, <<>>}, <<>>}]).


is_not_test_() ->
    lists:map(
      ?T(is_not(alpha1())),
      [{nomatch, "abc"},
       {{"", "123"}, "123"},
       {{<<"">>, <<"123">>}, <<"123">>},
       {{<<>>, <<>>}, <<>>}]).


hex_digit0_test_() ->
    lists:map(
      ?T(hex_digit0()),
      [{{"Z", "21c"}, "21cZ"},
       {{<<"Z">>, <<"21c">>}, <<"21cZ">>},
       {{"Z21c", ""}, "Z21c"},
       {{"", ""}, ""}]).


hex_digit1_test_() ->
    lists:map(
      ?T(hex_digit1()),
      [{{"Z", "21c"}, "21cZ"},
       {{<<"Z">>, <<"21c">>}, <<"21cZ">>},
       {nomatch, "Z21c"},
       {nomatch, ""}]).

character_take_n_test_() ->
    lists:map(
      ?T(scran_character_complete:take(5)),
      [{{"", "12345"}, "12345"},
       {{<<"c">>,<<"åäöab"/utf8>>},
        list_to_binary([195, 165, 195, 164, 195, 182, 97, 98, 99])},
       {{"6", "12345"}, "123456"},
       {{<<"6">>, <<"12345">>}, <<"123456">>},
       {nomatch, "1234"},
       {nomatch, ""}]).

bytes_take_num_byte_parser_test_() ->
    lists:map(
      ?T(scran_bytes:take(scran_number_be:u(16))),
      [{{"", <<"12345">>}, binary_to_list(<<5:16, "12345">>)},
       {{"6", <<"12345">>}, binary_to_list(<<5:16, "123456">>)},
       {{<<"6">>, <<"12345">>}, <<5:16, "123456">>},
       {{<<"äö"/utf8>>, <<"å"/utf8>>}, <<2:16, 195, 165, 195, 164, 195, 182>>},
       {nomatch, binary_to_list(<<5:16, "1234">>)},
       {{"1234", <<>>}, binary_to_list(<<0:16, "1234">>)},
       {nomatch, ""}]).

lsn_test_() ->
    lists:map(
      ?T(sequence([hex_digit1(), tag("/"), hex_digit1()])),
      [{{"", ["0", "/", "0"]}, "0/0"},
      {{<<"">>,[<<"0">>, <<"/">>, <<"0">>]}, <<"0/0">>}]).

many1_test_() ->
    lists:map(
      ?T(many1(alt([tag("abc"), tag("def")]))),
      [{{"", ["abc", "abc"]}, "abcabc"},
       {{"", ["abc", "def"]}, "abcdef"},
       {{"123", ["abc"]}, "abc123"},
       {{<<"123">>, [<<"abc">>]}, <<"abc123">>},
       {nomatch, "123123"},
       {nomatch, ""}]).

separated_list0_test_() ->
    lists:map(
      ?T(separated_list0(tag("|"), tag("abc"))),
      [{{"", ["abc", "abc", "abc"]}, "abc|abc|abc"},
       {{"123abc", ["abc"]}, "abc123abc"},
       {{"|def", ["abc"]}, "abc|def"},
       {{"", []}, ""},
       {{"def|abc", []}, "def|abc"}]).

separated_list1_test_() ->
    lists:map(
      ?T(separated_list1(tag("|"), tag("abc"))),
      [{{"", ["abc", "abc", "abc"]}, "abc|abc|abc"},
       {{"123abc", ["abc"]}, "abc123abc"},
       {{<<"123abc">>, [<<"abc">>]}, <<"abc123abc">>},
       {{"|def", ["abc"]}, "abc|def"},
       {nomatch, ""},
       {nomatch, "def|abc"}]).

none_of_test_() ->
    lists:map(
      ?T(none_of("abc")),
      [{{"", "z"}, "z"},
       {nomatch, "a"},
       {nomatch, ""},
       {{"a", "z"}, "za"},
       {{<<"a">>, <<"z">>}, <<"za">>}]).

i128_little_test_() ->
    lists:map(
      ?T(scran_number:i128(little)),
      [{{<<>>,
         16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d},
        <<16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d:128/little>>},
       {{<<>>,
         -16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d},
        <<-16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d:128/little>>}]).


number_bit_sizes() ->
    [8, 16, 24, 32, 40, 48, 56, 64, 72,
     80, 88, 96, 104, 112, 120, 128].


u2_test_() ->
    u(fun scran_number:u/2, [little, big], number_bit_sizes()).

u1_test_() ->
    u(fun
          (Endian, Size) ->
              M = scran_number,
              F = list_to_atom([$u | integer_to_list(Size)]),
              M:F(Endian)
      end,
      [little, big],
      number_bit_sizes()).

u1_be_test_() ->
    Endianess = big,
    u(fun
          (Endian, Size) when Endian == Endianess ->
              scran_number_be:u(Size)
      end,
      [Endianess],
      number_bit_sizes()).

u0_be_test_() ->
    Endianess = big,
    u(fun
          (Endian, Size) when Endian == Endianess ->
              M = scran_number_be,
              F = list_to_atom([$u | integer_to_list(Size)]),
              M:F()
      end,
      [Endianess],
      number_bit_sizes()).

u1_le_test_() ->
    Endianess = little,
    u(fun
          (Endian, Size) when Endian == Endianess ->
              scran_number_le:u(Size)
      end,
      [Endianess],
      number_bit_sizes()).

u0_le_test_() ->
    Endianess = little,
    u(fun
          (Endian, Size) when Endian == Endianess ->
              M = scran_number_le,
              F = list_to_atom([$u | integer_to_list(Size)]),
              M:F()
      end,
      [Endianess],
      number_bit_sizes()).


u(F, Endianess, Sizes) ->
    lists:flatmap(
      ?FUNCTION_NAME(F),
      [{Endian, Size} || Endian <- Endianess,
                                    Size <- Sizes]).

u(F) ->
    fun
        ({Endianess, Size}) ->
            lists:map(
              ?T(F(Endianess, Size)),
              [{{<<>>, 0}, bits(Endianess, Size, 0)},
               {{<<>>, 1}, bits(Endianess, Size, 1)},
               {{<<>>, 1 bsl Size - 1}, bits(Endianess, Size, 1 bsl Size - 1)},
               {nomatch, <<>>},
               {nomatch, bits(Endianess, Size + 1, 1)},
               {nomatch, bits(Endianess, Size - 1, 1)}])
    end.


i2_test_() ->
    i(fun scran_number:i/2, [little, big], number_bit_sizes()).

i1_test_() ->
    i(fun
          (Endian, Size) ->
              M = scran_number,
              F = list_to_atom([$i | integer_to_list(Size)]),
              M:F(Endian)
      end,
      [little, big],
      number_bit_sizes()).

i0_be_test_() ->
    Endianess = big,
    i(fun
          (Endian, Size) when Endian == Endianess ->
              M = scran_number_be,
              F = list_to_atom([$i | integer_to_list(Size)]),
              M:F()
      end,
      [Endianess],
      number_bit_sizes()).

i1_be_test_() ->
    Endianess = big,
    i(fun
          (Endian, Size) when Endian == Endianess ->
              scran_number_be:i(Size)
      end,
      [Endianess],
      number_bit_sizes()).

i1_le_test_() ->
    Endianess = little,
    i(fun
          (Endian, Size) when Endian == Endianess ->
              scran_number_le:i(Size)
      end,
      [Endianess],
      number_bit_sizes()).

i0_le_test_() ->
    Endianess = little,
    i(fun
          (Endian, Size) when Endian == Endianess ->
              M = scran_number_le,
              F = list_to_atom([$i | integer_to_list(Size)]),
              M:F()
      end,
      [Endianess],
      number_bit_sizes()).

i(F, Endianess, Sizes) ->
    lists:flatmap(
      ?FUNCTION_NAME(F),
      [{Endian, Size} || Endian <- Endianess,
                                    Size <- Sizes]).

i(F) ->
    fun
        ({Endianess, Size}) ->
            lists:map(
              ?T(F(Endianess, Size)),
              [{{<<>>, -1 bsl (Size - 1)}, bits(Endianess, Size, -1 bsl (Size - 1))},
               {{<<>>, -1}, bits(Endianess, Size, (1 bsl Size) - 1)},
               {{<<>>, 0}, bits(Endianess, Size, 0)},
               {{<<>>, 1}, bits(Endianess, Size, 1)},
               {{<<>>, 1 bsl (Size - 1) - 1}, bits(Endianess, Size, 1 bsl (Size - 1) - 1)},
               {nomatch, <<>>},
               {nomatch, bits(Endianess, Size + 1, 1)},
               {nomatch, bits(Endianess, Size - 1, 1)}])
    end.


bits(little, Size, Value) ->
    <<Value:Size/little>>;
bits(big, Size, Value) ->
    <<Value:Size/big>>.


bytes_take_test_() ->
    lists:map(
      ?T(scran_bytes:take(6)),
      [{{<<>>, <<"123456">>}, <<"123456">>},
       {{<<"321">>, <<"123456">>}, <<"123456321">>},
       {nomatch, <<"123">>}]).

bytes_tag_test_() ->
    lists:map(
      ?T(scran_bytes:tag(<<"abc">>)),
      [{{<<"123">>, <<"abc">>}, <<"abc123">>},
       {{<<>>, <<"abc">>}, <<"abc">>},
       {nomatch, <<"123">>}]).

bytes_null_terminated_test_() ->
    lists:map(
      ?T(scran_bytes:null_terminated()),
      [{{<<>>, <<"null">>}, <<"null", 0:8>>},
       {nomatch, <<"missing null">>},
       {{<<"terminated">>, <<"null">>}, <<"null", 0:8, "terminated">>}]).

bytes_length_encoded_test_() ->
    lists:map(
      ?T(scran_bytes:length_encoded(scran_number_be:u16())),
      [{{<<" World!">>, <<"Hello">>}, <<5:16, "Hello World!">>},
       {nomatch, <<5:16, "He">>},
       {nomatch, <<>>}]).

result_into_bits_test_() ->
    lists:map(
      ?T(into_bits(scran_number_be:u16(), 16)),
      [{{<<>>, <<5:16>>}, <<5:16>>},
       {{<<"abc">>, <<5:16>>}, <<5:16, "abc">>},
       {nomatch, <<5:8>>},
       {nomatch, <<>>}]).

bits_into_boolean_test_() ->
    lists:map(
      ?T(into_boolean()),
      [{{<<>>, true}, <<1:1>>},
       {{<<>>, false}, <<0:1>>},
       {{<<0:1>>, true}, <<2#10:2>>},
       {{<<1:1>>, false}, <<2#01:2>>},
       {nomatch, <<>>}]).

bits_into_many1_boolean_test_() ->
    lists:map(
      ?T(map_parser(into_bits(scran_number_be:u(3), 3),
                    many1(into_boolean()))),
      [{{<<>>, [true, false, true]}, <<2#101:3>>},
       {{<<>>, [false, true, false]}, <<2#010:3>>}]).

kv_test_() ->
    lists:map(
      ?T(kv(action, alpha1())),
      [{{[], {action, "SELECT"}}, "SELECT"},
       {nomatch, "123"}]).

into_snake_case_test_() ->
    lists:map(
      ?T(kv(join_type,
            into_snake_case(
              alt([re_no_case("(INNER )?JOIN"),
                   re_no_case("LEFT (OUTER )?JOIN"),
                   re_no_case("RIGHT (OUTER )?JOIN"),
                   re_no_case("FULL (OUTER )?JOIN")])))),
      [{{[], {join_type, "join"}}, "join"},
       {{<<>>, {join_type, <<"join">>}}, <<"JOIN">>},
       {{[], {join_type, "inner_join"}}, "inner join"},
       {{<<>>, {join_type, <<"inner_join">>}}, <<"INNER JOIN">>},
       {{[], {join_type, "left_join"}}, "left join"},
       {{<<>>, {join_type, <<"left_join">>}}, <<"LEFT JOIN">>},
       {{[], {join_type, "left_outer_join"}}, "left outer join"},
       {{<<>>, {join_type, <<"left_outer_join">>}}, <<"LEFT OUTER JOIN">>},
       {nomatch, "123"}]).

into_atom_test_() ->
    lists:map(
      ?T(into_atom(alpha1())),
      [{{[], join}, "join"},
      {{<<>>, join}, <<"join">>}]).

into_existing_atom_test_() ->
    lists:map(
      ?T(into_existing_atom(alpha1())),
      [{{[], join}, "join"},
       {{<<>>, join}, <<"join">>},
       {nomatch, alpha(5)}]).

into_map_test_() ->
    lists:map(
      ?T(into_map(
          separated_pair(kv(k, alpha1()),
                         tag("="),
                         kv(v, digit1())))),
      [{{[], #{k => "abc", v => "123"}}, "abc=123"},
       {nomatch, "abc"}]).

into_integer_test_() ->
    lists:map(
      ?T(into_integer(digit1())),
      [{{"abc", 123}, "123abc"},
       {{<<"abc">>, 123}, <<"123abc">>},
       {nomatch, "abc123"}]).

into_tuple_test_() ->
    lists:map(
      ?T(into_tuple(
           separated_pair(
             alpha1(),
             tag("="),
             into_integer(digit1())))),
      [{{"", {"a", 123}}, "a=123"},
       {{"b=321", {"a", 123}}, "a=123b=321"},
       {nomatch, "a"},
       {nomatch, "a="},
       {nomatch, "a=b"},
       {nomatch, ""}]).

result_ignore_test_() ->
    lists:map(
      ?T(into_map(
          sequence(
            [kv(k, alpha1()),
             ignore(tag("=")),
             kv(v, digit1())]))),
      [{{[], #{k => "abc", v => "123"}}, "abc=123"},
       {nomatch, "abc"}]).

count_n_test_() ->
    lists:map(
      ?T(scran_multi:count(3, one_of(lists:seq($0, $9)))),
      [{{[], ["1", "2", "3"]}, "123"},
       {{"456", ["1", "2", "3"]}, "123456"},
       {nomatch, "12"},
       {nomatch, "abc"}]).

count_num_of_item_parser_test_() ->
    lists:map(
      ?T(scran_multi:count(
           scran_number_be:u(16),
           into_tuple(
             sequence([scran_bytes:null_terminated(),
                       scran_bytes:null_terminated()])))),
      [{{<<"pqr">>, [{<<"a">>, <<"123">>}]},
        <<1:16, "a", 0, "123", 0, "pqr">>},

       {{<<"pqr">>, [{<<"a">>, <<"123">>}, {<<"b">>, <<"321">>}]},
        <<2:16, "a", 0, "123", 0, "b", 0, "321", 0, "pqr">>},

       {{<<>>, []}, <<0:16>>},

       {nomatch, <<-1:16>>},

       {nomatch, <<>>},

       {nomatch, <<2:16, "a", 0, "123", 0, "pqr">>}]).


fold_test_() ->
    lists:map(
      ?T(scran_multi:fold(
           scran_number_be:u(16),
           into_tuple(
             sequence([scran_bytes:null_terminated(),
                       scran_bytes:null_terminated()])),
          [],
          fun
              (Result, A) ->
                  [Result | A]
          end)),
      [{{<<"pqr">>, [{<<"a">>, <<"123">>}]},
        <<1:16, "a", 0, "123", 0, "pqr">>},

       {{<<"pqr">>, [{<<"b">>, <<"321">>}, {<<"a">>, <<"123">>}]},
        <<2:16, "a", 0, "123", 0, "b", 0, "321", 0, "pqr">>},

       {{<<>>, []}, <<0:16>>},

       {nomatch, <<-1:16>>},

       {nomatch, <<>>},

       {nomatch, <<2:16, "a", 0, "123", 0, "pqr">>}]).


fold_many0_test_() ->
    lists:map(
      ?T(scran_multi:fold_many0(
          one_of(lists:seq($0, $9)),
          [],
          fun
              (Result, A) ->
                  [Result | A]
          end)),
      [{{"", ["3", "2", "1"]}, "123"},
       {{"", ["6", "5", "4", "3", "2", "1"]}, "123456"},
       {{"", []}, ""},
       {{"abc", []}, "abc"}]).

fold_many1_test_() ->
    lists:map(
      ?T(scran_multi:fold_many1(
          one_of(lists:seq($0, $9)),
          [],
          fun
              (Result, A) ->
                  [Result | A]
          end)),
      [{{[], ["3", "2", "1"]}, "123"},
       {{[], ["6", "5", "4", "3", "2", "1"]}, "123456"},
       {nomatch, ""},
       {nomatch, "abc"}]).

fold_many_m_n_variable_test_() ->
    lists:map(
      ?T(scran_multi:fold_many_m_n(
          3,
          5,
          one_of(lists:seq($0, $9)),
          [],
          fun
              (Result, A) ->
                  [Result | A]
          end)),
      [{nomatch, "1"},
       {nomatch, "12"},
       {{[], ["3", "2", "1"]}, "123"},
       {{[], ["4", "3", "2", "1"]}, "1234"},
       {{[], ["5", "4", "3", "2", "1"]}, "12345"},
       {{"6", ["5", "4", "3", "2", "1"]}, "123456"},
       {nomatch, ""},
       {nomatch, "abc"}]).

fold_many_m_n_fixed_test_() ->
    lists:map(
      ?T(scran_multi:fold_many_m_n(
          3,
          3,
          one_of(lists:seq($0, $9)),
          [],
          fun
              (Result, A) ->
                  [Result | A]
          end)),
      [{nomatch, "1"},
       {nomatch, "12"},
       {{[], ["3", "2", "1"]}, "123"},
       {{"4", ["3", "2", "1"]}, "1234"},
       {{"45", ["3", "2", "1"]}, "12345"},
       {{"456", ["3", "2", "1"]}, "123456"},
       {nomatch, ""},
       {nomatch, "abc"}]).


success_test_() ->
    lists:map(
      ?T(scran_combinator:success(good)),
      [{{[], good}, []},
       {{"123", good}, "123"},
       {{<<>>, good}, <<>>},
       {{<<"123">>, good}, <<"123">>}]).


nm(Test) ->
    iolist_to_binary(io_lib:fwrite("~p", [Test])).

alpha(N) ->
    list_to_binary(pick(N, lists:seq($a, $z))).

pick(N, Pool) ->
    ?FUNCTION_NAME(N, Pool, []).


pick(0, _, A) ->
    A;

pick(N, Pool, A) ->
    ?FUNCTION_NAME(N - 1,
                   Pool,
                   [lists:nth(rand:uniform(length(Pool)), Pool) | A]).
