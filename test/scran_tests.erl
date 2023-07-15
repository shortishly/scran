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
-import(scran_character_complete, [tag/1]).
-import(scran_character_complete, [tag_no_case/1]).
-import(scran_character_complete, [take/1]).
-import(scran_combinator, [all_consuming/1]).
-import(scran_combinator, [condition/2]).
-import(scran_combinator, [eof/0]).
-import(scran_combinator, [is_not/1]).
-import(scran_combinator, [map_parser/2]).
-import(scran_combinator, [map_result/2]).
-import(scran_combinator, [opt/1]).
-import(scran_combinator, [peek/1]).
-import(scran_combinator, [value/2]).
-import(scran_multi, [many1/1]).
-import(scran_multi, [separated_list0/2]).
-import(scran_multi, [separated_list1/2]).
-import(scran_result, [into_bits/2]).
-import(scran_sequence, [delimited/3]).
-import(scran_sequence, [pair/2]).
-import(scran_sequence, [preceded/2]).
-import(scran_sequence, [separated_pair/3]).
-import(scran_sequence, [sequence/1]).
-import(scran_sequence, [terminated/2]).
-include_lib("eunit/include/eunit.hrl").


one_of_test_() ->
    lists:map(
      t(one_of("abc")),
      [{{[], "b"}, "b"},
       {{"cc", "c"}, "ccc"},
       {{<<"cc">>, <<"c">>}, <<"ccc">>},
       {nomatch, "d"},
       {nomatch, ""}]).


tag_test_() ->
    lists:map(
      t(tag("Hello")),
      [{{", World!", "Hello"}, "Hello, World!"},
       {{<<", World!">>, <<"Hello">>}, <<"Hello, World!">>},
       {nomatch, "Something"},
       {nomatch, ""}]).

tag_no_case_test_() ->
    lists:map(
      t(tag_no_case("hello")),
      [{{", World!", "Hello"}, "Hello, World!"},
       {{<<", World!">>, <<"Hello">>}, <<"Hello, World!">>},
       {{", World!", "hello"}, "hello, World!"},
       {{", World!", "HeLlO"}, "HeLlO, World!"},
       {nomatch, "Something"},
       {nomatch, ""}]).

re_b_plus_test_() ->
    lists:map(
      t(re("b+")),
      [{nomatch, "aaabbb"},
       {{"aaa", "bbb"}, "bbbaaa"},
       {{<<"aaa">>, <<"bbb">>}, <<"bbbaaa">>}]).

re_az_star_test_() ->
    lists:map(
      t(re("[a-z_]*")),
      [{{"123", ""}, "123"},
       {{"", "bbbaaa"}, "bbbaaa"},
       {{<<"">>, <<"bbbaaa">>}, <<"bbbaaa">>},
       {{"", ""}, ""}]).

re_no_case_test_() ->
    lists:map(
      t(re("inner( join)?")),
      [{{"abc", "inner"}, "innerabc"},
       {{"abc", "inner join"}, "inner joinabc"},
       {nomatch, "outer join"}]).

alpha0_test_() ->
    lists:map(
      t(alpha0()),
      [{{"1c", "ab"}, "ab1c"},
       {{<<"1c">>, <<"ab">>}, <<"ab1c">>},
       {{"1c", []}, "1c"},
       {{[], []}, []}]).

alpha1_test_() ->
    lists:map(
      t(alpha1()),
      [{{"1c", "aB"}, "aB1c"},
       {{<<"1c">>, <<"aB">>}, <<"aB1c">>},
       {nomatch, "1c"},
       {nomatch, ""}]).

alphanumeric0_test_() ->
    lists:map(
      t(alphanumeric0()),
      [{{"%1", "21cZ"}, "21cZ%1"},
       {{<<"%1">>, <<"21cZ">>}, <<"21cZ%1">>},
       {{"&Z21c", ""}, "&Z21c"},
       {{"", ""}, ""}]).

alphanumeric1_test_() ->
    lists:map(
      t(alphanumeric1()),
      [{{"%1", "21cZ"}, "21cZ%1"},
       {{<<"%1">>, <<"21cZ">>}, <<"21cZ%1">>},
       {nomatch, "&H2"},
       {nomatch, ""}]).

digit0_test_() ->
    lists:map(
      t(digit0()),
      [{{"c", "21"}, "21c"},
       {{<<"c">>, <<"21">>}, <<"21c">>},
       {{"", "21"}, "21"},
       {{"a21c", ""}, "a21c"},
       {{"", ""}, ""}]).

digit1_test_() ->
    lists:map(
      t(digit1()),
      [{{"c", "21"}, "21c"},
       {{<<"c">>, <<"21">>}, <<"21c">>},
       {{"", "21"}, "21"},
       {nomatch, "a21c"},
       {nomatch, ""}]).

multispace0_test_() ->
    lists:map(
      t(multispace0()),
      [{{"21c", " \t\n\r"}, " \t\n\r21c"},
       {{<<"21c">>, <<" \t\n\r">>}, <<" \t\n\r21c">>},
       {{"Z21c", ""}, "Z21c"},
       {{"", ""}, ""}]).


multispace1_test_() ->
    lists:map(
      t(multispace1()),
      [{{"21c", " \t\n\r"}, " \t\n\r21c"},
       {{<<"21c">>, <<" \t\n\r">>}, <<" \t\n\r21c">>},
       {nomatch, "Z21c"},
       {nomatch, ""}]).


alt_test_() ->
    lists:map(
      t(alt([alpha1(), digit1()])),
      [{{"", "abc"}, "abc"},
       {{"123", "abc"}, "abc123"},
       {{<<"123">>, <<"abc">>}, <<"abc123">>},
       {{"", "123456"}, "123456"},
       {{"abc", "123456"}, "123456abc"},
       {nomatch, " "}]).

permutation_test_() ->
    lists:map(
      t(permutation([alpha1(), digit1()])),
      [{{"", ["abc", "123"]}, "abc123"},
       {{"", ["123", "abc"]}, "123abc"},
       {{<<"">>, [<<"123">>, <<"abc">>]}, <<"123abc">>},
       {nomatch, "abc"}]).

sequence_test_() ->
    lists:map(
      t(sequence([alpha1(), digit1()])),
      [{{"", ["abc", "123"]}, "abc123"},
       {{<<"">>, [<<"abc">>, <<"123">>]}, <<"abc123">>},
       {nomatch, "123abc"},
       {nomatch, "abc"}]).

flat_sequence_test_() ->
    lists:map(
      t(sequence([alpha1(),
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
      t(all_consuming(alpha1())),
      [{{"", "abcd"}, "abcd"},
       {{<<"">>, <<"abcd">>}, <<"abcd">>},
       {nomatch, "abcd;"},
       {nomatch, "123abcd;"}]).

delimited_test_() ->
    lists:map(
      t(delimited(tag("("), tag("abc"), tag(")"))),
      [{{"", "abc"}, "(abc)"},
       {{"def", "abc"}, "(abc)def"},
       {{<<"def">>, <<"abc">>}, <<"(abc)def">>},
       {nomatch, ""},
       {nomatch, "123"}]).

pair_test_() ->
    lists:map(
      t(pair(tag("abc"), tag("def"))),
      [{{"", ["abc", "def"]}, "abcdef"},
       {{"ghi", ["abc", "def"]}, "abcdefghi"},
       {{<<"ghi">>, [<<"abc">>, <<"def">>]}, <<"abcdefghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).

preceded_test_() ->
    lists:map(
      t(preceded(tag("abc"), tag("def"))),
      [{{"", "def"}, "abcdef"},
       {{"ghi", "def"}, "abcdefghi"},
       {{<<"ghi">>, <<"def">>}, <<"abcdefghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).

separated_pair_test_() ->
    lists:map(
      t(separated_pair(tag("abc"), tag("|"), tag("def"))),
      [{{"", ["abc", "def"]}, "abc|def"},
       {{"ghi", ["abc", "def"]}, "abc|defghi"},
       {{<<"ghi">>, [<<"abc">>, <<"def">>]}, <<"abc|defghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).

terminated_test_() ->
    lists:map(
      t(terminated(tag("abc"), tag("def"))),
      [{{"", "abc"}, "abcdef"},
       {{"ghi", "abc"}, "abcdefghi"},
       {{<<"ghi">>, <<"abc">>}, <<"abcdefghi">>},
       {nomatch, ""},
       {nomatch, "123"}]).


map_result_test_() ->
    lists:map(
      t(map_result(digit1(), fun string:length/1)),
      [{{"", 6}, "123456"},
       {{<<"">>, 6}, <<"123456">>},
       {nomatch, "abc"}]).

map_parser_test_() ->
    lists:map(
      t(map_parser(take(5), digit1())),
      [{{"", "12345"}, "12345"},
       {{"abc", "12345"}, "12345abc"},
       {{<<"abc">>, <<"12345">>}, <<"12345abc">>},
       {{"", "123"}, "123ab"},
       {{"c", "123"}, "123abc"},
       {nomatch, "123"}]).

opt_test_() ->
    lists:map(
      t(opt(alpha1())),
      [{{";", "abcd"}, "abcd;"},
       {{<<";">>, <<"abcd">>}, <<"abcd;">>},
       {{"123;", none}, "123;"}]).

value_test_() ->
    lists:map(
      t(value(1234, alpha1())),
      [{{";", 1234}, "abcd;"},
       {{<<";">>, 1234}, <<"abcd;">>},
       {nomatch, "123;"}]).

condition_true_test_() ->
    lists:map(
      t(condition(true, alpha1())),
      [{{";", "abcd"}, "abcd;"},
       {{<<";">>, <<"abcd">>}, <<"abcd;">>},
       {nomatch, "123;"}]).

condition_false_test_() ->
    lists:map(
      t(condition(false, alpha1())),
      [{{"abcd;", none}, "abcd;"},
       {{<<"abcd;">>, none}, <<"abcd;">>}]).


peek_test_() ->
    lists:map(
      t(peek(alpha1())),
      [{{"abcd;", "abcd"}, "abcd;"},
       {nomatch, "123;"}]).

eof_test_() ->
    lists:map(
      t(eof()),
      [{nomatch, "abc"},
       {{"", ""}, ""},
       {{<<>>, <<>>}, <<>>}]).


is_not_test_() ->
    lists:map(
      t(is_not(alpha1())),
      [{nomatch, "abc"},
       {{"", "123"}, "123"},
       {{<<"">>, <<"123">>}, <<"123">>},
       {{<<>>, <<>>}, <<>>}]).


hex_digit0_test_() ->
    lists:map(
      t(hex_digit0()),
      [{{"Z", "21c"}, "21cZ"},
       {{<<"Z">>, <<"21c">>}, <<"21cZ">>},
       {{"Z21c", ""}, "Z21c"},
       {{"", ""}, ""}]).


hex_digit1_test_() ->
    lists:map(
      t(hex_digit1()),
      [{{"Z", "21c"}, "21cZ"},
       {{<<"Z">>, <<"21c">>}, <<"21cZ">>},
       {nomatch, "Z21c"},
       {nomatch, ""}]).

take_test_() ->
    lists:map(
      t(take(5)),
      [{{"", "12345"}, "12345"},
       {{"6", "12345"}, "123456"},
       {{<<"6">>, <<"12345">>}, <<"123456">>},
       {nomatch, "1234"},
       {nomatch, ""}]).


lsn_test_() ->
    lists:map(
      t(sequence([hex_digit1(), tag("/"), hex_digit1()])),
      [{{"", ["0", "/", "0"]}, "0/0"},
      {{<<"">>,[<<"0">>, <<"/">>, <<"0">>]}, <<"0/0">>}]).

many1_test_() ->
    lists:map(
      t(many1(alt([tag("abc"), tag("def")]))),
      [{{"", ["abc", "abc"]}, "abcabc"},
       {{"", ["abc", "def"]}, "abcdef"},
       {{"123", ["abc"]}, "abc123"},
       {{<<"123">>, [<<"abc">>]}, <<"abc123">>},
       {nomatch, "123123"},
       {nomatch, ""}]).

separated_list0_test_() ->
    lists:map(
      t(separated_list0(tag("|"), tag("abc"))),
      [{{"", ["abc", "abc", "abc"]}, "abc|abc|abc"},
       {{"123abc", ["abc"]}, "abc123abc"},
       {{"|def", ["abc"]}, "abc|def"},
       {{"", []}, ""},
       {{"def|abc", []}, "def|abc"}]).

separated_list1_test_() ->
    lists:map(
      t(separated_list1(tag("|"), tag("abc"))),
      [{{"", ["abc", "abc", "abc"]}, "abc|abc|abc"},
       {{"123abc", ["abc"]}, "abc123abc"},
       {{<<"123abc">>, [<<"abc">>]}, <<"abc123abc">>},
       {{"|def", ["abc"]}, "abc|def"},
       {nomatch, ""},
       {nomatch, "def|abc"}]).

none_of_test_() ->
    lists:map(
      t(none_of("abc")),
      [{{"", "z"}, "z"},
       {nomatch, "a"},
       {nomatch, ""},
       {{"a", "z"}, "za"},
       {{<<"a">>, <<"z">>}, <<"za">>}]).

i128_little_test_() ->
    lists:map(
      t(scran_number:i128(little)),
      [{{<<>>,
         16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d},
        <<16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d:128/little>>},
       {{<<>>,
         -16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d},
        <<-16#1a2b3c4d1a2b3c4d1a2b3c4d1a2b3c4d:128/little>>}]).

u_test_() ->
    Endianess = [little, big],
    Sizes = [8, 16, 24, 32, 40, 48, 56, 64, 72,
             80, 88, 96, 104, 112, 120, 128],
    lists:flatmap(
      fun u/1,
      [{Endian, Size} || Endian <- Endianess,
                         Size <- Sizes]).

u({Endianess, Size}) ->
    lists:map(
      t(scran_number:u(Endianess, Size)),
      [{{<<>>, 0}, u(Endianess, Size, 0)},
       {{<<>>, 1}, u(Endianess, Size, 1)},
       {{<<>>, 1 bsl Size - 1}, u(Endianess, Size, 1 bsl Size - 1)},
       {nomatch, <<>>},
       {nomatch, u(Endianess, Size + 1, 1)},
       {nomatch, u(Endianess, Size - 1, 1)}]).


u(little, Size, Value) ->
    <<Value:Size/little>>;
u(big, Size, Value) ->
    <<Value:Size/big>>.


bytes_take_test_() ->
    lists:map(
      t(scran_bytes:take(6)),
      [{{<<>>, <<"123456">>}, <<"123456">>},
       {{<<"321">>, <<"123456">>}, <<"123456321">>},
       {nomatch, <<"123">>}]).

bytes_null_terminated_test_() ->
    lists:map(
      t(scran_bytes:null_terminated()),
      [{{<<>>, <<"null">>}, <<"null", 0:8>>},
       {nomatch, <<"missing null">>},
       {{<<"terminated">>, <<"null">>}, <<"null", 0:8, "terminated">>}]).

bytes_length_encoded_test_() ->
    lists:map(
      t(scran_bytes:length_encoded(scran_number_be:u16())),
      [{{<<" World!">>, <<"Hello">>}, <<5:16, "Hello World!">>},
       {nomatch, <<5:16, "He">>},
       {nomatch, <<>>}]).

result_into_bits_test_() ->
    lists:map(
      t(into_bits(scran_number_be:u16(), 16)),
      [{{<<>>, <<5:16>>}, <<5:16>>},
       {{<<"abc">>, <<5:16>>}, <<5:16, "abc">>},
       {nomatch, <<5:8>>},
       {nomatch, <<>>}]).

bits_into_boolean_test_() ->
    lists:map(
      t(into_boolean()),
      [{{<<>>, true}, <<1:1>>},
       {{<<>>, false}, <<0:1>>},
       {{<<0:1>>, true}, <<2#10:2>>},
       {{<<1:1>>, false}, <<2#01:2>>},
       {nomatch, <<>>}]).

bits_into_many1_boolean_test_() ->
    lists:map(
      t(map_parser(into_bits(scran_number_be:u(3), 3),
                   many1(into_boolean()))),
      [{{<<>>, [true, false, true]}, <<2#101:3>>},
       {{<<>>, [false, true, false]}, <<2#010:3>>}]).


t(Parser) ->
    fun
        ({Expected, Input} = Test) ->
            {nm(Test), ?_assertEqual(Expected, Parser(Input))}
    end.


nm(Test) ->
    iolist_to_binary(io_lib:fwrite("~p", [Test])).
