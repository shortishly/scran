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


-module(scran_character_complete).


-feature(maybe_expr, enable).


-export([alpha0/0]).
-export([alpha1/0]).
-export([alphanumeric0/0]).
-export([alphanumeric1/0]).
-export([digit0/0]).
-export([digit1/0]).
-export([hex_digit0/0]).
-export([hex_digit1/0]).
-export([multispace0/0]).
-export([multispace1/0]).
-export([none_of/1]).
-export([one_of/1]).
-export([re/1]).
-export([re_no_case/1]).
-export([tag/1]).
-export([tag_no_case/1]).
-export([take/1]).
-include_lib("kernel/include/logger.hrl").


-spec take(pos_integer()) -> scran:parser().

take(N) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{n => N, input => Input}),
            maybe
                true ?= string:length(Input) >= N,
                {string:slice(Input, N), string:slice(Input, 0, N)}
            else
                false ->
                    nomatch
            end
    end.


-spec re(iodata()) -> scran:parser().

re(Regex) ->
    re(Regex, [anchored]).


-spec re_no_case(iodata()) -> scran:parser().

re_no_case(Regex) ->
    re(Regex, [anchored, caseless]).


-type compile_option() :: unicode | anchored | caseless | dollar_endonly
                        | dotall | extended | firstline | multiline
                        | no_auto_capture | dupnames | ungreedy
                        | {newline, nl_spec()}
                        | bsr_anycrlf | bsr_unicode
                        | no_start_optimize | ucp | never_utf.

-type nl_spec() :: cr | crlf | lf | anycrlf | any.

-spec re(iodata(), [compile_option()]) -> scran:parser().

re(Regex, CompileOptions) ->
    {ok, MP} = re:compile(Regex, CompileOptions),
    fun
        (Input) ->
            ?LOG_DEBUG(#{regex => Regex, input => Input}),
            maybe
                {match, [{Begin, End} | _]} ?= re:run(Input, MP),
                {string:slice(Input, End),
                 string:slice(Input, Begin, End)}
            end
    end.


-spec tag(unicode:chardata()) -> scran:parser().

tag(Tag) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{tag => Tag, input => Input}),
            case string:prefix(Input, Tag) of
                nomatch ->
                    nomatch;

                Remainder when is_binary(Input),
                               is_list(Tag) ->
                    {Remainder, list_to_binary(Tag)};

                Remainder ->
                    {Remainder, Tag}
            end
    end.

-spec tag_no_case(unicode:chardata()) -> scran:parser().

tag_no_case(Tag) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{tag => Tag, input => Input}),
            case string:prefix(
                   string:lowercase(Input),
                   string:lowercase(Tag)) of

                nomatch ->
                    nomatch;

                _Remainder ->
                    {string:slice(Input, string:length(Tag)), string:slice(Input, 0, string:length(Tag))}
            end
    end.


-spec one_of([unicode:chardata()]) -> scran:parser().

one_of(Choice) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{choice => Choice, input => Input}),
            maybe
                false ?= string:is_empty(Input),
                FirstCharacter = string:slice(Input, 0, 1),
                true ?= string:find(Choice, FirstCharacter) /= nomatch,
                {string:slice(Input, 1), FirstCharacter}

            else
                Failed when is_boolean(Failed) ->
                    nomatch
            end
    end.

-spec none_of([unicode:chardata()]) -> scran:parser().

none_of(Choice) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{choice => Choice, input => Input}),
            maybe
                false ?= string:is_empty(Input),
                FirstCharacter = string:slice(Input, 0, 1),
                true ?= string:find(Choice, FirstCharacter) == nomatch,
                {string:slice(Input, 1), FirstCharacter}

            else
                Failed when is_boolean(Failed) ->
                    nomatch
            end
    end.


%% Recognizes zero or more lowercase and uppercase ASCII alphabetic
%% characters: a-z, A-Z
alpha0() ->
    fun
        (Input) ->
            (zero_or_more(alpha()))(Input)
    end.

%% Recognizes one or more lowercase and uppercase ASCII alphabetic
%% characters: a-z, A-Z
alpha1() ->
    fun
        (Input) ->
            (at_least_one(alpha()))(Input)
    end.


%% Recognizes zero or more ASCII numerical and alphabetic characters:
%% 0-9, a-z, A-Z
alphanumeric0() ->
    fun
        (Input) ->
            (zero_or_more(alphanumeric()))(Input)
    end.

%% Recognizes one or more ASCII numerical and alphabetic characters:
%% 0-9, a-z, A-Z
alphanumeric1() ->
    fun
        (Input) ->
            (at_least_one(alphanumeric()))(Input)
    end.

%% Recognizes zero or more ASCII numerical characters: 0-9
digit0() ->
    fun
        (Input) ->
            (zero_or_more(numeric()))(Input)
    end.

%% Recognizes one or more ASCII numerical characters: 0-9
digit1() ->
    fun
        (Input) ->
            (at_least_one(numeric()))(Input)
    end.

%% Recognizes zero or more spaces, tabs, carriage returns and line
%% feeds.
multispace0() ->
    fun
        (Input) ->
            (zero_or_more(whitespace()))(Input)
    end.

%% Recognizes zero or more ASCII hexadecimal numerical characters:
%% 0-9, A-F, a-f
hex_digit0() ->
    fun
        (Input) ->
            (zero_or_more(hex()))(Input)
    end.

hex_digit1() ->
    fun
        (Input) ->
            (at_least_one(hex()))(Input)
    end.


%% Recognizes one or more spaces, tabs, carriage returns and line
%% feeds.
multispace1() ->
    fun
        (Input) ->
            (at_least_one(whitespace()))(Input)
    end.


at_least_one(Characters) ->
    fun
        (Input) ->
            maybe
                {_, Matched} = Result = (zero_or_more(Characters))(Input),
                true ?= string:length(Matched) >= 1,
                Result
            else
                false ->
                    nomatch
            end
    end.

zero_or_more(Characters) ->
    fun
        (Input) ->
            flip(string:take(Input, Characters))
    end.


flip(Tuple) ->
    list_to_tuple(lists:reverse(tuple_to_list(Tuple))).


alpha() ->
    lists:seq($a, $z) ++ lists:seq($A, $Z).


numeric() ->
    lists:seq($0, $9).


alphanumeric() ->
    alpha() ++ numeric().

hex() ->
    numeric() ++ lists:seq($a, $f) ++ lists:seq($A, $F).

whitespace() ->
    "\s\t\n\r".
