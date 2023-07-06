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


-module(scran_combinator).

-feature(maybe_expr, enable).


-export([all_consuming/1]).
-export([condition/2]).
-export([eof/0]).
-export([ignore_result/1]).
-export([is_not/1]).
-export([map_parser/2]).
-export([map_result/2]).
-export([opt/1]).
-export([peek/1]).
-export([value/2]).
-export_type([mapper/0]).
-include_lib("kernel/include/logger.hrl").


%% Succeeds if all the input has been consumed by its child parser.
%%

-spec all_consuming(scran:parser()) -> scran:parser().

all_consuming(Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {_, Input} ?= Parser(Input)

            else
                _ ->
                    nomatch
            end
    end.


%% Maps a function on the result of a parser.
%%

-type mapper() :: fun((unicode:chardata()) -> any()).

-spec map_result(scran:parser(), mapper()) -> scran:parser().

map_result(Parser, Function) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {Remaining, Result} ?= Parser(Input),
                {Remaining, Function(Result)}
            end
    end.


-spec ignore_result(scran:parser()) -> scran:parser().

ignore_result(Parser) ->
    map_result(Parser,
               fun
                   (_) ->
                       none
               end).



%% Applies a parser over the result of another one.

-spec map_parser(scran:parser(), scran:parser()) -> scran:parser().

map_parser(Parser, AppliedParser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {Remaining, Result} ?= Parser(Input),
                {_, AppliedResult} ?= AppliedParser(Result),
                {Remaining, AppliedResult}
            end
    end.


%% Optional parser, will return none if the option is not taken.

-spec opt(scran:parser()) -> scran:parser().

opt(Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {_, _} ?= Parser(Input)
            else
                nomatch ->
                    {Input, none}
            end
    end.


%% Returns the provided value if the child parser succeeds.

-spec value(any(), scran:parser()) -> scran:parser().

value(Return, Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {Remaining, _} ?= Parser(Input),
                {Remaining, Return}
            end
    end.


%% Calls the parser if the condition is met.

-spec condition(boolean(), scran:parser()) -> scran:parser().

condition(false, Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            {Input, none}
    end;

condition(true, Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {_, _} ?= Parser(Input)
            end
    end.


%% Tries to apply its parser without consuming the input.


-spec peek(scran:parser()) -> scran:parser().

peek(Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {_, Matched} ?= Parser(Input),
                ?LOG_DEBUG(#{input => Input,
                             matched => Matched,
                             parser => scran_debug:pp(Parser)}),
                {Input, Matched}
            end
    end.

%% Returns its input if it is at the end of input data.

-spec eof() -> scran:parser().

eof() ->
    fun
        (Input) ->
            case string:is_empty(Input) of
                true ->
                    {Input, Input};

                false ->
                    nomatch
            end
    end.


%% Succeeds if the child parser returns an error.

-spec is_not(scran:parser()) -> scran:parser().

is_not(Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            case Parser(Input) of
                nomatch when is_binary(Input) ->
                    {<<>>, Input};

                nomatch ->
                    {"", Input};

                {_Input, _Matched} ->
                    nomatch
            end
    end.
