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


%% @doc Parser combinators.

-module(scran_combinator).

-feature(maybe_expr, enable).


-export([all_consuming/1]).
-export([condition/2]).
-export([condition/3]).
-export([eof/0]).
-export([failure/0]).
-export([ignore_result/1]).
-export([is_not/1]).
-export([map_parser/2]).
-export([map_result/2]).
-export([opt/1]).
-export([peek/1]).
-export([rest/0]).
-export([success/1]).
-export([value/2]).
-export_type([mapper/1]).
-include_lib("kernel/include/logger.hrl").


%% @doc Succeeds if all the input has been consumed by its child
%% parser.
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


%% @doc Maps a function on the result of a parser.

-type mapper(O) :: fun((O) -> O).

-spec map_result(scran:parser(I, O), mapper(O)) -> scran:parser(I, O).

map_result(Parser, Function) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {Remaining, Result} ?= Parser(Input),
                {Remaining, Function(Result)}
            end
    end.


%% @doc Ignore the result of the embedded parser.
%% @deprecated Please use {@link scran_result:ignore/1} instead.

-spec ignore_result(scran:parser()) -> scran:parser().

ignore_result(Parser) ->
    scran_result:ignore(Parser).


%% @doc Applies a parser over the result of another one.

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


%% @doc Optional parser, will return none if the option is not taken.

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

%% @doc Returns the provided value if the child parser succeeds.

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


%% @doc Calls the parser if the condition is met.

-spec condition(function() | boolean(), scran:parser()) -> scran:parser().

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
    end;

condition(F, Parser) when is_function(F) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{f => F,
                         input => Input,
                         parser => scran_debug:pp(Parser)}),

            (?FUNCTION_NAME(F(), Parser))(Input)
    end.


-spec  condition(function() | boolean(), scran:parser(), scran:parser()) -> scran:parser().

condition(true, Parser, _) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {_, _} ?= Parser(Input)
            end
    end;

condition(false, _, Parser) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{input => Input, parser => scran_debug:pp(Parser)}),
            maybe
                {_, _} ?= Parser(Input)
            end
    end;

condition(F, TrueBranch, FalseBranch) when is_function(F) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{f => F,
                         input => Input,
                         false_branch => scran_debug:pp(FalseBranch),
                         true_branch => scran_debug:pp(TrueBranch)}),

            (?FUNCTION_NAME(F(), TrueBranch, FalseBranch))(Input)
    end.



%% @doc Tries to apply its parser without consuming the input.

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


%% @doc Returns its input if it is at the end of input data.

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


%% @doc Returns the remaining input.

-spec rest() -> scran:parser().

rest() ->
    fun
        (Input) when is_binary(Input) ->
            {<<>>, Input};

        (Input) ->
            {[], Input}
    end.


%% @doc Succeeds if the child parser returns an error.

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


%% @doc A parser which always succeeds with given value without
%% consuming any input.

-spec success(any()) -> scran:parser().

success(Result) ->
    fun
        (Input) ->
            {Input, Result}
    end.


%% @doc A parser which always fails without consuming any input.

-spec failure() -> scran:parser().

failure() ->
    fun
        (_) ->
            nomatch
    end.
