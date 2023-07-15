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


%% @doc Parser combinators that operate on sequences of input.

-module(scran_sequence).


-feature(maybe_expr, enable).


-export([delimited/3]).
-export([pair/2]).
-export([preceded/2]).
-export([separated_pair/3]).
-export([sequence/1]).
-export([terminated/2]).
-include_lib("kernel/include/logger.hrl").


%% @doc Matches an object from the first parser and discards it, then
%% gets an object from the second parser, and finally matches an
%% object from the third parser and discards it.
%%

-spec delimited(scran:parser(),
                scran:parser(),
                scran:parser()) -> scran:parser().

delimited(First, Second, Third) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{first => scran_debug:pp(First),
                         second => scran_debug:pp(Second),
                         third => scran_debug:pp(Third),
                         input => Input}),
            maybe
                {SecondInput, _} ?= First(Input),
                {ThirdInput, Result} ?= Second(SecondInput),
                {Remaining, _} ?= Third(ThirdInput),
                ?LOG_DEBUG(#{remaining => Remaining, result => Result}),
                {Remaining, Result}
            end
    end.


%% @doc Gets an object from the first parser, then gets another object
%% from the second parser.
%%

-spec pair(scran:parser(), scran:parser()) -> scran:parser().

pair(First, Second) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{first => scran_debug:pp(First),
                         second => scran_debug:pp(Second),
                         input => Input}),
            maybe
                {SecondInput, FirstResult} ?= First(Input),
                {Remaining, SecondResult} ?= Second(SecondInput),
                ?LOG_DEBUG(#{remaining => Remaining,
                             result => [FirstResult, SecondResult]}),
                {Remaining, [FirstResult, SecondResult]}
            end
    end.


%% @doc Matches an object from the first parser and discards it, then
%% gets an object from the second parser.
%%

-spec preceded(scran:parser(), scran:parser()) -> scran:parser().

preceded(First, Second) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{first => scran_debug:pp(First),
                         second => scran_debug:pp(Second),
                         input => Input}),
            maybe
                {SecondInput, _} ?= First(Input),
                {Remaining, Result} ?= Second(SecondInput),
                ?LOG_DEBUG(#{remaining => Remaining,
                             result => Result}),
                {Remaining, Result}
            end
    end.


%% @doc Gets an object from the first parser, then matches an object
%% from the separator and discards it, then gets another object from
%% the second parser.
%%

-spec separated_pair(scran:parser(),
                     scran:parser(),
                     scran:parser()) -> scran:parser().

separated_pair(First, Separator, Second) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{first => scran_debug:pp(First),
                         separator => scran_debug:pp(Separator),
                         second => scran_debug:pp(Second),
                         input => Input}),
            maybe
                {SeparatorInput, FirstResult} ?= First(Input),
                {SecondInput, _} ?= Separator(SeparatorInput),
                {Remaining, SecondResult} ?= Second(SecondInput),
                ?LOG_DEBUG(#{remaining => Remaining,
                             result => [FirstResult, SecondResult]}),
                {Remaining, [FirstResult, SecondResult]}
            end
    end.


%% @doc Gets an object from the first parser, then matches an object
%% from the second parser and discards it.
%%

-spec terminated(scran:parser(), scran:parser()) -> scran:parser().

terminated(First, Second) ->
    fun
        (Input) ->
            ?LOG_DEBUG(#{first => scran_debug:pp(First),
                         second => scran_debug:pp(Second),
                         input => Input}),
            maybe
                {SecondInput, FirstResult} ?= First(Input),
                {Remaining, _} ?= Second(SecondInput),
                ?LOG_DEBUG(#{remaining => Remaining,
                             result => FirstResult}),
                {Remaining, FirstResult}
            end
    end.


%% @doc The input is applied to each step in the sequence.

%% -spec sequence([scran:parser()]) -> scran:parser().

sequence(Steps) ->
    fun
        (Input) ->
            ?FUNCTION_NAME(Steps, {Input, []})
    end.


-spec sequence([scran:parser()],
               {scran:input(), [scran:result()]}) -> scran:result().

sequence([Step | Steps], {Input, Results}) ->
    case Step(Input) of
        {Remaining, none = Result} ->
            ?LOG_DEBUG(#{step => scran_debug:pp(Step),
                         input => Input,
                         result => Result,
                         results => Results,
                         remaining => Remaining}),
            ?FUNCTION_NAME(Steps, {Remaining, Results});

        {Remaining, Result} when is_binary(Input), is_list(Result) ->
            ?LOG_DEBUG(#{step => scran_debug:pp(Step),
                         input => Input,
                         result => Result,
                         results => Results,
                         remaining => Remaining}),
            ?FUNCTION_NAME(
               Steps,
               {Remaining, lists:reverse(Result) ++ Results});

        {Remaining, Result} when is_list(Result) ->
            ?LOG_DEBUG(#{step => scran_debug:pp(Step),
                         input => Input,
                         result => Result,
                         results => Results,
                         remaining => Remaining}),
            ?FUNCTION_NAME(
               Steps,
               {Remaining,
                case io_lib:printable_list(Result) of
                    true ->
                        [Result | Results];

                    false ->
                        lists:reverse(Result) ++ Results
                end});

        {Remaining, Result} ->
            ?LOG_DEBUG(#{step => scran_debug:pp(Step),
                         result => Result,
                         input => Input,
                         results => Results,
                         remaining => Remaining}),
            ?FUNCTION_NAME(
               Steps,
               {Remaining, [Result | Results]});

        nomatch ->
            ?LOG_DEBUG(#{input => Input,
                         nomatch => scran_debug:pp(Step)}),
            nomatch
    end;

sequence([], {Remainder, Results}) ->
    ?LOG_DEBUG(#{remainder => Remainder, results => Results}),
    {Remainder, lists:reverse(Results)}.
