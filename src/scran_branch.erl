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

%% @doc Parser combinators that deal with branches.

-module(scran_branch).

-feature(maybe_expr, enable).

-import(scran_sequence, [sequence/1]).
-export([alt/1]).
-export([permutation/1]).
-include_lib("kernel/include/logger.hrl").


%% @doc Try each alternative parser in turn until one returns other
%% than nomatch (or none from optional/conditional parsers).

-spec alt([scran:parser()]) -> scran:parser().

alt(Alternatives) ->
    fun
        (Input) ->
            ?FUNCTION_NAME(Alternatives, Input)
    end.


-spec alt([scran:parser()], scran:input()) -> scran:result().

alt([Alternative | Alternatives], Input) ->
    ?LOG_DEBUG(#{alternative => scran_debug:pp(Alternative),
                 input => Input}),
    case Alternative(Input) of
        {_, none} ->
            %% condition or optional parser that has returned none,
            %% continue parseing with remaining alternatives.
            ?LOG_DEBUG(#{nomatch => scran_debug:pp(Alternative)}),
            ?FUNCTION_NAME(Alternatives, Input);

        {_, _} = Result ->
            Result;

        nomatch ->
            ?LOG_DEBUG(#{nomatch => scran_debug:pp(Alternative)}),
            ?FUNCTION_NAME(Alternatives, Input)
    end;

alt([], _) ->
    nomatch.


%% @doc Try each permutation of the supplied parsers until one returns
%% other than nomatch.

-spec permutation([scran:parser()]) -> scran:parser().

permutation(Alternatives) ->
    fun
        (Input) ->
            ?FUNCTION_NAME(permutations(Alternatives), Input)
    end.


-spec permutation([[scran:parser()]], unicode:chardata()) -> scran:result().

permutation([Permutation | Permutations], Input) ->
    maybe
        {_, _} ?= (sequence(Permutation))(Input)

    else
        nomatch ->
            ?FUNCTION_NAME(Permutations, Input)
    end;

permutation([], _) ->
    nomatch.


-spec permutations([scran:parser()]) -> [[scran:parser()]].

permutations([]) ->
    [[]];

permutations(L) ->
    [[H | T] || H <- L, T <- ?FUNCTION_NAME(L -- [H])].
