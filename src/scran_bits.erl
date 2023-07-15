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


%% @doc Parser combinators that deal with bit inputs

-module(scran_bits).


-export([into_boolean/0]).


%% @doc Takes 1 bit from the input converting it into a boolean.
%% @returns A tuple of the remaining input and a boolean result.

-spec into_boolean() -> scran:parser(<<_:1, _:_*1>>, boolean()).

into_boolean() ->
    fun
        (<<0:1, Remaining/bits>>) ->
            {Remaining, false};

        (<<1:1, Remaining/bits>>) ->
            {Remaining, true};

        (_) ->
            nomatch
    end.
