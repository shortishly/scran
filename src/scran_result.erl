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


-module(scran_result).


-export([into_bits/2]).
-export([into_map/1]).
-export([kv/2]).
-import(scran_combinator, [map_result/2]).


-spec kv(any(), scran:parser(I, O)) -> scran:parser(I, {any(), O}).

kv(Key, Parser) ->
    fun
        (Input) ->
            (map_result(Parser, kv(Key)))(Input)
    end.

kv(Key) ->
    fun
        (Value) ->
            {Key, Value}
    end.


-spec into_map(scran:parser(I, [{K, V}])) -> scran:parser(I, #{K => V}).

into_map(Parser) ->
    fun
        (Input) ->
            (map_result(Parser, fun maps:from_list/1))(Input)
    end.


-spec into_bits(scran:parser(I, integer()), pos_integer()) -> scran:parser(I, bitstring()).

into_bits(Parser, Bits) ->
    fun
        (Input) ->
            (map_result(
               Parser,
               fun
                   (Result) ->
                       <<Result:Bits>>
               end))(Input)
    end.
