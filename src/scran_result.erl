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


%% @doc Parser combinators that map the result.

-module(scran_result).


-export([ignore/1]).
-export([into_atom/1]).
-export([into_bits/2]).
-export([into_existing_atom/1]).
-export([into_integer/1]).
-export([into_integer/2]).
-export([into_map/1]).
-export([into_snake_case/1]).
-export([kv/2]).
-import(scran_combinator, [map_result/2]).


%% @doc The parser result is converted into a 2-tuple with the supplied
%% key as the first element.

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


%% @doc The parser result is converted into snake case.

-spec into_snake_case(
        scran:parser(unicode:chardata(),
                     unicode:chardata())) ->
          scran:parser(unicode:chardata(),
                       unicode:chardata()).

into_snake_case(Parser) ->
    fun
        (Input) ->
            (map_result(
               Parser,
               fun
                   (Result) when is_binary(Result) ->
                       snake_case(Result);

                   (Result) when is_list(Result) ->
                       binary_to_list(snake_case(Result))

               end))(Input)
    end.


-spec snake_case(unicode:chardata()) -> binary().

snake_case(Result) ->
    iolist_to_binary(
      lists:join(
        "_",
        string:split(
          string:lowercase(Result),
          " ",
          all))).


%% @doc The parser result is converted into an atom.

-spec into_atom(
        scran:parser(unicode:chardata(),
                     unicode:chardata())) ->
          scran:parser(unicode:chardata(),
                       atom()).

into_atom(Parser) ->
    fun
        (Input) ->
            (map_result(
               Parser,
               fun
                   (Result) when is_binary(Result) ->
                       binary_to_atom(Result);

                   (Result) when is_list(Result) ->
                       list_to_atom(Result)
               end))(Input)
    end.


%% @doc The parser result is converted into an existing atom.

-spec into_existing_atom(
        scran:parser(unicode:chardata(),
                     unicode:chardata())) ->
          scran:parser(unicode:chardata(),
                       atom()).

into_existing_atom(Parser) ->
    fun
        (Input) ->
            try
                (map_result(
                   Parser,
                   fun
                       (Result) when is_binary(Result) ->
                           binary_to_existing_atom(Result);

                       (Result) when is_list(Result) ->
                           list_to_existing_atom(Result)
                   end))(Input)

            catch
                error:badarg ->
                    nomatch
            end
    end.


%% @doc When the parser result is a list of key/value tuples it is
%% converted into an Erlang map.

-spec into_map(scran:parser(I, [{K, V}])) -> scran:parser(I, #{K => V}).

into_map(Parser) ->
    fun
        (Input) ->
            (map_result(Parser, fun maps:from_list/1))(Input)
    end.


%% @doc The parser result is mapped into a bitstring of the supplied
%% length.

-spec into_bits(scran:parser(I, integer()),
                pos_integer()) -> scran:parser(I, bitstring()).

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


%% @doc Convert the parser result into a base 10 integer.

into_integer(Parser) ->
    ?FUNCTION_NAME(Parser, 10).


%% @doc Convert the parser result into an integer with the supplied
%% base.

into_integer(Parser, Base) ->
    fun
        (Input) ->
            (map_result(
               Parser,
               fun
                   (Result) when is_binary(Result) ->
                       binary_to_integer(Result, Base);

                   (Result) when is_list(Result) ->
                       list_to_integer(Result, Base)
               end))(Input)
    end.


%% @doc Ignore the result of the embedded parser.

-spec ignore(scran:parser()) -> scran:parser().

ignore(Parser) ->
    map_result(Parser,
               fun
                   (_) ->
                       none
               end).
