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


-module(scran_debug).


-export([pp/1]).
-include_lib("kernel/include/logger.hrl").

-spec pp(function()) -> io_lib:chars() | binary().

pp(Function) ->
    {name, Encoded} = erlang:fun_info(Function, name),
    case  string:split(atom_to_list(Encoded), "/") of
        [[$- | F], _] ->
            {env, Env} = erlang:fun_info(Function, env),
            case erlang:fun_info(Function, arity) of
                {arity, 0} ->
                    iolist_to_binary([F, "()"]);

                {arity, _} ->
                    iolist_to_binary(
                      [F,
                       "(",
                       lists:join(", ", mapping(Env)),
                       ")"])
            end;

        [_] ->
              io_lib:format("~p", [Function])
    end.


mapping(Args) ->
    ?LOG_DEBUG(#{args => Args}),
    lists:map(
      fun
          (Arg) when is_function(Arg) ->
              pp(Arg);

          (Arg) when is_tuple(Arg), element(1, Arg) == re_pattern ->
              "re_pattern";

          (Arg) when is_list(Arg) ->
              case io_lib:printable_list(Arg) of
                  false ->
                      ["[", lists:join(", ", ?FUNCTION_NAME(Arg)), "]"];
                  true ->
                      io_lib:format("~p", [Arg])
              end;

          (Otherwise) ->
              io_lib:format("~p", [Otherwise])
      end,
      Args).
