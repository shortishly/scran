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


-module(scran).


-feature(maybe_expr, enable).


-export_type([combiner/0]).
-export_type([input/0]).
-export_type([parser/0]).
-export_type([parser/2]).
-export_type([result/0]).
-export_type([result/2]).
-export_type([with_result/0]).
-include_lib("kernel/include/logger.hrl").


-type input() :: unicode:chardata()
               | binary().

-type parser() :: parser(input(), result(input(), any())).

-type parser(I, O) :: fun((I) -> result(I, O)).

-type result() :: result(input(), input()).

-type result(I, O) :: {RemainingInput :: I, ProducedOutput :: O}
                    | none
                    | nomatch.

-type with_result() :: fun ((any()) -> scran:parser()).

-type combiner() :: fun ((any(), any()) -> scran:result()).
