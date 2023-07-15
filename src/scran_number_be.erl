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


%% @doc Parser combinators that return numbers from big endian bytes.

-module(scran_number_be).


-define(ENDIAN, big).
-export([i/1]).
-export([i104/0]).
-export([i112/0]).
-export([i120/0]).
-export([i128/0]).
-export([i16/0]).
-export([i24/0]).
-export([i32/0]).
-export([i40/0]).
-export([i48/0]).
-export([i56/0]).
-export([i64/0]).
-export([i72/0]).
-export([i8/0]).
-export([i80/0]).
-export([i88/0]).
-export([i96/0]).
-export([u/1]).
-export([u104/0]).
-export([u112/0]).
-export([u120/0]).
-export([u128/0]).
-export([u16/0]).
-export([u24/0]).
-export([u32/0]).
-export([u40/0]).
-export([u48/0]).
-export([u56/0]).
-export([u64/0]).
-export([u72/0]).
-export([u8/0]).
-export([u80/0]).
-export([u88/0]).
-export([u96/0]).


-spec i(scran_number:bit_size()) -> scran_number:iparser().

i(Size) ->
    scran_number:i(?ENDIAN, Size).


-spec i8() -> scran_number:iparser().
-spec i16() -> scran_number:iparser().
-spec i24() -> scran_number:iparser().
-spec i32() -> scran_number:iparser().
-spec i40() -> scran_number:iparser().
-spec i48() -> scran_number:iparser().
-spec i56() -> scran_number:iparser().
-spec i64() -> scran_number:iparser().
-spec i72() -> scran_number:iparser().
-spec i80() -> scran_number:iparser().
-spec i88() -> scran_number:iparser().
-spec i96() -> scran_number:iparser().
-spec i104() -> scran_number:iparser().
-spec i112() -> scran_number:iparser().
-spec i120() -> scran_number:iparser().
-spec i128() -> scran_number:iparser().


i8() -> i(8).
i16() -> i(16).
i24() -> i(24).
i32() -> i(32).
i40() -> i(40).
i48() -> i(48).
i56() -> i(56).
i64() -> i(64).
i72() -> i(72).
i80() -> i(80).
i88() -> i(88).
i96() -> i(96).
i104() -> i(104).
i112() -> i(112).
i120() -> i(120).
i128() -> i(128).


-spec u8() -> scran_number:uparser().
-spec u16() -> scran_number:uparser().
-spec u24() -> scran_number:uparser().
-spec u32() -> scran_number:uparser().
-spec u40() -> scran_number:uparser().
-spec u48() -> scran_number:uparser().
-spec u56() -> scran_number:uparser().
-spec u64() -> scran_number:uparser().
-spec u72() -> scran_number:uparser().
-spec u80() -> scran_number:uparser().
-spec u88() -> scran_number:uparser().
-spec u96() -> scran_number:uparser().
-spec u104() -> scran_number:uparser().
-spec u112() -> scran_number:uparser().
-spec u120() -> scran_number:uparser().
-spec u128() -> scran_number:uparser().


-spec u(scran_number:bit_size()) -> scran_number:uparser().

u(Size) ->
    scran_number:u(?ENDIAN, Size).


u8() -> u(8).
u16() -> u(16).
u24() -> u(24).
u32() -> u(32).
u40() -> u(40).
u48() -> u(48).
u56() -> u(56).
u64() -> u(64).
u72() -> u(72).
u80() -> u(80).
u88() -> u(88).
u96() -> u(96).
u104() -> u(104).
u112() -> u(112).
u120() -> u(120).
u128() -> u(128).
