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


-module(scran_number).

-feature(maybe_expr, enable).

-export([i/2]).
-export([i104/1]).
-export([i112/1]).
-export([i120/1]).
-export([i128/1]).
-export([i16/1]).
-export([i24/1]).
-export([i32/1]).
-export([i40/1]).
-export([i48/1]).
-export([i56/1]).
-export([i64/1]).
-export([i72/1]).
-export([i8/1]).
-export([i80/1]).
-export([i88/1]).
-export([i96/1]).
-export([u/2]).
-export([u104/1]).
-export([u112/1]).
-export([u120/1]).
-export([u128/1]).
-export([u16/1]).
-export([u24/1]).
-export([u32/1]).
-export([u40/1]).
-export([u48/1]).
-export([u56/1]).
-export([u64/1]).
-export([u72/1]).
-export([u8/1]).
-export([u80/1]).
-export([u88/1]).
-export([u96/1]).
-export_type([bit_size/0]).
-export_type([endianess/0]).
-export_type([iparser/0]).
-export_type([uparser/0]).


-type endianess() :: little | big.

-type bit_size() :: pos_integer().

-type iparser() :: scran:parser(binary(), integer()).


-spec i8(endianess()) -> iparser().
-spec i16(endianess()) -> iparser().
-spec i24(endianess()) -> iparser().
-spec i32(endianess()) -> iparser().
-spec i40(endianess()) -> iparser().
-spec i48(endianess()) -> iparser().
-spec i56(endianess()) -> iparser().
-spec i64(endianess()) -> iparser().
-spec i72(endianess()) -> iparser().
-spec i80(endianess()) -> iparser().
-spec i88(endianess()) -> iparser().
-spec i96(endianess()) -> iparser().
-spec i104(endianess()) -> iparser().
-spec i112(endianess()) -> iparser().
-spec i120(endianess()) -> iparser().
-spec i128(endianess()) -> iparser().

i8(Endianess) -> i(Endianess, 8).
i16(Endianess) -> i(Endianess, 16).
i24(Endianess) -> i(Endianess, 24).
i32(Endianess) -> i(Endianess, 32).
i40(Endianess) -> i(Endianess, 40).
i48(Endianess) -> i(Endianess, 48).
i56(Endianess) -> i(Endianess, 56).
i64(Endianess) -> i(Endianess, 64).
i72(Endianess) -> i(Endianess, 72).
i80(Endianess) -> i(Endianess, 80).
i88(Endianess) -> i(Endianess, 88).
i96(Endianess) -> i(Endianess, 96).
i104(Endianess) -> i(Endianess, 104).
i112(Endianess) -> i(Endianess, 112).
i120(Endianess) -> i(Endianess, 120).
i128(Endianess) -> i(Endianess, 128).


-spec i(endianess(), bit_size()) -> iparser().

i(Endianess, Size) -> int(Endianess, signed, Size).

-type uparser() :: scran:parser(binary(), non_neg_integer()).

-spec u8(endianess()) -> uparser().
-spec u16(endianess()) -> uparser().
-spec u24(endianess()) -> uparser().
-spec u32(endianess()) -> uparser().
-spec u40(endianess()) -> uparser().
-spec u48(endianess()) -> uparser().
-spec u56(endianess()) -> uparser().
-spec u64(endianess()) -> uparser().
-spec u72(endianess()) -> uparser().
-spec u80(endianess()) -> uparser().
-spec u88(endianess()) -> uparser().
-spec u96(endianess()) -> uparser().
-spec u104(endianess()) -> uparser().
-spec u112(endianess()) -> uparser().
-spec u120(endianess()) -> uparser().
-spec u128(endianess()) -> uparser().

u8(Endianess) -> u(Endianess, 8).
u16(Endianess) -> u(Endianess, 16).
u24(Endianess) -> u(Endianess, 24).
u32(Endianess) -> u(Endianess, 32).
u40(Endianess) -> u(Endianess, 40).
u48(Endianess) -> u(Endianess, 48).
u56(Endianess) -> u(Endianess, 56).
u64(Endianess) -> u(Endianess, 64).
u72(Endianess) -> u(Endianess, 72).
u80(Endianess) -> u(Endianess, 80).
u88(Endianess) -> u(Endianess, 88).
u96(Endianess) -> u(Endianess, 96).
u104(Endianess) -> u(Endianess, 104).
u112(Endianess) -> u(Endianess, 112).
u120(Endianess) -> u(Endianess, 120).
u128(Endianess) -> u(Endianess, 128).


-spec u(endianess(), bit_size()) -> uparser().

u(Endianess, Size) -> int(Endianess, unsigned, Size).


-spec int(endianess(), signed | unsigned, bit_size()) -> iparser() | uparser().

int(little, signed, Size) ->
    fun
        (<<I:Size/little-signed, Remaining/bytes>>) ->
            {Remaining, I};

        (_) ->
            nomatch
    end;

int(little, unsigned, Size) ->
    fun
        (<<I:Size/little-unsigned, Remaining/bytes>>) ->
            {Remaining, I};

        (_) ->
            nomatch
    end;

int(big, signed, Size) ->
    fun
        (<<I:Size/big-signed, Remaining/bytes>>) ->
            {Remaining, I};

        (_) ->
            nomatch
    end;

int(big, unsigned, Size) ->
    fun
        (<<I:Size/big-unsigned, Remaining/bytes>>) ->
            {Remaining, I};

        (_) ->
            nomatch
    end.
