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


%% @doc Parser combinators that return numbers from big or little
%% endian bytes.

-module(scran_number).

-feature(maybe_expr, enable).

-export([f/2]).
-export([f104/1]).
-export([f112/1]).
-export([f120/1]).
-export([f128/1]).
-export([f16/1]).
-export([f24/1]).
-export([f32/1]).
-export([f40/1]).
-export([f48/1]).
-export([f56/1]).
-export([f64/1]).
-export([f72/1]).
-export([f8/1]).
-export([f80/1]).
-export([f88/1]).
-export([f96/1]).
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
-export([precision/1]).
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
-export_type([fparser/0]).
-export_type([iparser/0]).
-export_type([uparser/0]).


-type endianess() :: little | big.

-type bit_size() :: pos_integer().

-type iparser() :: scran:parser(binary(), integer()).
-type fparser() :: scran:parser(binary(), float()).


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


%% @doc return a signed integer from an 8 bit little or big endian
%% quantity.
i8(Endianess) -> i(Endianess, 8).


%% @doc return a signed integer from a 16 bit little or big endian
%% quantity.
i16(Endianess) -> i(Endianess, 16).


%% @doc return a signed integer from a 24 bit little or big endian
%% quantity.
i24(Endianess) -> i(Endianess, 24).

%% @doc return a signed integer from a 32 bit little or big endian
%% quantity.
i32(Endianess) -> i(Endianess, 32).

%% @doc return a signed integer from a 40 bit little or big endian
%% quantity.
i40(Endianess) -> i(Endianess, 40).

%% @doc return a signed integer from a 48 bit little or big endian
%% quantity.
i48(Endianess) -> i(Endianess, 48).

%% @doc return a signed integer from a 56 bit little or big endian
%% quantity.
i56(Endianess) -> i(Endianess, 56).

%% @doc return a signed integer from a 64 bit little or big endian
%% quantity.
i64(Endianess) -> i(Endianess, 64).

%% @doc return a signed integer from a 72 bit little or big endian
%% quantity.
i72(Endianess) -> i(Endianess, 72).

%% @doc return a signed integer from an 80 bit little or big endian
%% quantity.
i80(Endianess) -> i(Endianess, 80).

%% @doc return a signed integer from an 88 bit little or big endian
%% quantity.
i88(Endianess) -> i(Endianess, 88).

%% @doc return a signed integer from a 96 bit little or big endian
%% quantity.
i96(Endianess) -> i(Endianess, 96).

%% @doc return a signed integer from a 104 bit little or big endian
%% quantity.
i104(Endianess) -> i(Endianess, 104).

%% @doc return a signed integer from a 112 bit little or big endian
%% quantity.
i112(Endianess) -> i(Endianess, 112).

%% @doc return a signed integer from a 120 bit little or big endian
%% quantity.
i120(Endianess) -> i(Endianess, 120).

%% @doc return a signed integer from a 128 bit little or big endian
%% quantity.
i128(Endianess) -> i(Endianess, 128).


-spec i(endianess(), bit_size()) -> iparser().

%% @doc return a signed integer from a little or big endian quantity.
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


%% @doc return an unsigned integer from an 8 bit little or big endian
%% quantity.
u8(Endianess) -> u(Endianess, 8).

%% @doc return an unsigned integer from a 16 bit little or big endian
%% quantity.
u16(Endianess) -> u(Endianess, 16).

%% @doc return an unsigned integer from a 24 bit little or big endian
%% quantity.
u24(Endianess) -> u(Endianess, 24).

%% @doc return an unsigned integer from a 32 bit little or big endian
%% quantity.
u32(Endianess) -> u(Endianess, 32).

%% @doc return an unsigned integer from a 40 bit little or big endian
%% quantity.
u40(Endianess) -> u(Endianess, 40).

%% @doc return an unsigned integer from a 48 bit little or big endian
%% quantity.
u48(Endianess) -> u(Endianess, 48).

%% @doc return an unsigned integer from a 56 bit little or big endian
%% quantity.
u56(Endianess) -> u(Endianess, 56).

%% @doc return an unsigned integer from a 64 bit little or big endian
%% quantity.
u64(Endianess) -> u(Endianess, 64).

%% @doc return an unsigned integer from a 72 bit little or big endian
%% quantity.
u72(Endianess) -> u(Endianess, 72).

%% @doc return an unsigned integer from an 80 bit little or big endian
%% quantity.
u80(Endianess) -> u(Endianess, 80).

%% @doc return an unsigned integer from an 88 bit little or big endian
%% quantity.
u88(Endianess) -> u(Endianess, 88).

%% @doc return an unsigned integer from a 96 bit little or big endian
%% quantity.
u96(Endianess) -> u(Endianess, 96).

%% @doc return an unsigned integer from a 104 bit little or big endian
%% quantity.
u104(Endianess) -> u(Endianess, 104).

%% @doc return an unsigned integer from a 112 bit little or big endian
%% quantity.
u112(Endianess) -> u(Endianess, 112).

%% @doc return an unsigned integer from a 120 bit little or big endian
%% quantity.
u120(Endianess) -> u(Endianess, 120).

%% @doc return an unsigned integer from a 128 bit little or big endian
%% quantity.
u128(Endianess) -> u(Endianess, 128).


%% @doc return an unsigned integer from a little or big endian
%% quantity.

-spec u(endianess(), bit_size()) -> uparser().

u(Endianess, Size) -> int(Endianess, unsigned, Size).


-spec int(endianess(), signed | unsigned, bit_size()) -> iparser() | uparser().

int(little, signed, Size) ->
    fun
        (<<I:Size/little-signed, Remaining/bits>>) ->
            {Remaining, I};

        (L) when is_list(L), length(L) * 8 >= Size ->
            {Taken, Remaining} = lists:split(Size div 8, L),
            <<I:Size/little-signed>> = list_to_binary(Taken),
            {Remaining, I};

        (_) ->
            nomatch
    end;

int(little, unsigned, Size) ->
    fun
        (<<I:Size/little, Remaining/bits>>) ->
            {Remaining, I};

        (L) when is_list(L), length(L) * 8 >= Size ->
            {Taken, Remaining} = lists:split(Size div 8, L),
            <<I:Size/little>> = list_to_binary(Taken),
            {Remaining, I};

        (_) ->
            nomatch
    end;

int(big, signed, Size) ->
    fun
        (<<I:Size/signed, Remaining/bits>>) ->
            {Remaining, I};

        (L) when is_list(L), length(L) * 8 >= Size ->
            {Taken, Remaining} = lists:split(Size div 8, L),
            <<I:Size/signed>> = list_to_binary(Taken),
            {Remaining, I};

        (_) ->
            nomatch
    end;

int(big, unsigned, Size) ->
    fun
        (<<I:Size, Remaining/bits>>) ->
            {Remaining, I};

        (L) when is_list(L), length(L) * 8 >= Size ->
            {Taken, Remaining} = lists:split(Size div 8, L),
            <<I:Size>> = list_to_binary(Taken),
            {Remaining, I};

        (_) ->
            nomatch
    end.


%% @doc return a signed float from an 8 bit little or big endian
%% quantity.
f8(Endianess) -> f(Endianess, 8).


%% @doc return a signed float from a 16 bit little or big endian
%% quantity.
f16(Endianess) -> f(Endianess, 16).


%% @doc return a signed float from a 24 bit little or big endian
%% quantity.
f24(Endianess) -> f(Endianess, 24).

%% @doc return a signed float from a 32 bit little or big endian
%% quantity.
f32(Endianess) -> f(Endianess, 32).

%% @doc return a signed float from a 40 bit little or big endian
%% quantity.
f40(Endianess) -> f(Endianess, 40).

%% @doc return a signed float from a 48 bit little or big endian
%% quantity.
f48(Endianess) -> f(Endianess, 48).

%% @doc return a signed float from a 56 bit little or big endian
%% quantity.
f56(Endianess) -> f(Endianess, 56).

%% @doc return a signed float from a 64 bit little or big endian
%% quantity.
f64(Endianess) -> f(Endianess, 64).

%% @doc return a signed float from a 72 bit little or big endian
%% quantity.
f72(Endianess) -> f(Endianess, 72).

%% @doc return a signed float from an 80 bit little or big endian
%% quantity.
f80(Endianess) -> f(Endianess, 80).

%% @doc return a signed float from an 88 bit little or big endian
%% quantity.
f88(Endianess) -> f(Endianess, 88).

%% @doc return a signed float from a 96 bit little or big endian
%% quantity.
f96(Endianess) -> f(Endianess, 96).

%% @doc return a signed float from a 104 bit little or big endian
%% quantity.
f104(Endianess) -> f(Endianess, 104).

%% @doc return a signed float from a 112 bit little or big endian
%% quantity.
f112(Endianess) -> f(Endianess, 112).

%% @doc return a signed float from a 120 bit little or big endian
%% quantity.
f120(Endianess) -> f(Endianess, 120).

%% @doc return a signed float from a 128 bit little or big endian
%% quantity.
f128(Endianess) -> f(Endianess, 128).


-spec f(endianess(), bit_size()) -> fparser().

f(little, Size) ->
    fun
        (<<I:Size/float-little, Remaining/bits>>) ->
            {Remaining, I};

        (L) when is_list(L), length(L) * 8 >= Size ->
            {Taken, Remaining} = lists:split(Size div 8, L),
            <<I:Size/float-little>> = list_to_binary(Taken),
            {Remaining, I};

        (_) ->
            nomatch
    end;

f(big, Size) ->
    fun
        (<<I:Size/float, Remaining/bits>>) ->
            {Remaining, I};

        (L) when is_list(L), length(L) * 8 >= Size ->
            {Taken, Remaining} = lists:split(Size div 8, L),
            <<I:Size/float>> = list_to_binary(Taken),
            {Remaining, I};

        (_) ->
            nomatch
    end.


-spec precision(pos_integer()) -> fparser().

%% @doc Truncate a floating point value to a number of decimal digits.
precision(Digits) ->
    fun
        (Value) ->
            Decimals = float_to_binary(
                         Value,
                         [{decimals,
                           Digits - trunc(
                                      math:ceil(
                                        math:log10(
                                          trunc(abs(Value)) + 1)))}]),

            case binary:split(Decimals, <<".">>) of
                [_, _] ->
                    binary_to_float(Decimals);

                [_] ->
                    binary_to_float(<<Decimals/bytes, ".0">>)
            end
    end.
