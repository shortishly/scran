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


%% @doc Parser combinators that deal with byte inputs


-module(scran_bytes).


-feature(maybe_expr, enable).


-export([length_encoded/1]).
-export([null_terminated/0]).
-export([split/1]).
-export([tag/1]).
-export([take/1]).
-include_lib("kernel/include/logger.hrl").


%% @doc Take N bytes from the input, returning the remaining input and
%% the taken bytes.

-spec take(non_neg_integer()) -> scran:parser(binary(), binary()).

take(N) ->
    fun
        (<<Taken:N/bytes, Remaining/bytes>>) ->
            ?LOG_DEBUG(#{n => N,
                         taken => Taken,
                         remaining => Remaining}),

            {Remaining, Taken};

        (_) ->
            nomatch
    end.


%% @doc Read bytes from input until reaching a null byte (which is
%% discarded), returning the remaining input and the bytes prior to
%% the null.

-spec null_terminated() -> scran:parser().

null_terminated() ->
    split(<<0>>).


%% @doc Split the input on the supplied binary pattern, returning the
%% input before and leaving the remaining input after.

-spec split(binary()) -> scran:parser().

split(Pattern) ->
    fun
        (Input) ->
            maybe
                [Before, After] ?= binary:split(Input, Pattern),
                {After, Before}

            else
                [_] ->
                    nomatch
            end
    end.


%% @doc Use the supplied parser to determine the length of the
%% returned taken bytes.

-spec length_encoded(scran_number:uparser()) -> scran:parser().

length_encoded(LengthParser) ->
    fun
        (Input) ->
            maybe
                {Remaining, Length} ?= LengthParser(Input),
                (take(Length))(Remaining)
            end
    end.


%% @doc Return the matching input.

-spec tag(binary()) -> scran:parser().

tag(Tag) ->
    fun
        (<<Matched:(byte_size(Tag))/bytes, Remaining/bytes>>)
          when Tag == Matched ->
            {Remaining, Tag};

        (_) ->
            nomatch
    end.
