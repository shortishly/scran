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

%% @doc Parser combinators applying their child parser multiple times.

-module(scran_multi).


-feature(maybe_expr, enable).


-export([count/2]).
-export([fold/4]).
-export([fold_many0/3]).
-export([fold_many1/3]).
-export([fold_many_m_n/5]).
-export([many1/1]).
-export([separated_list0/2]).
-export([separated_list1/2]).
-export_type([gatherer/0]).
-include_lib("kernel/include/logger.hrl").


-type gatherer() :: fun((term(), term()) -> term()).


%% @doc Run the embedded parser N times gathering the results in a
%% list.

-spec count(pos_integer() | scran:parser(scran:input(), pos_integer()), scran:parser()) -> scran:parser().

count(NumOfItemParser, ItemParser) when is_function(NumOfItemParser) ->
    ?LOG_DEBUG(#{num_of_item_parser => NumOfItemParser,
                 parser => scran_debug:pp(ItemParser)}),
    fun
        (Input) ->
            maybe
                {Remainding, N} ?= NumOfItemParser(Input),
                (?FUNCTION_NAME(N, ItemParser))(Remainding)
            end
    end;

count(N, Parser) when is_integer(N), N >= 0 ->
    ?LOG_DEBUG(#{n => N, parser => scran_debug:pp(Parser)}),
    fun
        (Input) ->
            ?FUNCTION_NAME(Parser, Input, N, [])
    end.

count(Parser, Input, 0 = N, A) ->
    ?LOG_DEBUG(#{input => Input,
                 a => A,
                 n => N,
                 parser => scran_debug:pp(Parser)}),
    {Input, lists:reverse(A)};

count(Parser, Input, N, A) ->
    ?LOG_DEBUG(#{input => Input,
                 a => A,
                 n => N,
                 parser => scran_debug:pp(Parser)}),
    case Parser(Input) of
        {Remaining, Result} ->
            ?FUNCTION_NAME(Parser, Remaining, N - 1, [Result | A]);

        nomatch ->
            nomatch
    end.


fold(NumOfItemParser, ItemParser, Initial, Gatherer) ->
    ?LOG_DEBUG(#{num_of_items_parser => NumOfItemParser,
                 item_parser => scran_debug:pp(ItemParser),
                 initial => Initial,
                 gatherer => scran_debug:pp(Gatherer)}),
    fun
        (Input) ->
            maybe
                {Remainding, N} ?= NumOfItemParser(Input),
                ?FUNCTION_NAME(Remainding, N, ItemParser, Initial, Gatherer)
            end
    end.

fold(Input, 0 = N, ItemParser, A, Gatherer) ->
    ?LOG_DEBUG(#{input => Input,
                 n => N,
                 item_parser => scran_debug:pp(ItemParser),
                 a => A,
                 gatherer => scran_debug:pp(Gatherer)}),
    {Input, A};

fold(Input, N, ItemParser, A, Gatherer) when N > 0 ->
    ?LOG_DEBUG(#{input => Input,
                 n => N,
                 item_parser => scran_debug:pp(ItemParser),
                 a => A,
                 gatherer => scran_debug:pp(Gatherer)}),
    maybe
        {Remaining, Result} ?= ItemParser(Input),
        ?FUNCTION_NAME(Remaining, N - 1, ItemParser, Gatherer(Result, A), Gatherer)
    end.


%% @doc Repeats the embedded parser gathering the results.

-spec fold_many0(scran:parser(), term(), gatherer()) -> scran:parser().

fold_many0(Parser, Initial, Gatherer) ->
    ?LOG_DEBUG(#{initial => Initial,
                 parser => scran_debug:pp(Parser),
                 gatherer => scran_debug:pp(Gatherer)}),

    fun
        (Input) ->
            ?FUNCTION_NAME(Parser, Input, Initial, Gatherer)
    end.

fold_many0(Parser, Input, A, Gatherer) ->
    ?LOG_DEBUG(#{parser => scran_debug:pp(Parser),
                 input => Input,
                 a => A,
                 gatherer => scran_debug:pp(Gatherer)}),

    case Parser(Input) of
        {Remaining, Result} ->
            ?FUNCTION_NAME(Parser, Remaining, Gatherer(Result, A), Gatherer);

        nomatch ->
            {Input, A}
    end.


%% @doc Repeats the embedded parser gathering the results.

-spec fold_many1(scran:parser(), term(), gatherer()) -> scran:parser().

fold_many1(Parser, Initial, Gatherer) ->
    ?LOG_DEBUG(#{initial => Initial,
                 parser => scran_debug:pp(Parser),
                 gatherer => scran_debug:pp(Gatherer)}),

    fun
        (Input) ->
            ?FUNCTION_NAME(Parser, Input, 0, Initial, Gatherer)
    end.

fold_many1(Parser, Input, I, A, Gatherer) ->
    ?LOG_DEBUG(#{parser => scran_debug:pp(Parser),
                 input => Input,
                 i => I,
                 a => A,
                 gatherer => scran_debug:pp(Gatherer)}),

    case Parser(Input) of
        {Remaining, Result} ->
            ?FUNCTION_NAME(Parser, Remaining, I + 1, Gatherer(Result, A), Gatherer);

        nomatch when I == 0 ->
            nomatch;

        nomatch ->
            {Input, A}
    end.

%% @doc Repeats the embedded parser Minimum..=Maximum times, calling Gather to gather the results

-spec fold_many_m_n(non_neg_integer(), pos_integer(), scran:parser(), term(), gatherer()) -> scran:parser().

fold_many_m_n(Minimum, Maximum, Parser, Initial, Gatherer) when Minimum =< Maximum ->
    ?LOG_DEBUG(#{minimum => Minimum,
                 maximum => Maximum,
                 parser => scran_debug:pp(Parser),
                 initial => Initial,
                 gatherer => scran_debug:pp(Gatherer)}),

    fun
        (Input) ->
            ?FUNCTION_NAME(Minimum, Maximum, Parser, Input, 0, Initial, Gatherer)
    end.

fold_many_m_n(Minimum, Maximum, Parser, Input, I, A, Gatherer) when Minimum < Maximum, I < Minimum ->
    ?LOG_DEBUG(#{minimum => Minimum,
                 maximum => Maximum,
                 parser => scran_debug:pp(Parser),
                 input => Input,
                 i => I,
                 a => A,
                 gatherer => scran_debug:pp(Gatherer)}),

    case Parser(Input) of
        {Remaining, Result} ->
            ?FUNCTION_NAME(Minimum,
                           Maximum,
                           Parser,
                           Remaining,
                           I + 1,
                           Gatherer(Result, A),
                           Gatherer);
        nomatch ->
            nomatch
    end;

fold_many_m_n(Minimum, Maximum, Parser, Input, I, A, Gatherer) when I < Maximum ->
    ?LOG_DEBUG(#{minimum => Minimum,
                 maximum => Maximum,
                 parser => scran_debug:pp(Parser),
                 input => Input,
                 i => I,
                 a => A,
                 gatherer => scran_debug:pp(Gatherer)}),

    case Parser(Input) of
        {Remaining, Result} when I < (Maximum - 1) ->
            ?FUNCTION_NAME(Minimum,
                           Maximum,
                           Parser,
                           Remaining,
                           I + 1,
                           Gatherer(Result, A),
                           Gatherer);

        {Remaining, Result} ->
            {Remaining, Gatherer(Result, A)};

        nomatch when Minimum =< I ->
            {Input, A};

        nomatch ->
            nomatch
    end.


%% @doc Runs the embedded parser at least once, gathering the results.

-spec many1(scran:parser(I, O)) -> scran:parser(I, [O, ...]).

many1(Parser) ->
    fun
        (Input) ->
            ?FUNCTION_NAME(Parser, Input, [])
    end.


-spec many1(scran:parser(I, O), I, [O]) -> scran:result(I, [O, ...]).

many1(Parser, Input, A) ->
    ?LOG_DEBUG(#{input => Input,
                 a => A,
                 parser => scran_debug:pp(Parser)}),

    case Parser(Input) of
        {Remaing, Result} when Result /= none ->
            ?FUNCTION_NAME(Parser, Remaing, [Result | A]);

        nomatch when A == [] ->
            nomatch;

        nomatch ->
            {Input, lists:reverse(A)}
    end.


%% @doc Alternates between two parsers to produce a possibly empty
%% list of elements.

-spec separated_list0(scran:parser(I, any()),
                      scran:parser(I, Element)) -> scran:parser(I, [Element]).

separated_list0(SeparatorParser, ElementParser) ->
    fun
        (Input) ->
            maybe
                ?LOG_DEBUG(
                   #{input => Input,
                     separator_parser => scran_debug:pp(SeparatorParser),
                     element_parser => scran_debug:pp(ElementParser)}),

                {SeparatorInput, Element} ?= ElementParser(Input),

                ?LOG_DEBUG(#{separator_input => SeparatorInput,
                             element => Element}),

                separated_list(SeparatorParser,
                               ElementParser,
                               SeparatorInput,
                               [Element])
            else
                nomatch ->
                    {Input, []}
            end
    end.


%% @doc Alternates between two parsers to produce a non-empty list of
%% elements.

-spec separated_list1(scran:parser(I, any()),
                      scran:parser(I, Element)) ->
          scran:parser(I, [Element, ...]).

separated_list1(SeparatorParser, ElementParser) ->
    fun
        (Input) ->
            maybe
                ?LOG_DEBUG(
                   #{input => Input,
                     separator_parser => scran_debug:pp(SeparatorParser),
                     element_parser => scran_debug:pp(ElementParser)}),

                {SeparatorInput, Element} ?= ElementParser(Input),

                ?LOG_DEBUG(#{separator_input => SeparatorInput,
                             element => Element}),

                separated_list(SeparatorParser,
                               ElementParser,
                               SeparatorInput,
                               [Element])
            end
    end.

separated_list(SeparatorParser, ElementParser, Input, A) ->
    maybe
        ?LOG_DEBUG(
           #{input => Input,
             separator_parser => scran_debug:pp(SeparatorParser),
             a => A,
             element_parser => scran_debug:pp(ElementParser)}),

        {ElementInput, _} ?= SeparatorParser(Input),
        {NextIterationInput, Element} ?= ElementParser(ElementInput),

        ?LOG_DEBUG(#{element_input => ElementInput,
                     next_iteration_input => NextIterationInput,
                     element => Element}),

        ?FUNCTION_NAME(SeparatorParser,
                       ElementParser,
                       NextIterationInput,
                       [Element | A])
    else
        nomatch ->
            ?LOG_DEBUG(#{input => Input, a => lists:reverse(A)}),
            {Input, lists:reverse(A)}
    end.
