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


-module(scran_multi).


-feature(maybe_expr, enable).

-export([many1/1]).
-export([separated_list0/2]).
-export([separated_list1/2]).
-include_lib("kernel/include/logger.hrl").


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
