<br>

<p align="center">
    <a href="https://shortishly.github.io/scran/cover/">
      <img alt="Test Coverage" src="https://img.shields.io/badge/dynamic/json?url=https%3A%2F%2Fshortishly.github.io%2Fscran%2Fcover%2Fcoverage.json&query=%24.total&style=flat-square&label=Test%20Coverage&color=green">
    </a>
    <a href="https://shortishly.github.io/scran/edoc/">
      <img alt="edoc" src="https://img.shields.io/badge/Documentation-edoc-green?style=flat-square">
    </a>
    <img alt="Erlang/OTP 25+" src="https://img.shields.io/badge/Erlang%2FOTP-25%2B-green?style=flat-square">
</p>

## What is scran?

scran is a parser combinator library heavily influenced by
[nom][nom-parser-combinators].

Unlike nom, scran isn't primarily intended for binary parsing because
we already have [bit syntax expressions][erlang-bit-syntax] in
Erlang. However, it does have a small number of combinators for length
encoded, or null terminated byte sequences (typically "strings"),
big/little integers with common bit sizes.

Internally, scran uses the [maybe keyword, which is supported by OTP
25+][maybe-expression].

## Example

Each parser returns a function that takes Unicode input, returning a
tuple of the unmatched and matched input.

```erlang
1> (scran_character_complete:tag("Hello"))("Hello, World!").
{", World!","Hello"}
```

Where `"Hello"` has been matched by `tag("Hello")` and `", World!"' is
the remaining input.

```erlang
2> (scran_character_complete:tag("Hello"))("hello, world!"). 
nomatch
```

The `tag("Hello")` is case sensitive so there is `nomatch` for "hello,
world!".

```erlang
3> (scran_character_complete:tag_no_case("Hello"))("hello, world!").
{", world!","hello"}
4> (scran_character_complete:tag_no_case("Hello"))("hellO, wOrlD!").
{", wOrlD!","hellO"}
```

`tag_no_case` can be used for case insensitive matching.

## Character Complete

The `scran_character_complete` module has various parsers used match
different character classes or combinations.

### alpha0

The `alpha0` parser will match zero or more alphabetic characters
in the range of `[a-zA-Z]`.

```erlang
12> (scran_character_complete:alpha0())("abc").
{[], "abc"}

13> (scran_character_complete:alpha0())("").
{[], []}

14> (scran_character_complete:alpha0())("abc123").
{"123", "abc"}

15> (scran_character_complete:alpha0())("123abc").
{"123abc", []}
```

### alpha1

The `alpha1` parser will match one or more alphabetic characters
in the range of `[a-zA-Z]`.

```erlang
8> (scran_character_complete:alpha1())("abc").
{[], "abc"}

9> (scran_character_complete:alpha1())("").
nomatch

10> (scran_character_complete:alpha1())("abc123").
{"123","abc"}

11> (scran_character_complete:alpha1())("123abc").
nomatch
```

### alphanumeric0

The `alphanumeric0` parser will match zero or more alpha numeric
characters in the range of `[a-zA-Z0-9]`.

```erlang
16> (scran_character_complete:alphanumeric0())("abc").
{[], "abc"}

17> (scran_character_complete:alphanumeric0())("").
{[], []}

18> (scran_character_complete:alphanumeric0())("abc123").
{[], "abc123"}

19> (scran_character_complete:alphanumeric0())("123abc").
{[], "123abc"}

20> (scran_character_complete:alphanumeric0())("123abc!%^").
{"!%^","123abc"}

21> (scran_character_complete:alphanumeric0())("!%^abc123"). 
{"!%^abc123", []}
```

### alphanumeric1

The `alphanumeric1` parser will match one or more alpha numeric
characters in the range of `[a-zA-Z0-9]`.

```erlang
24> (scran_character_complete:alphanumeric1())("abc").
{[], "abc"}

25> (scran_character_complete:alphanumeric1())("").
nomatch

26> (scran_character_complete:alphanumeric1())("abc123").
{[], "abc123"}

27> (scran_character_complete:alphanumeric1())("123abc").
{[], "123abc"}

28> (scran_character_complete:alphanumeric1())("123abc!%^").
{"!%^", "123abc"}

29> (scran_character_complete:alphanumeric1())("!%^").
nomatch

30> (scran_character_complete:alphanumeric1())("!%^abc123").
nomatch
```

### digit0

The `digit0` parser will match zero or more numeric
characters in the range of `[0-9]`.

### digit1

The `digit1` parser will match one or more numeric
characters in the range of `[0-9]`.

### multispace0

The `multispace0` parser will match zero or more numeric
characters in the range of `[\\s\\t\\n\\r]`.

### multispace1

The `multispace1` parser will match one or more numeric
characters in the range of `[\\s\\t\\n\\r]`.

### none_of

The `none_of` parser will match if the next character of the input is
none of the supplied characters.

### one_of

The `one_of` parser will match if the next character of the input is
one of the supplied characters.

### re

The `re` parser will match if the input satisfies the supplied case
sensitive regular expression.

### re\_no\_case

The `re\_no\_case` parser will match if the input satisfies the supplied case
insensitive regular expression.

### tag

The `tag` parser will match if the input matches the supplied case
sensitive string.

### tag\_no\_case

The `tag\_no\_case` parser will match if the input matches the supplied case
insensitive string.

### take

The `take` parser will match if it can take the specified number of
characters from the input.

## Branches

The `scan_branch` module is used to specify different branches of
parsing behaviour.

### alt

With `scran_branch:alt/1` you can specify alternate parsers to
use. Each parser is tried in turn until `nomatch` is returned.

```erlang
1> (scran_branch:alt([scran_character_complete:alpha1(),
                      scran_character_complete:digit1()]))("abc123").
{"123","abc"}

2> (scran_branch:alt([scran_character_complete:alpha1(),
                      scran_character_complete:digit1()]))("123456").
{[],"123456"}

3> (scran_branch:alt([scran_character_complete:alpha1(),
                      scran_character_complete:digit1()]))("123456abc").
{"abc","123456"}

4> (scran_branch:alt([scran_character_complete:alpha1(),
                      scran_character_complete:digit1()]))("!@£$").
nomatch
```

### permutation

## scran_combinator

The `scan_combinator` module is used to specify parsers that are
combined into ones that can exhibit complex behaviours.

### all_comsuming

This parser succeeds if all the input has been consumed by its child parser.

### map_result

This parser maps a function on the result of a parser.

### ignore_result

This parser ignores the result of a parser.

### map_parser

This parser applies a parser over the result of another one.

### opt

An optional parser, will return none if the option is not taken.

### value

This parser returns the provided value if the child parser succeeds.

### condition

This parser calls the supplied parser if the condition is met.

### peek

This parser tries to apply its parser without consuming the input.

### eof

This parser returns its input if it is at the end of input data.

### is_not

This parser succeeds if the child parser returns an error.

## Additional

The test cases are currently the best place to look at simple examples
of the combinators. There is also a more complex example that is used
to [parse part of the PostgreSQL grammar][github-com-pgsqlp].

### coverage report

Coverage report [is available here][scran-cover].

### edoc

edoc [is available here][scran-edoc].

[erlang-bit-syntax]: https://www.erlang.org/doc/reference_manual/expressions.html#bit_syntax
[github-com-pgsqlp]: https://github.com/shortishly/pgsqlp
[maybe-expression]: https://www.erlang.org/doc/reference_manual/expressions.html#maybe
[nom-parser-combinators]: https://github.com/rust-bakery/nom
[scran-cover]: https://shortishly.github.io/scran/cover
[scran-edoc]: https://shortishly.github.io/scran/edoc
