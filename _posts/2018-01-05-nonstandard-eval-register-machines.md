---
title: Using nonstandard evaluation to simulate a register machine
excerpt: "No, not the cha-ching kind of machine"
tags:
  - r
  - nonstandard evaluation
  - advent of code
  - rlang
  - regular expressions
share: true
header:
  overlay_image: "assets/images/samuel-zeller-1280.jpg"
  caption: "Photo credit: [**Samuel Zeller**](https://unsplash.com/photos/vpR0oc4X8Mk)"
  overlay_filter: rgba(192, 192, 192, 0.5)
---





I recently completed all 25 days of [Advent of Code 2017][aoc-2017], an annual
series of recreational programming puzzles. Each day describes a programming
puzzle and illustrates a handful of simple examples of the problem. The puzzle
then requires the participant to solve a much, much larger form of the problem.

For five or so of the puzzles, I used _nonstandard evaluation_ to implement my
solution. [As I previously wrote][nse-post], nonstandard evaluation is a way of
bottling up magic spells (lines of R code) and changing how or where they are
cast (evaluated). I chose to use special evaluation not because it was the
easiest or most obvious solution but because I wanted to develop my skills with
computing on the language. In this post, I work through one of the examples
where I used nonstandard evaluation to write an interpreter for a simple 
machine.

## Puzzle description

[Day 8](http://adventofcode.com/2017/day/8) requires us to simulate the state 
of a register machine as it receives a series of instructions.

> Each instruction consists of several parts: the register to modify, whether to
increase or decrease that register's value, the amount by which to increase or
decrease it, and a condition. If the condition fails, skip the instruction
without modifying the register. The registers all start at `0`. The instructions
look like this:
>
>     b inc 5 if a > 1
>     a inc 1 if b < 5
>     c dec -10 if a >= 1
>     c inc -20 if c == 10
>
> [...]
>
> You might also encounter `<=` (less than or equal to) or `!=` (not equal to).
However, the CPU doesn't have the bandwidth to tell you what all the registers
are named, and leaves that to you to determine.
>
> **What is the largest value in any register** after completing the
instructions in your puzzle input?

If I squint long enough at the register instructions, I can basically 
see R code.


```r
# b inc 5 if a > 1
b <- if (a > 1) b + 5 else b

# a inc 1 if b < 5
a <- if (b < 5) a + 1 else a

# c dec -10 if a >= 1
c <- if (a >= 1) c - -10 else c

# c inc -20 if c == 10
c <- if (c == 10) c + -20 else c
```

If we can set up a way to convert the machine instructions into R code, R will
handle the job of looking up values, modifying values and evaluating the logic
and `if` statements. In other words, if we can convert register instructions
into R code, **the problem simplifies into something like running an R script**.

And that's a good strategy, because we have a lot of instructions to process.
Each user receives some unique (I think) input for each problem, and my problem
input contains 1,000 instructions.


```r
library(magrittr)
full_input <- "https://raw.githubusercontent.com" %>% 
  file.path(
    "tjmahr", "adventofcode17", "master", "inst", "input08.txt"
  ) %>% 
  readr::read_lines()

length(full_input)
#> [1] 1000

head(full_input)
#> [1] "kd dec -37 if gm <= 9"   "x dec -715 if kjn == 0" 
#> [3] "ey inc 249 if x < 722"   "n dec 970 if t > 3"     
#> [5] "f dec -385 if msg > -3"  "kd dec -456 if ic <= -8"
```

Our strategy for simulating the register machine will have the following steps:

* Parsing a register instruction
* Creating an R expression from an instruction
* Evaluating an R expression _inside_ of a register machine
* Changing the evaluation rules to adapt to the quirks of this problem

## Parsing the register instructions with regular expressions

The instructions have a very simple grammar. Here is how I would tag the first
few lines of my problem input.

```
[target] [verb] [num1] if [s1] [op] [num2]
      kd    dec    -37 if   gm   <=     9
       x    dec   -715 if  kjn   ==     0
      ey    inc    249 if    x    <   722
       n    dec    970 if    t    >     3
       f    dec   -385 if  msg    >    -3
```

We can parse these lines using regular expressions. Regular expressions are an
incredibly powerful language for processing text using pattern-matching rules. I
will walk through each of the regular expression patterns used to parse an
instruction.

To match the verbs, we can use the [or `|` operator][re-or], so
`(inc|dec)` matches `inc` or `dec`. We can also match the six different
comparison operators using `|` too. In the code below, I put the patterns in
parentheses so that they will be treated as a single group.


```r
re_verb <- "(inc|dec)"
re_op <- "(>|<|==|!=|>=|<=)"
```

A register name is just a sequence of letters. The special character `\w`
matches any _word_ character; that is, it matches uppercase/lowercase letters,
digits and underscores. [The `(token)+` suffix][re-rep] matches 1 or more 
repetitions of a token. Putting these two together, `\w+` will match 1 or 
more adjacent word characters. That pattern in principle could matches things 
beside register names (like numbers) but the instruction format here is so
constrained that it's not a problem.


```r
# We have to double the backslashes when using them in R strings
re_name <- "(\\w+)"
```

Numbers are just integers, and sometimes they are negative. A number here
is an optional `-` plus some digits. The special character `\d` matches any
digit from 0 to 9, so `\d+` matches 1 or more successive digits. We can use 
[the `(token)?` suffix][re-opt] to match an optional token. In our case, 
`-?\d+` will match a sequence of digits and match leading hyphen if one is 
present.


```r
re_number <- "(-?\\d+)"
```

Each pattern in parentheses is a matching group, and the function `str_match()`
from [the stringr package](http://stringr.tidyverse.org/) will return a matrix
with a column for each matched group. I also include an extra set of
parentheses around the condition in the `if` statement to also match the whole
condition as well as its parts.

[re-or]: https://www.regular-expressions.info/alternation.html  
    "Alternation in regular expressions"

[re-rep]: https://www.regular-expressions.info/repeat.html
    "Repetition in regular expressions"

[re-opt]: https://www.regular-expressions.info/optional.html
    "Optional matching in regular expressions"



```r
# Combine the sub-patterns together
re <- sprintf(
  "%s %s %s if (%s %s %s)", 
  re_name, re_verb, re_number, re_name, re_op, re_number
)
re
#> [1] "(\\w+) (inc|dec) (-?\\d+) if ((\\w+) (>|<|==|!=|>=|<=) (-?\\d+))"

text <- "b inc 5 if a > 1"
stringr::str_match(text, re)
#>      [,1]               [,2] [,3]  [,4] [,5]    [,6] [,7] [,8]
#> [1,] "b inc 5 if a > 1" "b"  "inc" "5"  "a > 1" "a"  ">"  "1"

# Column 5 matches the subgroups in columns 6, 7 and 8 as a single 
# group because of the extra grouping parentheses after the `if`.
```

We can package this step into a function that takes an instruction's
text and returns a list with the labeled parts of that instruction.


```r
parse_instruction <- function(text) {
  stopifnot(length(text) == 1)
  re <- "(\\w+) (inc|dec) (-?\\d+) if ((\\w+) (>|<|==|!=|>=|<=) (-?\\d+))"
  
  text %>% 
    stringr::str_match(re) %>% 
    as.list() %>% 
    setNames(
      c("instruction", "target", "verb", "num1", "cond", "s1", "op", "num2")
    )
}

str(parse_instruction(text))
#> List of 8
#>  $ instruction: chr "b inc 5 if a > 1"
#>  $ target     : chr "b"
#>  $ verb       : chr "inc"
#>  $ num1       : chr "5"
#>  $ cond       : chr "a > 1"
#>  $ s1         : chr "a"
#>  $ op         : chr ">"
#>  $ num2       : chr "1"
```


## Creating R code

Next, we need to convert some strings into R code. We can do this with 
`rlang::parse_expr()`. It takes a string and creates an R expression, 
something I've described as a kind of [bottled up magic spell][nse-post]: An 
expression captures magic words (code) allow us to manipulate or cast (evaluate)
them.


```r
code <- rlang::parse_expr("print('hello')")
code
#> print("hello")

code <- rlang::parse_expr("if (a > 1) b + 5 else b")
code
#> if (a > 1) b + 5 else b
```

The format of the instructions is relatively straightforward. We can fill
in a template with the parts of the parsed line. Because `inc/dec` are just
addition and subtraction, we replace them with the appropriate math operations.


```r
create_r_instruction <- function(parsed) {
  parsed$math <- if (parsed$verb == "inc") "+" else "-"
  code <- sprintf(
    "if (%s) %s %s %s else %s", 
    parsed$cond, parsed$target, parsed$math, parsed$num1, parsed$target
  )
  rlang::parse_expr(code)
}

r_code <- "b inc 5 if a > 1" %>%
  parse_instruction() %>% 
  create_r_instruction()

r_code
#> if (a > 1) b + 5 else b
```

## Create the register machine

We have to figure out _where_ we want to evaluate the generated R code. We
create a register object to hold the values. The object will just be a `list()`
with some extra metadata. This object will be the location where the R code 
is evaluated.


```r
create_register_machine <- function(...) {
  initial <- list(...)
  data <- c(initial, list(.metadata = list()))
  structure(data, class = c("register_machine", "list"))
}

# Give the machines a pretty print method
print.register_machine <- function(x, ...) {
  utils::str(x, ...)
  invisible(x)
}

create_register_machine()
#> $.metadata
#> list()
#> 
#> attr(,"class")
#> [1] "register_machine" "list"
```

For now, we can initialize registers by using named arguments to the function.


```r
create_register_machine(a = 0, b = 0)
#> $a
#> [1] 0
#> 
#> $b
#> [1] 0
#> 
#> $.metadata
#> list()
#> 
#> attr(,"class")
#> [1] "register_machine" "list"
```


## Evaluating code inside of the machine

So far, we have:

* A way to analyze register instructions and convert them into R code
* An object that holds register values

Now, we need to evaluate an expression *inside* of the register. We will use  
[tidy evaluation][tidy-eval]; the function `eval_tidy()` lets us evaluate an
R expression inside of a data object.


```r
r_code
#> if (a > 1) b + 5 else b

# b + 5
r <- create_register_machine(a = 4, b = 7)
rlang::eval_tidy(r_code, data = r)
#> [1] 12

# just b
r <- create_register_machine(a = 0, b = 7)
rlang::eval_tidy(r_code, data = r)
#> [1] 7
```

Now, we need to actually do something. We need to update the register machine
using the value from the evaluated instruction. Otherwise, the machine will just
read expressions and forget everything it's read.

To update the machine, we have to determine the register to update. Fortunately,
our generated code always ends with an `else` branch that has the target 
register.


```r
r_code
#> if (a > 1) b + 5 else b
```

If we can pull out that symbol after the `else`, we will have the name of
register to update in the machine. Because the code is so formulaic, we can just
extract the symbol directly using the code's [abstract syntax tree (AST)][ast]. 
`pryr::call_tree()` shows the AST for an expression.


```r
pryr::call_tree(r_code)
#> \- ()
#>   \- `if
#>   \- ()
#>     \- `>
#>     \- `a
#>     \-  1
#>   \- ()
#>     \- `+
#>     \- `b
#>     \-  5
#>   \- `b

# Update 2021-02-08: lobstr is now the preferred package.
lobstr::ast(!! r_code)
#> o-`if` 
#> +-o-`>` 
#> | +-a 
#> | \-1 
#> +-o-`+` 
#> | +-b 
#> | \-5 
#> \-b
```

We can extract elements from the tree like elements in a list by selecting 
indices. 


```r
# The numbers match one of the slashs at the first level of indentation
r_code[[1]]
#> `if`
r_code[[2]]
#> a > 1

# We can crawl down subtrees too
r_code[[2]][[2]]
#> a

# But what we want is the last branch from the `if` level
r_code[[4]]
#> b
```

If we convert the symbol into a string, we can look it up in the register using
the usual list lookup syntax.


```r
r <- create_register_machine(a = 4, b = 7)
target <- rlang::as_string(r_code[[4]])
r[[target]]
#> [1] 7
```

We can also use list lookup syntax with assignment to _modify_ the register.


```r
r[[target]] <- rlang::eval_tidy(r_code, data = r)
r
#> $a
#> [1] 4
#> 
#> $b
#> [1] 12
#> 
#> $.metadata
#> list()
#> 
#> attr(,"class")
#> [1] "register_machine" "list"
```

Let's wrap these steps into a function. 


```r
eval_instruction <- function(register_machine, instruction) {
  target <- rlang::as_string(instruction[[4]])
  register_machine[[target]] <- rlang::eval_tidy(
    expr = instruction, 
    data = register_machine)
  register_machine
}

create_register_machine(a = 2, b = 0) %>% 
  eval_instruction(r_code)
#> $a
#> [1] 2
#> 
#> $b
#> [1] 5
#> 
#> $.metadata
#> list()
#> 
#> attr(,"class")
#> [1] "register_machine" "list"

create_register_machine(a = 2, b = 0) %>% 
  # For quick testing, we pass in quoted expressions
  eval_instruction(quote(if (a > 1) b - 100 else b)) %>% 
  # Should not run
  eval_instruction(quote(if (a < 1) b + 5 else b)) %>% 
  # Should run
  eval_instruction(quote(if (a > 1) a + 10 else a))
#> $a
#> [1] 12
#> 
#> $b
#> [1] -100
#> 
#> $.metadata
#> list()
#> 
#> attr(,"class")
#> [1] "register_machine" "list"
```

## Time for some *extra* nonstandard evaluation

The code so far only works if the machine already has registers that match the
registers in an instruction. Otherwise, we raise an error.


```r
create_register_machine() %>% 
  eval_instruction(quote(if (a > 1) b - 100 else b))
#> Error in rlang::eval_tidy(expr = instruction, data = register_machine): object 'a' not found
```

To solve the problem, we could study the 1,000 lines of input beforehand,
extract the register names, initialize them to 0 and then evaluate the
instructions.[^start-here] Or... or... we could procrastinate and only
initialize a register name to 0 when the machine encounters a name it doesn't
recognize. If, for some reason, our machine received instructions 
one at a time, like over a network connection, then the procrastinated approach
seems even more reasonable.

This latter strategy will involve some *very* nonstandard evaluation. I
emphasize the "very" because **we are changing one of the fundamental
rules of R evaluation** 😈. R throws an error if
you ask it to evaluate the name of a variable that doesn't exist. But
here we are going to detect those missing variables and set them to 0
before they get evaluated.

To find the brand-new register names, we can inspect the call tree and find the
names of the registers. We already know where the target is. The other place 
where names show up is in the condition of the `if` statement.


```r
pryr::call_tree(r_code)
#> \- ()
#>   \- `if
#>   \- ()
#>     \- `>
#>     \- `a
#>     \-  1
#>   \- ()
#>     \- `+
#>     \- `b
#>     \-  5
#>   \- `b

extract_register_names <- function(instruction) {
  reg_target <- rlang::as_string(instruction[[4]])
  reg_condition <- rlang::as_string(instruction[[2]][[2]])
  list(
    target = reg_target,
    registers = unique(c(reg_target, reg_condition))
  )
}

extract_register_names(quote(if (a > 1) b - 100 else b)) %>% str()
#> List of 2
#>  $ target   : chr "b"
#>  $ registers: chr [1:2] "b" "a"

# Just returns unique names
extract_register_names(quote(if (b > 1) b - 100 else b)) %>% str()
#> List of 2
#>  $ target   : chr "b"
#>  $ registers: chr "b"
```

We can define a helper function which checks for missing names---names that
yield `NULL` values when we try to retrieve them---and initializes them to 0.


```r
initialize_new_registers <- function(register_machine, registers) {
  for (each_register in registers) {
    if (is.null(register_machine[[each_register]])) {
      register_machine[[each_register]] <- 0
    }
  }
  register_machine
}

# Before
r
#> $a
#> [1] 4
#> 
#> $b
#> [1] 12
#> 
#> $.metadata
#> list()
#> 
#> attr(,"class")
#> [1] "register_machine" "list"

initialize_new_registers(r, c("a", "b", "w", "a", "s", "j"))
#> $a
#> [1] 4
#> 
#> $b
#> [1] 12
#> 
#> $.metadata
#> list()
#> 
#> $w
#> [1] 0
#> 
#> $s
#> [1] 0
#> 
#> $j
#> [1] 0
#> 
#> attr(,"class")
#> [1] "register_machine" "list"
```

Finally, we update our evaluation function to do this step automatically. I'm
also going to add some code to record the value of the maximum register whenever
an instruction is evaluated because, ummm, that's the whole point of puzzle.


```r
eval_instruction <- function(register_machine, instruction) {
  # Set any new registers to 0
  registers <- extract_register_names(instruction)
  register_machine <- initialize_new_registers(
    register_machine, registers$registers)
  
  # Evaluate instruction
  register_machine[[registers$target]] <- rlang::eval_tidy(
    expr = instruction, 
    data = register_machine)
  
  # Find the maximum value
  register_names <- setdiff(names(register_machine), ".metadata")
  current_max <- max(unlist(register_machine[register_names]))
  register_machine$.metadata$max <- current_max
  register_machine
}
```

Let's try four instructions from a clean slate.


```r
create_register_machine() %>% 
  # b gets 5
  eval_instruction(quote(if (d < 1) b + 5 else b)) %>% 
  # c gets 10
  eval_instruction(quote(if (b > 1) c + 10 else c)) %>% 
  # b gets 5 more
  eval_instruction(quote(if (a < 1) b + 5 else b))
#> $.metadata
#> $.metadata$max
#> [1] 10
#> 
#> 
#> $b
#> [1] 10
#> 
#> $d
#> [1] 0
#> 
#> $c
#> [1] 10
#> 
#> $a
#> [1] 0
#> 
#> attr(,"class")
#> [1] "register_machine" "list"
```

Now, for the moment of truth... Let's process all 1,000 instructions.


```r
r <- create_register_machine()

for (each_instruction in full_input) {
  parsed <- each_instruction %>% 
    parse_instruction() %>% 
    create_r_instruction()
  r <- eval_instruction(r, parsed)
}

r
#> $.metadata
#> $.metadata$max
#> [1] 4832
#> 
#> 
#> $kd
#> [1] -2334
#> 
#> $gm
#> [1] -4239
#> 
#> $x
#> [1] -345
#> 
#> $kjn
#> [1] -1813
#> 
#> $ey
#> [1] 209
#> 
#> $n
#> [1] -764
#> 
#> $t
#> [1] 2997
#> 
#> $f
#> [1] 4468
#> 
#> $msg
#> [1] -3906
#> 
#> $ic
#> [1] -263
#> 
#> $zv
#> [1] -599
#> 
#> $gub
#> [1] 2025
#> 
#> $yp
#> [1] -2530
#> 
#> $lyr
#> [1] -2065
#> 
#> $j
#> [1] 3619
#> 
#> $e
#> [1] -4230
#> 
#> $riz
#> [1] 863
#> 
#> $lzd
#> [1] 4832
#> 
#> $ucy
#> [1] -3947
#> 
#> $i
#> [1] 3448
#> 
#> $omz
#> [1] -3365
#> 
#> $djq
#> [1] 392
#> 
#> $bxy
#> [1] 1574
#> 
#> $tj
#> [1] 1278
#> 
#> $y
#> [1] 1521
#> 
#> $m
#> [1] 2571
#> 
#> attr(,"class")
#> [1] "register_machine" "list"
```

⭐ Ta-da! The maximum register value is 4,832.
**Problem solved!** 


## And then the rules change

Advent of Code problems come in two parts, and we don't learn the question
behind Part 2 until we complete Part 1. In this case, after submitting our
solution for Part 1, we receive the following requirement:

> To be safe, the CPU also needs to know **the highest value held in any
> register during this process** so that it can decide how much memory
> to allocate to these operations.

Accounting for this twist requires a small change to the evaluation code. We 
add another metadata variable to track the highest value ever stored in a 
register.


```r
eval_instruction <- function(register_machine, instruction) {
  # Set any new registers to 0
  registers <- extract_register_names(instruction)
  register_machine <- initialize_new_registers(
    register_machine, registers$registers)
  
  # Evaluate instruction
  register_machine[[registers$target]] <- rlang::eval_tidy(
    expr = instruction, 
    data = register_machine)
  
  # Find the maximum value
  register_names <- setdiff(names(register_machine), ".metadata")
  current_max <- max(unlist(register_machine[register_names]))
  register_machine$.metadata$max <- current_max
  
  # Create the max-ever value if necessary
  if (is.null(register_machine$.metadata$max_ever)) {
    register_machine$.metadata$max_ever <- 0
  }
  
  # Maybe update the max-ever value
  if (register_machine$.metadata$max_ever < current_max) {
    register_machine$.metadata$max_ever <- current_max
  }
  
  register_machine
}
```

Admittedly, `eval_instruction()` is starting to get bloated.
Conceptually, we could probably the break this function down into three
functions: pre-evaluation steps, evaluation, and post-evaluation
steps.[^brainstorm] But this is good enough for now.

We run the instructions again to get the updated metadata.


```r
r <- create_register_machine()

for (each_instruction in full_input) {
  parsed <- each_instruction %>% 
    parse_instruction() %>% 
    create_r_instruction()
  r <- eval_instruction(r, parsed)
}

r$.metadata
#> $max
#> [1] 4832
#> 
#> $max_ever
#> [1] 5443
```

🌟 And boom! **Another problem solved.**

## `eval(thoughts, envir = this_problem)`

I like this kind of nonstandard evaluation approach for converting
problems into R code, but it's mostly useful when the problem describes
a series of instructions that can be parsed and evaluated. For problems
like this register machine simulation, the nonstandard evaluation route
is straightforward. But it's also a viable problem-solving strategy when
the "machine" or the "instructions" are subtler, as in [this problem
about simulating "dance" moves][dance-moves].

Odds are, you'll never have to write an interpreter for a toy machine or
language. Nevertheless, here are some R functions that we used for this
puzzle that are helpful in other contexts:

  - `stringr::str_match()` to extract all the groups in a regular
    expression at once.
  - `rlang::parse_expr()` to convert a string of text into an R
    expression.
  - `pryr::call_tree()` to visualize an expression's syntax tree and
    `expression[[i]][[j]]` to pluck out symbols from locations in a
    tree.
  - `rlang::as_string()` to convert a symbol into a string.
  - `rlang::eval_tidy()` to evaluate an expression inside of a data
    context.






***

*Last knitted on 2021-11-16. [Source code on
GitHub](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/2018-01-05-nonstandard-eval-register-machines.Rmd).*[^si] 

[^si]: 
    
    ```r
    sessioninfo::session_info()
    #> - Session info  --------------------------------------------------------------
    #>  hash: necktie, brown square, flag: Guinea-Bissau
    #> 
    #>  setting  value
    #>  version  R version 4.1.2 (2021-11-01)
    #>  os       Windows 10 x64 (build 22000)
    #>  system   x86_64, mingw32
    #>  ui       RTerm
    #>  language (EN)
    #>  collate  English_United States.1252
    #>  ctype    English_United States.1252
    #>  tz       America/Chicago
    #>  date     2021-11-16
    #>  pandoc   NA
    #> 
    #> - Packages -------------------------------------------------------------------
    #>  package     * version    date (UTC) lib source
    #>  assertthat    0.2.1      2019-03-21 [1] CRAN (R 4.1.0)
    #>  bit           4.0.4      2020-08-04 [1] CRAN (R 4.1.0)
    #>  bit64         4.0.5      2020-08-30 [1] CRAN (R 4.1.0)
    #>  cli           3.1.0      2021-10-27 [1] CRAN (R 4.1.1)
    #>  codetools     0.2-18     2020-11-04 [2] CRAN (R 4.1.2)
    #>  crayon        1.4.2      2021-10-29 [1] CRAN (R 4.1.1)
    #>  curl          4.3.2      2021-06-23 [1] CRAN (R 4.1.0)
    #>  ellipsis      0.3.2      2021-04-29 [1] CRAN (R 4.1.0)
    #>  emo           0.0.0.9000 2021-10-14 [1] Github (hadley/emo@3f03b11)
    #>  evaluate      0.14       2019-05-28 [1] CRAN (R 4.1.0)
    #>  fansi         0.5.0      2021-05-25 [1] CRAN (R 4.1.0)
    #>  generics      0.1.1      2021-10-25 [1] CRAN (R 4.1.1)
    #>  git2r         0.28.0     2021-01-10 [1] CRAN (R 4.1.1)
    #>  glue          1.4.2      2020-08-27 [1] CRAN (R 4.1.1)
    #>  here          1.0.1      2020-12-13 [1] CRAN (R 4.1.0)
    #>  hms           1.1.1      2021-09-26 [1] CRAN (R 4.1.1)
    #>  knitr       * 1.36       2021-09-29 [1] CRAN (R 4.1.1)
    #>  lifecycle     1.0.1      2021-09-24 [1] CRAN (R 4.1.1)
    #>  lobstr        1.1.1      2019-07-02 [1] CRAN (R 4.1.1)
    #>  lubridate     1.8.0      2021-10-07 [1] CRAN (R 4.1.1)
    #>  magrittr    * 2.0.1      2020-11-17 [1] CRAN (R 4.1.0)
    #>  pillar        1.6.4      2021-10-18 [1] CRAN (R 4.1.1)
    #>  pkgconfig     2.0.3      2019-09-22 [1] CRAN (R 4.1.0)
    #>  pryr          0.1.5      2021-07-26 [1] CRAN (R 4.1.1)
    #>  purrr         0.3.4      2020-04-17 [1] CRAN (R 4.1.0)
    #>  R6            2.5.1      2021-08-19 [1] CRAN (R 4.1.1)
    #>  ragg          1.2.0      2021-10-30 [1] CRAN (R 4.1.1)
    #>  Rcpp          1.0.7      2021-07-07 [1] CRAN (R 4.1.0)
    #>  readr         2.0.2      2021-09-27 [1] CRAN (R 4.1.1)
    #>  rlang         0.4.12     2021-10-18 [1] CRAN (R 4.1.1)
    #>  rprojroot     2.0.2      2020-11-15 [1] CRAN (R 4.1.0)
    #>  rstudioapi    0.13       2020-11-12 [1] CRAN (R 4.1.0)
    #>  sessioninfo   1.2.1      2021-11-02 [1] CRAN (R 4.1.2)
    #>  stringi       1.7.5      2021-10-04 [1] CRAN (R 4.1.1)
    #>  stringr       1.4.0      2019-02-10 [1] CRAN (R 4.1.0)
    #>  systemfonts   1.0.3      2021-10-13 [1] CRAN (R 4.1.1)
    #>  textshaping   0.3.6      2021-10-13 [1] CRAN (R 4.1.1)
    #>  tibble        3.1.5      2021-09-30 [1] CRAN (R 4.1.1)
    #>  tidyselect    1.1.1      2021-04-30 [1] CRAN (R 4.1.0)
    #>  tzdb          0.2.0      2021-10-27 [1] CRAN (R 4.1.1)
    #>  utf8          1.2.2      2021-07-24 [1] CRAN (R 4.1.0)
    #>  vctrs         0.3.8      2021-04-29 [1] CRAN (R 4.1.0)
    #>  vroom         1.5.5      2021-09-14 [1] CRAN (R 4.1.1)
    #>  xfun          0.27       2021-10-18 [1] CRAN (R 4.1.1)
    #> 
    #>  [1] C:/Users/trist/Documents/R/win-library/4.1
    #>  [2] C:/Program Files/R/R-4.1.2/library
    #> 
    #> ------------------------------------------------------------------------------
    ```


[^start-here]: That actually would be pretty easy. Get a dataframe with 
    `purrr::map_df(full_input, parse_instruction)`. Find the unique register 
    names. Create a list of 0's with those names. Use `do.call()` to call 
    `create_register_machine()` with that list. With no special evaluation 
    trickery, this approach is closer to the idea of "just running R code".

[^brainstorm]: If all I did for a living was write code to simulate machines 
    or toy languages, I might try to formalize this custom evaluation process 
    with pre-evaluation and post-evaluations "hooks" that could be arguments 
    to a custom evaluation function. I'm just brainstorming though.

[aoc-2017]: http://adventofcode.com/2017 
    "Advent of Code 2017"

[ast]: http://adv-r.had.co.nz/Expressions.html#structure-of-expressions
    "Advanced R chapter on Expressions"

[dance-moves]: http://adventofcode.com/2017/day/16 
    "Advent of Code 2017 - Day 16"

[nse-post]: /set-na-where-nonstandard-evaluation-use-case/ 
    "set_na_where(): a nonstandard evaluation use case"

[tidy-eval]: http://rlang.tidyverse.org/articles/tidy-evaluation.html
    "Description of tidy evaluation"
