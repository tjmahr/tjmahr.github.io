---
title: "eval() vs rlang::eval_bare() semantics"
date: 2021-11-01
tags: [r, metaprogramming, evaluation]
---

`eval()` is stack inconsistent (whatever that means)

`base::eval()` and `rlang::eval_bare()` [do things differently](https://rlang.r-lib.org/reference/eval_bare.html).

Here are the examples from `?eval_bare()`

<!--

``` r
library(rlang)

# eval_bare() works just like base::eval() but you have to create
# the evaluation environment yourself:
eval_bare(quote(foo), env(foo = "bar"))
#> [1] "bar"

# eval() has different evaluation semantics than eval_bare(). It
# can return from the supplied environment even if its an
# environment that is not on the call stack (i.e. because you've
# created it yourself). The following would trigger an error with
# eval_bare():
ret <- quote(return("foo"))
eval(ret, env())
#> [1] "foo"
eval_bare(ret, env())  
#> Error in eval_bare(ret, env()) : 
#>   no function to return from, jumping to top level

# Another feature of eval() is that you can control surround loops:
bail <- quote(break)
while (TRUE) {
  eval(bail)
}

while (TRUE) {
  eval_bare(bail)
}
#> Error in eval_bare(bail) : no loop for break/next, jumping to top level

# To explore the consequences of stack inconsistent semantics, let's
# create a function that evaluates `parent.frame()` deep in the call
# stack, in an environment corresponding to a frame in the middle of
# the stack. For consistency with R's lazy evaluation semantics, we'd
# expect to get the caller of that frame as result:
fn <- function(eval_fn) {
  list(
    returned_env = middle(eval_fn),
    actual_env = current_env()
  )
}
middle <- function(eval_fn) {
  deep(eval_fn, current_env())
}
deep <- function(eval_fn, eval_env) {
  expr <- quote(parent.frame())
  eval_fn(expr, eval_env)
}

# With eval_bare(), we do get the expected environment:
fn(rlang::eval_bare)
#> $returned_env
#> <environment: 0x55cac3595df8>
#> 
#> $actual_env
#> <environment: 0x55cac3595df8>
#> 

# But that's not the case with base::eval():
fn(base::eval)
#> $returned_env
#> <environment: 0x55cac1cc7980>
#> 
#> $actual_env
#> <environment: 0x55cac1cc2170>
#> 
```
-->
<pre class='chroma'>
<span><span class='kr'><a href='https://rdrr.io/r/base/library.html'>library</a></span><span class='o'>(</span><span class='nv'><a href='https://rlang.r-lib.org'>rlang</a></span><span class='o'>)</span></span>
<span></span>
<span><span class='c'># eval_bare() works just like base::eval() but you have to create</span></span>
<span><span class='c'># the evaluation environment yourself:</span></span>
<span><span class='nf'><a href='https://rlang.r-lib.org/reference/eval_bare.html'>eval_bare</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='nv'>foo</span><span class='o'>)</span>, <span class='nf'><a href='https://rlang.r-lib.org/reference/env.html'>env</a></span><span class='o'>(</span>foo <span class='o'>=</span> <span class='s'>"bar"</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "bar"</span></span>
<span></span>
<span><span class='c'># eval() has different evaluation semantics than eval_bare(). It</span></span>
<span><span class='c'># can return from the supplied environment even if its an</span></span>
<span><span class='c'># environment that is not on the call stack (i.e. because you've</span></span>
<span><span class='c'># created it yourself). The following would trigger an error with</span></span>
<span><span class='c'># eval_bare():</span></span>
<span><span class='nv'>ret</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='kr'><a href='https://rdrr.io/r/base/function.html'>return</a></span><span class='o'>(</span><span class='s'>"foo"</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='nf'><a href='https://rdrr.io/r/base/eval.html'>eval</a></span><span class='o'>(</span><span class='nv'>ret</span>, <span class='nf'><a href='https://rlang.r-lib.org/reference/env.html'>env</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='c'>#&gt; [1] "foo"</span></span>
<span><span class='nf'><a href='https://rlang.r-lib.org/reference/eval_bare.html'>eval_bare</a></span><span class='o'>(</span><span class='nv'>ret</span>, <span class='nf'><a href='https://rlang.r-lib.org/reference/env.html'>env</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span>  </span>
<span><span class='c'>#&gt; Error in eval_bare(ret, env()) : </span></span>
<span><span class='c'>#&gt;   no function to return from, jumping to top level</span></span>
<span></span>
<span><span class='c'># Another feature of eval() is that you can control surround loops:</span></span>
<span><span class='nv'>bail</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='kr'>break</span><span class='o'>)</span></span>
<span><span class='kr'>while</span> <span class='o'>(</span><span class='kc'>TRUE</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/eval.html'>eval</a></span><span class='o'>(</span><span class='nv'>bail</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='kr'>while</span> <span class='o'>(</span><span class='kc'>TRUE</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nf'><a href='https://rlang.r-lib.org/reference/eval_bare.html'>eval_bare</a></span><span class='o'>(</span><span class='nv'>bail</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span><span class='c'>#&gt; Error in eval_bare(bail) : no loop for break/next, jumping to top level</span></span>
<span></span>
<span><span class='c'># To explore the consequences of stack inconsistent semantics, let's</span></span>
<span><span class='c'># create a function that evaluates `parent.frame()` deep in the call</span></span>
<span><span class='c'># stack, in an environment corresponding to a frame in the middle of</span></span>
<span><span class='c'># the stack. For consistency with R's lazy evaluation semantics, we'd</span></span>
<span><span class='c'># expect to get the caller of that frame as result:</span></span>
<span><span class='nv'>fn</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>eval_fn</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nf'><a href='https://rdrr.io/r/base/list.html'>list</a></span><span class='o'>(</span></span>
<span>    returned_env <span class='o'>=</span> <span class='nf'>middle</span><span class='o'>(</span><span class='nv'>eval_fn</span><span class='o'>)</span>,</span>
<span>    actual_env <span class='o'>=</span> <span class='nf'><a href='https://rlang.r-lib.org/reference/stack.html'>current_env</a></span><span class='o'>(</span><span class='o'>)</span></span>
<span>  <span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span><span class='nv'>middle</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>eval_fn</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nf'>deep</span><span class='o'>(</span><span class='nv'>eval_fn</span>, <span class='nf'><a href='https://rlang.r-lib.org/reference/stack.html'>current_env</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span><span class='nv'>deep</span> <span class='o'>&lt;-</span> <span class='kr'>function</span><span class='o'>(</span><span class='nv'>eval_fn</span>, <span class='nv'>eval_env</span><span class='o'>)</span> <span class='o'>{</span></span>
<span>  <span class='nv'>expr</span> <span class='o'>&lt;-</span> <span class='nf'><a href='https://rdrr.io/r/base/substitute.html'>quote</a></span><span class='o'>(</span><span class='nf'><a href='https://rdrr.io/r/base/sys.parent.html'>parent.frame</a></span><span class='o'>(</span><span class='o'>)</span><span class='o'>)</span></span>
<span>  <span class='nf'>eval_fn</span><span class='o'>(</span><span class='nv'>expr</span>, <span class='nv'>eval_env</span><span class='o'>)</span></span>
<span><span class='o'>}</span></span>
<span></span>
<span><span class='c'># With eval_bare(), we do get the expected environment:</span></span>
<span><span class='nf'>fn</span><span class='o'>(</span><span class='nf'>rlang</span><span class='nf'>::</span><span class='nv'><a href='https://rlang.r-lib.org/reference/eval_bare.html'>eval_bare</a></span><span class='o'>)</span></span>
<span><span class='c'>#&gt; $returned_env</span></span>
<span><span class='c'>#&gt; &lt;environment: 0x55cac3595df8&gt;</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; $actual_env</span></span>
<span><span class='c'>#&gt; &lt;environment: 0x55cac3595df8&gt;</span></span>
<span><span class='c'>#&gt; </span></span>
<span></span>
<span><span class='c'># But that's not the case with base::eval():</span></span>
<span><span class='nf'>fn</span><span class='o'>(</span><span class='nf'>base</span><span class='nf'>::</span><span class='nv'><a href='https://rdrr.io/r/base/eval.html'>eval</a></span><span class='o'>)</span></span>
<span><span class='c'>#&gt; $returned_env</span></span>
<span><span class='c'>#&gt; &lt;environment: 0x55cac1cc7980&gt;</span></span>
<span><span class='c'>#&gt; </span></span>
<span><span class='c'>#&gt; $actual_env</span></span>
<span><span class='c'>#&gt; &lt;environment: 0x55cac1cc2170&gt;</span></span>
<span><span class='c'>#&gt; </span></span></pre>
