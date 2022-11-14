## Yard

Yard is an funny programming language compiler and interpreter written in pure Rust.  
Its a functional language, having no concept of classes or inheritance. Types are static and must be known at compile time.  
It contains features such as:

1.  a COMEFROM keyword (inverse goto)
2.  a `don't` code block which never executes
3.  no if. only `unless`, an inverted version of if. Meaning a block gets executed if the expression is false
4.  three spaces start a single line comment
5.  many ways of making comments: `// comment`, `:REM: comment`, `# comment`, `-- comment --` cuz we can

### Keywords

<table><tbody><tr><td>Syntax</td><td>Description</td><td>Example</td></tr><tr><td>Unless</td><td>Inverted if. Only executes the following block if the expression is false</td><td><pre><code class="language-plaintext">unless 3 &gt; 4 {
    // code block
}</code></pre></td></tr><tr><td>Loop</td><td>While(true)-Loop. Loops forever until either yield, ret, break are used.<br>Loop take no boolean expression.</td><td><pre><code class="language-plaintext">loop {
    // code block
}</code></pre></td></tr><tr><td>cont</td><td>Short form for “continue”. Jumps to the next iteration of a loop</td><td>&nbsp;</td></tr><tr><td>break</td><td>Exits a loop immediately</td><td>&nbsp;</td></tr><tr><td>yield</td><td>Returns from a function with an argument as return value</td><td><pre><code class="language-plaintext">foo() = int {
    yield 4
}</code></pre></td></tr><tr><td>ret</td><td>Short form for “return”. This keyword takes no argument and returns from a function. NOTE: if ret is used in a function with a return type, an error occurs.</td><td>&nbsp;</td></tr><tr><td>please</td><td>Decides on runtime wether a block should be executed or not.</td><td><pre><code class="language-plaintext">please {
	// code
}</code></pre></td></tr></tbody></table>

### Example code

```plaintext
foo(bar: int, abc:int) = int {
    bar + abc
}

main {
    foo(4, 5)
}
```