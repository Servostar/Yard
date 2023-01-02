## Yard

Yard is an funny programming language compiler and interpreter written in pure Rust.  
Its a functional language, having no concept of classes or inheritance. Types are static and must be known at compile time.

### Primitve data types 

The language has 4 primitive data types:

*   `bool` a 1 byte boolean value which is either true or false. It can also be initalised with a random value at compile time iwht `maybe`
*   `int` a 8 byte two's complement signed integer with 64-bit of storage space
*   `rat` a 8 byte IEEE-754 double precision floating point value
*   `str` a dynamically sized immutable string wich for now can only be used with the concatonation operator `..`

NOTE: no custom data types such as structs or classes are supported.

### Keywords

<table><tbody><tr><td>Syntax</td><td>Description</td><td>Example</td></tr><tr><td>despite</td><td>Inverted if. Only executes the following block if the expression is false</td><td><pre><code class="language-plaintext">despite 3 &gt; 4 {
    // code block
}</code></pre></td></tr><tr><td>loop</td><td>While(true)-Loop. Loops forever until either yield, ret, break are used.<br>Loop take no boolean expression.</td><td><pre><code class="language-plaintext">loop {
    // code block
}</code></pre></td></tr><tr><td>cont</td><td>Short form for “continue”. Jumps to the next iteration of a loop</td><td><pre><code class="language-plaintext">loop {
    cont;
}</code></pre></td></tr><tr><td>break</td><td>Exits a loop immediately</td><td><pre><code class="language-plaintext">loop {
    break;
}</code></pre></td></tr><tr><td>yield</td><td>Returns from a function with an argument as return value</td><td><pre><code class="language-plaintext">foo() = int {
    yield 4
}</code></pre></td></tr><tr><td>ret</td><td>Short form for “return”. This keyword takes no argument and returns from a function. NOTE: if ret is used in a function with a return type, an error occurs.</td><td><pre><code class="language-plaintext">foo() {
    ret    // exit function
}</code></pre></td></tr><tr><td>please</td><td>Decides on runtime wether a block should be executed or not.</td><td><pre><code class="language-plaintext">please {
    // code
}</code></pre></td></tr><tr><td>unless</td><td>Works like a while loop except inverted. it will only run if the condition is false.</td><td><pre><code class="language-plaintext">unless x &gt; 5 {
    // code
}</code></pre></td></tr><tr><td>goto</td><td>jumps to a specified label</td><td><pre><code class="language-plaintext">'label
// code
goto label;</code></pre></td></tr></tbody></table>

### Example code

```plaintext
-- compute the factorial
-- in a recursive and functional way
fac(x:int) = int {
    despite x != 1 {
        yield 1;
    }

    yield fac(x - 1) * x
}

number = rat 34	# this is a function without arguments and a braceless body

// main function
main = int {

    result = fac(number);

    println("The Factorial of " .. number .. " is: " .. result);

    yield 0;
}
```