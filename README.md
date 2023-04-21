# TC2037
Computational Methods Project for a Deterministic Finite Automata to evaluate a mathematical expression.


# About

This project provides an implementation of a function that evaluates a mathematical expression using a Deterministic Finite Automaton (DFA). The DFA is implemented as a struct named `dfa` which contains a function, an initial state, and a list of accept states.

The main function is `arithmetic-lexer` which calls the function `evaluate-dfa`. This function takes a `dfa` object and a string as input and returns the result of the evaluation. If the input is valid, it returns the tokens found (operator, integer, float, exp, variable or closing parenthesis) with the corresponding value that is identified by the token. The function uses a transition function, `transition-fn`, to determine the appropriate state given an input. 

## Finite State Machine

The deterministic Finite Automata used for this project is the following:

![diagram](./diagram.png)  


## Installation

This project requires the Racket programming language. You can download Racket from the [official website](https://racket-lang.org/).

## Usage

To use the`arithmetic-lexer` module you can call the function with the input string that represents a mathematical expression as shown below:

```
(arithmetic-lexer "2 + 3 * (4 - 1)")
```

The function will return a list of tokens that represents the mathematical expression if it is a valid expression. For example, the above call will return the following list:

```
'(("2" int) ("+" op) ("3" int) ("*" op) ("(" open_par) ("4" int) ("-" op) ("1" int) (")" close_par))
```

It will return invalid if the expression is not a valid expression:

```
'invalid
```

## DFA Structure

The function `arithmetic-lexer` calls a function `evaluate-dfa`  which takes the dfa object and an input string. The dfa object is as follows:

```
(struct dfa (func initial-state accept-states))
```

- `func`: the transition function that determines the next state based on the current state and input character.
- `initial-state`: the starting state of the DFA.
- `accept-states`: a list of states that represent the end of a valid mathematical expression.

## Transition Function

The `transition-fn` function is used by the `evaluate-dfa` function to determine if an input character is a valid component of a mathematical expression. The function takes a state and a character as input and returns a tuple containing the next state and the token type (if any) that was found. 

## Accept states

- `'int'`: an integer value.
- `'float'`: a floating-point value.
- `'exp'`: exponent values of the form **2e-1**
- `'var'`: a variable name.
- `'close_par'`: a closing parenthesis.
- `'spa'`: whitespace


