# A sound algorithm for asynchronous session subtyping


## Requirements:

You will need [GHC](https://www.haskell.org/platform/) and [GraphViz (dot)](https://www.graphviz.org/) installed on your machine (the last one is only required to generate graphs).


Depending on your setup, you might need to extra Haskell packages (GHC and Google will help you figure these out and you can install them with `cabal`).


## Compiling and running the tool

Here, we assume that you have a terminal open and that you are in the `tool` sub-directory.

### Compile:

* Run: `ghc Checker`

### Usage

#### Passive mode

* Run the following to check whether T1 is a subtype of T2  `./Checker -T <path-to-T1> <path-to-T2>`

* Remove the `-T` flag if you want to print graphs (this might take a while in some cases)

#### Interactive mode

5
* Run the following to check whether T1 is a subtype of T2  `./Checker -T 'T1' 'T2'`

where  T1 and T2 are session types written as follows:

- `rec X . !a; [?d;X, ?x;!b;X]`
- `rec X. [!a;[?x;X, ?d;X], !b;[?x;X, ?d;X]]`
