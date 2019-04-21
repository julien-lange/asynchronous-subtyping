# A sound algorithm for asynchronous session subtyping


## Requirements:

You will need [ghc](https://www.haskell.org/platform/) and [GraphViz (dot)](https://www.graphviz.org/) installed on your machine (the last one is only required to generate graphs).


Depending on your setup, you might need to install extra Haskell packages (GHC's complaints will help you figure these out and you can install them with `cabal`).


## Compiling and running the tool

Here, we assume that you have a terminal open and that you are in the `tool` sub-directory.

### Compile:

* Run: `ghc Checker`

### Usage

There are two modes, in the "passive" mode you give the input parameters as file paths, in the "interactive" mode, you give them directly as strings.

#### Passive mode

* Run the following to check whether T1 is a subtype of T2  `./Checker <path-to-file-T1> <path-to-file-T2>`

* Add the `--pics` flag if you want to print graphs (this might take a while in some cases)

The files `file-Ti` must contain session of the following shape:

- '!a;?b;end
- `rec X . !a; [?d;X, ?x;!b;X]`
- `rec X. [!a;[?x;X, ?d;X], !b;[?x;X, ?d;X]]`


where, e.g., `rec X. T` is a recursion definition, `X` is a recursive call, `[!a;T1, !b;T2]` is an internal choice, `[?a;T1, ?b;T2]` is an external choice, and `end` is termination. You can omit the square brackets for unary choice (one send/receive action).

#### Interactive mode

* Run the following to check whether T1 is a subtype of T2  `./Checker -iT 'T1' 'T2'`

where T1 and T2 are session types, e.g.,

* `./Checker -i '!a;?b;end' '?b;!a;end'` (add the `--pics` flag to generate graphs)


In both mode, the tool says *True* if T1 is a subtype of T2, *False* if not, and *Maybe* if the tool is not quite sure.

When you set the `--pics` flag, the tool generates the following files in the current directory:

* `m1.png` is T1 and `m2.png` is T2 (graphical representation thereof)
* `simulation_tree.png` is the (partial) simulation tree (finite subtree)
* `candidate_trees.png` is the list of candidate subtrees
* for each candidate subtree `i`, there will be a pair of files `pti.png` and `cti.png` which are graphical representations of the systems of equations.

If the first pass is inconclusive (i.e., the tool says *Maybe*), then
another pass is executed to check `dual(T2) < dual(T1)`. In this case,
the corresponding `.png` files, prefixed by `rev_` will be produced.

It will also print something like:

```
Finite subtrees: [False,False]      --> none of the subtrees are 'finite'
Canditate subtrees: [True,True]     --> all subtrees are 'candidate')
Compatibility: [True,True]          --> all subtrees validate the 'compatibilty' requirement
2 producers/consummers printed.     --> how many pti/cti.png are generated
```


## Running the tests

In the 'script' sub-directory, run `./runtest.py`. This will print the results of 174 tests. For reference, the current version (18/04/2019) prints the results in `scripts/latest-tests.txt`.
