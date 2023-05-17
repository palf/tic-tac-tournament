# Tic Tac Tournament

This project allows you to develop and execute agents to play tic-tac-toe (noughts & crosses).

You can specify which agents to use, and how many games to play. Results will be printed to `stdout`.

This is a test-bed to try out improvements to the reinforcement learning algorithm.

## Usage

```sh
$ stack build
$ stack exec versus
```

This will use the default options, random vs random once.

To get a list of all supported options, use:


### Versus

```sh
$ stack exec versus -- --help
```

We're using `optparse-applicative` to handle command-line arguments.

For a more complete example, try:

```sh
$ stack exec versus -- -X perfect -O learner02 -t 10000
```

This should take less than one minute.

### Train

This command will train the learner model and create a `weights.json` file.

This file is not used by the `versus` executable; it's for debugging.

```sh
$ stack exec train -- --help

$ stack exec train -- -t 10000
```
