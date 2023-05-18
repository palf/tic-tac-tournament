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

```sh
$ stack exec versus -- --help
```

### Versus

We're using `optparse-applicative` to handle command-line arguments.

For a more complete example, try:

```sh
$ stack exec versus -- -X perfect -O learner02 -t 1000
```

### Train

This command will train the learner model and create a `weights.json` file.

This file is not used by the `versus` executable; it's for debugging.

```sh
$ stack exec train -- --help

$ stack exec train -- -t 10000
```


## Agents

Agents currently implemented are:

- "random": picks a random valid move
- "minimax": uses the minimax algorithm to find a perfect play. Is a bit slow
- "perfect": uses a handcrafted table to make the perfect play. Should never lose; if it does, that's a bug
- "learner01": uses a reinforcement approach, moves all weights to either 0, 0.5 or 1
- "learner02": uses a reinforcement approach, moves all weights closer to the value of subsequent states
