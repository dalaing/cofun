
This corresponds fairly closely with the code used in the blog series.

Where they have diverged, I'll try to fix things up once the dust settles by 

* update the blog post
* adding the various relevant versions of the code back in as additional modules

Although there's still a bit more dust to stir up :)

# Building everything with Cabal sandboxes

* Run `cabal sandbox init --sandbox ./sandbox` in this directory
* Change into the directory of the project you want to build
* Run `cabal sandbox init --sandbox ../sandbox`
* Run `cabal install --only-dependencies`
* Run `cabal install`

## Suggested order to build things in

The order will matter a bit, due to the internal dependencies.

My suggestion is:

* cofun-pairing
* cofun-coproduct
* cofun-console
* cofun-network
* adder
* adder-components
* adder-coproduct

# Building everything with Nix

* Head into whichever directory looks interesting to you
* Run `nix-shell --command 'cabal configure'`
* Run `cabal build`, `cabal repl`, or whatever else you like
