# Saharax: experiments towards a Jax-like library for OCaml

At this stage, this repo is mostly a playground to experiment with a jax-like library for OCaml, building on [ocaml-xla](https://github.com/LaurentMazare/ocaml-xla).

Things we've done so far:
- Nested forward/reverse AD. Operator overloading with first-class modules has the advantage of automatically solving the `perturbation-confusion' problem in AD without requiring a tag system as in [owl](https://github.com/owlbarn/owl), but it isn't exactly the most ergonomic solution from a user perspective. Perhaps this could be fixed with appropriate PPX-based syntax extensions. Note that it's more of a proof of concept at this stage, the whole thing is currently limited to a few simple operations.
- [...]



