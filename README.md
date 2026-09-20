# London Haskell Intro

These slides are for the introduction for the London Haskell meetup in September, at Permutive

The site is built with [Hakyll](https://jaspervdj.be/hakyll/) and deployed to
GitHub Pages by `.github/workflows/pages.yml` on every push to `main`:
<https://london-haskell.github.io/intro-slides/>

To build locally:

```sh
stack build
stack exec london-haskell-intro-exe -- watch
```
