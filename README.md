# BMSC (constraint-estimation)

### Documentation
- https://pandora-isomemo.github.io/bmsc/

## Release notes

- see `NEWS.md`

## Local Installation

* to re-generate `R/rstanmodel.R` and the C++ Source Code in src use 
`rstantools::rstan_config()`
* Next, execute the `./createMakeVars` script. This step is essential to ensure
 `devtools::check()` functions correctly.
* After that you can install and compile the package e.g. `devtools::load_all()`

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is built automatically via GitHub Actions.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```