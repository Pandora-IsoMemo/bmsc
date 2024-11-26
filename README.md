# BMSC (constraint-estimation)

## Local Installation

* to re-generate `R/rstanmodel.R` and the C++ Source Code in src use 
`rstantools::rstan_config()
* Next, execute the `./createMakeVars` script. This step is essential to ensure
 `devtools::check()` functions correctly.
* After that you can install and compile the package e.g. `devtools::load_all()`
