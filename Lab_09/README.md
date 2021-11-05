Lab\_09
================
NickyNie

## Problem 1: Think

Give yourself a few minutes to think about what you just learned. List
three examples of problems that you believe may be solved using parallel
computing, and check for packages on the HPC CRAN task view that may be
related to it.

1.  Classification of a great number of data/images into several
    categories May use randomForestSRC, tensorflow
2.  Training model using CNN/RNN in CV/NLP projects May use tensorflow,
    batch
3.  Simulation of random trials May use parSim

## Problem 2: Before you

The following functions can be written to be more efficient without
using parallel:

This function generates a n x k dataset with all its entries distributed
poission with mean lambda.

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  # return x
  x
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
   matrix(rpois(n*k, lambda), nrow = n, ncol = k, byrow = TRUE)
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(n=1000),
  fun1alt(n=1000), unit = "relative"
)
```

    ## Unit: relative
    ##               expr      min       lq     mean   median       uq      max neval
    ##     fun1(n = 1000) 33.51906 36.79816 44.37692 43.73321 50.54922 12.63375   100
    ##  fun1alt(n = 1000)  1.00000  1.00000  1.00000  1.00000  1.00000  1.00000   100

Find the column max (hint: Checkout the function max.col()).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(5e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  idx <- max.col(t(x))  
  x[cbind(idx, 1:ncol(x))]
}

# Do we get the same?
all(fun2(x) == fun2alt(x))
```

    ## [1] TRUE

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x), unit = "relative"
)
```

    ## Unit: relative
    ##        expr      min       lq     mean   median       uq      max neval
    ##     fun2(x) 11.16718 8.383027 8.534591 8.349981 8.344346 2.769684   100
    ##  fun2alt(x)  1.00000 1.000000 1.000000 1.000000 1.000000 1.000000   100

## Problem 3: Parallelize everyhing

We will now turn our attention to non-parametric bootstrapping. Among
its many uses, non-parametric bootstrapping allow us to obtain
confidence intervals for parameter estimates without relying on
parametric assumptions.

The main assumption is that we can approximate many experiments by
resampling observations from our original dataset, which reflects the
population.

This function implements the non-parametric bootstrap:

``` r
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  cl <- makePSOCKcluster(ncpus)
  clusterSetRNGStream(cl, 123)  # Equivalent to `set.seed(123)`
  clusterExport(cl, varlist = c("idx", "dat", "stat"), envir = environment())
  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl = cl, seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: Stop cluster
  stopCluster(cl)
  
  ans
  
}
```

Use the previous pseudocode, and make it work with parallel. Here is
just an example for you to try:

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 5e4

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1381408 0.04697934
    ## x            4.8693718 5.04465851

``` r
##                   2.5%      97.5%
## (Intercept) -0.1372435 0.05074397
## x            4.8680977 5.04539763
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353

``` r
##                  2.5 %     97.5 %
## (Intercept) -0.1379033 0.04797344
## x            4.8650100 5.04883353
```

Check whether your version actually goes faster than the non-parallel
version:

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##   0.087   0.011   3.479

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##   0.132   0.012   2.254

## Problem 4: Compile this markdown document using Rscript

Once you have saved this Rmd file, try running the following command in
your terminal:

Rscript –vanilla -e
‘rmarkdown::render(“\[full-path-to-your-Rmd-file.Rmd\]”)’ & Where
\[full-path-to-your-Rmd-file.Rmd\] should be replace with the full path
to your Rmd file… :).
