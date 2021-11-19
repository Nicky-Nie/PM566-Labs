Assignment\_04
================
NickyNie
11/18/2021

# HPC

## Problem 1: Make sure your code is nice

Rewrite the following R functions to make them faster. It is OK (and
recommended) to take a look at Stackoverflow and Google

``` r
# Total row sums
fun1 <- function(mat) {
  n <- nrow(mat)
  ans <- double(n) 
  for (i in 1:n) {
    ans[i] <- sum(mat[i, ])
  }
  ans
}

fun1alt <- function(mat) {
  rowSums(mat)
}

# Cumulative sum by row
fun2 <- function(mat) {
  n <- nrow(mat)
  k <- ncol(mat)
  ans <- mat
  for (i in 1:n) {
    for (j in 2:k) {
      ans[i,j] <- mat[i, j] + ans[i, j - 1]
    }
  }
  ans
}

fun2alt <- function(mat) {
  ans2 <- as.data.table(t(mat))
  ans2 <- cumsum(ans2)
  ans2 <- t(ans2)
  ans2
}


# Use the data with this code
set.seed(2315)
dat <- matrix(rnorm(200 * 100), nrow = 200)

# Test for the first
microbenchmark::microbenchmark(
  fun1(dat),
  fun1alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq    mean   median       uq       max neval
    ##     fun1(dat) 7.628543 9.766509 6.01895 9.778444 9.189563 0.2563492   100
    ##  fun1alt(dat) 1.000000 1.000000 1.00000 1.000000 1.000000 1.0000000   100

``` r
# Test for the second
microbenchmark::microbenchmark(
  fun2(dat),
  fun2alt(dat), unit = "relative", check = "equivalent"
)
```

    ## Unit: relative
    ##          expr      min       lq     mean   median      uq      max neval
    ##     fun2(dat) 1.000000 1.000000 1.000000 1.000000 1.00000 1.000000   100
    ##  fun2alt(dat) 1.699268 1.793179 1.875168 1.732405 1.66052 2.138545   100

The last argument, check = “equivalent”, is included to make sure that
the functions return the same result.

## Problem 2: Make things run faster with parallel computing

The following function allows simulating PI

``` r
sim_pi <- function(n = 1000, i = NULL) {
  p <- matrix(runif(n*2), ncol = 2)
  mean(rowSums(p^2) < 1) * 4
}

# Here is an example of the run
set.seed(156)
sim_pi(1000) # 3.132
```

    ## [1] 3.132

In order to get accurate estimates, we can run this function multiple
times, with the following code:

``` r
# This runs the simulation a 4,000 times, each with 10,000 points
set.seed(1231)
system.time({
  ans <- unlist(lapply(1:4000, sim_pi, n = 10000))
  print(mean(ans))
})
```

    ## [1] 3.14124

    ##    user  system elapsed 
    ##   3.288   0.797   4.101

Rewrite the previous code using parLapply() to make it run faster. Make
sure you set the seed using clusterSetRNGStream():

``` r
cluster <- makeCluster(4L)
clusterSetRNGStream(cluster,iseed = 1111)
system.time({
  ans <- unlist(parLapply(cluster, 1:4000, sim_pi, n=1000))
  print(mean(ans))
})
```

    ## [1] 3.140899

    ##    user  system elapsed 
    ##   0.003   0.000   0.199

``` r
stopCluster(cluster)
```

# SQL

Setup a temporary database by running the following chunk

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
film <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film.csv")
film_category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/film_category.csv")
category <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/category.csv")

# Copy data.frames to database
dbWriteTable(con, "film", film)
dbWriteTable(con, "film_category", film_category)
dbWriteTable(con, "category", category)
```

When you write a new chunk, remember to replace the r with sql,
connection=con. Some of these questions will reqruire you to use an
inner join. Read more about them here
<https://www.w3schools.com/sql/sql_join_inner.asp>

## Question 1

How many many movies is there avaliable in each rating catagory.

``` sql
SELECT rating,
  COUNT (film_id) AS Counts
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | Counts |
| :----- | -----: |
| G      |    180 |
| NC-17  |    210 |
| PG     |    194 |
| PG-13  |    223 |
| R      |    195 |

5 records

</div>

## Question 2

What is the average replacement cost and rental rate for each rating
category.

``` sql
SELECT rating,
  AVG(replacement_cost) AS avg_replcement,
  AVG(rental_rate) AS avg_rental
FROM film
GROUP BY rating
```

<div class="knitsql-table">

| rating | avg\_replcement | avg\_rental |
| :----- | --------------: | ----------: |
| G      |        20.12333 |    2.912222 |
| NC-17  |        20.13762 |    2.970952 |
| PG     |        18.95907 |    3.051856 |
| PG-13  |        20.40256 |    3.034843 |
| R      |        20.23103 |    2.938718 |

5 records

</div>

## Question 3

Use table film\_category together with film to find the how many films
there are witth each category ID

``` sql
SELECT category_id,
  COUNT (*) AS Counts
FROM film AS f
  INNER JOIN film_category AS c
ON f.film_id = c.film_id
GROUP BY category_id
```

<div class="knitsql-table">

| category\_id | Counts |
| :----------- | -----: |
| 1            |     64 |
| 2            |     66 |
| 3            |     60 |
| 4            |     57 |
| 5            |     58 |
| 6            |     68 |
| 7            |     62 |
| 8            |     69 |
| 9            |     73 |
| 10           |     61 |

Displaying records 1 - 10

</div>

## Question 4

Incorporate table category into the answer to the previous question to
find the name of the most popular category.

``` sql
SELECT film_category.category_id,category.name,
  COUNT(*) AS count
FROM film_category
  INNER JOIN film ON film_category.film_id=film.film_id
  INNER JOIN category ON film_category.category_id=category.category_id
GROUP BY category.category_id
ORDER BY count DESC
```

<div class="knitsql-table">

| category\_id | name        | count |
| -----------: | :---------- | ----: |
|           15 | Sports      |    74 |
|            9 | Foreign     |    73 |
|            8 | Family      |    69 |
|            6 | Documentary |    68 |
|            2 | Animation   |    66 |
|            1 | Action      |    64 |
|           13 | New         |    63 |
|            7 | Drama       |    62 |
|           14 | Sci-Fi      |    61 |
|           10 | Games       |    61 |

Displaying records 1 - 10

</div>

The most popular category is sports.
