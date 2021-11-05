Lab\_10
================
NickyNie
2021/11/5

# Setup

``` r
# install.packages(c("RSQLite", "DBI"))

library(RSQLite)
library(DBI)

# Initialize a temporary in memory database
con <- dbConnect(SQLite(), ":memory:")

# Download tables
actor <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/actor.csv")
rental <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/rental.csv")
customer <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/customer.csv")
payment <- read.csv("https://raw.githubusercontent.com/ivanceras/sakila/master/csv-sakila-db/payment_p2007_01.csv")

# Copy data.frames to database
dbWriteTable(con, "actor", actor)
dbWriteTable(con, "rental", rental)
dbWriteTable(con, "customer", customer)
dbWriteTable(con, "payment", payment)
dbListTables(con)
```

    ## [1] "actor"    "customer" "payment"  "rental"

### TIP: Use can use the following QUERY to see the structure of a table

``` sql
PRAGMA table_info(actor)
```

``` r
x1
```

    ##   cid        name    type notnull dflt_value pk
    ## 1   0    actor_id INTEGER       0         NA  0
    ## 2   1  first_name    TEXT       0         NA  0
    ## 3   2   last_name    TEXT       0         NA  0
    ## 4   3 last_update    TEXT       0         NA  0

### SQL references:

<https://www.w3schools.com/sql/>

# Exercise 1

Retrive the actor ID, first name and last name for all actors using the
actor table. Sort by last name and then by first name.

``` r
dbGetQuery(con, "
/* This is COMMENT! */
SELECT actor_id, first_name, last_name
FROM actor /* YOU CAN ADD COMMENTS USING
MULTIPLE LINES! */
ORDER by last_name, first_name 
LIMIT 5")
```

    ##   actor_id first_name last_name
    ## 1       58  CHRISTIAN    AKROYD
    ## 2      182     DEBBIE    AKROYD
    ## 3       92    KIRSTEN    AKROYD
    ## 4      118       CUBA     ALLEN
    ## 5      145        KIM     ALLEN

# Exercise 2

Retrive the actor ID, first name, and last name for actors whose last
name equals ‘WILLIAMS’ or ‘DAVIS’.

``` sql
SELECT actor_id, first_name, last_name
FROM actor
WHERE last_name IN ('WILLIAMS', 'DAVIS')
```

<div class="knitsql-table">

| actor\_id | first\_name | last\_name |
| --------: | :---------- | :--------- |
|         4 | JENNIFER    | DAVIS      |
|        72 | SEAN        | WILLIAMS   |
|       101 | SUSAN       | DAVIS      |
|       110 | SUSAN       | DAVIS      |
|       137 | MORGAN      | WILLIAMS   |
|       172 | GROUCHO     | WILLIAMS   |

6 records

</div>

# Exercise 3

Write a query against the rental table that returns the IDs of the
customers who rented a film on July 5, 2005 (use the rental.rental\_date
column, and you can use the date() function to ignore the time
component). Include a single row for each distinct customer ID.

``` sql
SELECT DISTINCT customer_id, rental_date
FROM rental
WHERE date(rental_date) = '2005-07-05'
LIMIT 5
```

<div class="knitsql-table">

| customer\_id | rental\_date        |
| -----------: | :------------------ |
|          565 | 2005-07-05 22:49:24 |
|          242 | 2005-07-05 22:51:44 |
|           37 | 2005-07-05 22:56:33 |
|           60 | 2005-07-05 22:57:34 |
|          594 | 2005-07-05 22:59:53 |

5 records

</div>

# Exercise 4

## Exercise 4.1

Construct a query that retrives all rows from the payment table where
the amount is either 1.99, 7.99, 9.99.

``` r
q <- dbSendQuery(con, "
SELECT *
FROM payment
WHERE amount IN (1.99, 7.99, 9.99)"
)
dbFetch(q, n = 10)
```

    ##    payment_id customer_id staff_id rental_id amount               payment_date
    ## 1       16050         269        2         7   1.99 2007-01-24 21:40:19.996577
    ## 2       16056         270        1       193   1.99 2007-01-26 05:10:14.996577
    ## 3       16081         282        2        48   1.99 2007-01-25 04:49:12.996577
    ## 4       16103         294        1       595   1.99 2007-01-28 12:28:20.996577
    ## 5       16133         307        1       614   1.99 2007-01-28 14:01:54.996577
    ## 6       16158         316        1      1065   1.99 2007-01-31 07:23:22.996577
    ## 7       16160         318        1       224   9.99 2007-01-26 08:46:53.996577
    ## 8       16161         319        1        15   9.99 2007-01-24 23:07:48.996577
    ## 9       16180         330        2       967   7.99 2007-01-30 17:40:32.996577
    ## 10      16206         351        1      1137   1.99 2007-01-31 17:48:40.996577

``` r
dbClearResult(q)
```

## Exercise 4.2

Construct a query that retrives all rows from the payment table where
the amount is greater then 5

``` sql
SELECT *
FROM payment
WHERE amount > 5
```

<div class="knitsql-table">

| payment\_id | customer\_id | staff\_id | rental\_id | amount | payment\_date              |
| ----------: | -----------: | --------: | ---------: | -----: | :------------------------- |
|       16052 |          269 |         2 |        678 |   6.99 | 2007-01-28 21:44:14.996577 |
|       16058 |          271 |         1 |       1096 |   8.99 | 2007-01-31 11:59:15.996577 |
|       16060 |          272 |         1 |        405 |   6.99 | 2007-01-27 12:01:05.996577 |
|       16061 |          272 |         1 |       1041 |   6.99 | 2007-01-31 04:14:49.996577 |
|       16068 |          274 |         1 |        394 |   5.99 | 2007-01-27 09:54:37.996577 |
|       16073 |          276 |         1 |        860 |  10.99 | 2007-01-30 01:13:42.996577 |
|       16074 |          277 |         2 |        308 |   6.99 | 2007-01-26 20:30:05.996577 |
|       16082 |          282 |         2 |        282 |   6.99 | 2007-01-26 17:24:52.996577 |
|       16086 |          284 |         1 |       1145 |   6.99 | 2007-01-31 18:42:11.996577 |
|       16087 |          286 |         2 |         81 |   6.99 | 2007-01-25 10:43:45.996577 |

Displaying records 1 - 10

</div>

## Exercise 4.2

Construct a query that retrives all rows from the payment table where
the amount is greater then 5 and less then 8

``` sql
SELECT *
FROM payment
WHERE amount BETWEEN 5 AND 8
```

<div class="knitsql-table">

| payment\_id | customer\_id | staff\_id | rental\_id | amount | payment\_date              |
| ----------: | -----------: | --------: | ---------: | -----: | :------------------------- |
|       16052 |          269 |         2 |        678 |   6.99 | 2007-01-28 21:44:14.996577 |
|       16060 |          272 |         1 |        405 |   6.99 | 2007-01-27 12:01:05.996577 |
|       16061 |          272 |         1 |       1041 |   6.99 | 2007-01-31 04:14:49.996577 |
|       16068 |          274 |         1 |        394 |   5.99 | 2007-01-27 09:54:37.996577 |
|       16074 |          277 |         2 |        308 |   6.99 | 2007-01-26 20:30:05.996577 |
|       16082 |          282 |         2 |        282 |   6.99 | 2007-01-26 17:24:52.996577 |
|       16086 |          284 |         1 |       1145 |   6.99 | 2007-01-31 18:42:11.996577 |
|       16087 |          286 |         2 |         81 |   6.99 | 2007-01-25 10:43:45.996577 |
|       16092 |          288 |         2 |        427 |   6.99 | 2007-01-27 14:38:30.996577 |
|       16094 |          288 |         2 |        565 |   5.99 | 2007-01-28 07:54:57.996577 |

Displaying records 1 - 10

</div>

Bonus: Count how many are

``` r
dbGetQuery(con, "
SELECT COUNT(*)
FROM payment
WHERE amount > 5")
```

    ##   COUNT(*)
    ## 1      266

Counting per staff\_id

``` r
dbGetQuery(con, "
SELECT staff_id, COUNT(*) AS N
FROM payment
/* GROUP BY goes AFTER WHERE*/
WHERE amount > 5
GROUP BY staff_id
")
```

    ##   staff_id   N
    ## 1        1 151
    ## 2        2 115

# Exercise 5

Retrive all the payment IDs and their amount from the customers whose
last name is ‘DAVIS’.

``` sql
SELECT p.payment_id, p.amount, c.first_name, c.last_name
FROM payment AS p
  INNER JOIN customer AS c
ON p.customer_id = c.customer_id
WHERE c.last_name IN ("DAVIS")
```

<div class="knitsql-table">

| payment\_id | amount | first\_name | last\_name |
| :---------- | -----: | :---------- | :--------- |
| 16685       |   4.99 | JENNIFER    | DAVIS      |
| 16686       |   2.99 | JENNIFER    | DAVIS      |
| 16687       |   0.99 | JENNIFER    | DAVIS      |

3 records

</div>

# Exercise 6

## Exercise 6.1

Use COUNT(\*) to count the number of rows in rental

``` sql
SELECT COUNT(*) AS nrows
FROM rental
```

<div class="knitsql-table">

| nrows |
| ----: |
| 16044 |

1 records

</div>

## Exercise 6.2

Use COUNT(\*) and GROUP BY to count the number of rentals for each
customer\_id

``` sql
SELECT COUNT(*) AS n_rentals, customer_id
FROM rental
GROUP BY customer_id
```

<div class="knitsql-table">

| n\_rentals | customer\_id |
| ---------: | -----------: |
|         32 |            1 |
|         27 |            2 |
|         26 |            3 |
|         22 |            4 |
|         38 |            5 |
|         28 |            6 |
|         33 |            7 |
|         24 |            8 |
|         23 |            9 |
|         25 |           10 |

Displaying records 1 - 10

</div>

## Exercise 6.3

Repeat the previous query and sort by the count in descending order

``` sql
SELECT COUNT(*) AS n_rentals, customer_id
FROM rental
GROUP BY customer_id
ORDER BY COUNT(*) DESC
```

<div class="knitsql-table">

| n\_rentals | customer\_id |
| ---------: | -----------: |
|         46 |          148 |
|         45 |          526 |
|         42 |          236 |
|         42 |          144 |
|         41 |           75 |
|         40 |          469 |
|         40 |          197 |
|         39 |          468 |
|         39 |          178 |
|         39 |          137 |

Displaying records 1 - 10

</div>

## Exercise 6.4

Repeat the previous query but use HAVING to only keep the groups with 40
or more.

``` sql
SELECT COUNT(*) AS n_rentals, customer_id
FROM rental
GROUP BY customer_id
HAVING n_rentals >= 40
ORDER BY COUNT(*) DESC
```

<div class="knitsql-table">

| n\_rentals | customer\_id |
| ---------: | -----------: |
|         46 |          148 |
|         45 |          526 |
|         42 |          236 |
|         42 |          144 |
|         41 |           75 |
|         40 |          469 |
|         40 |          197 |

7 records

</div>

# Exercise 7

The following query calculates a number of summary statistics for the
payment table using MAX, MIN, AVG and SUM

``` sql
SELECT MAX(amount) AS max_amount, 
  MIN(amount) AS min_amount,
  AVG(amount) AS avg_amount,
  SUM(amount) AS sum_amount
FROM payment
```

<div class="knitsql-table">

| max\_amount | min\_amount | avg\_amount | sum\_amount |
| ----------: | ----------: | ----------: | ----------: |
|       11.99 |        0.99 |    4.169775 |     4824.43 |

1 records

</div>

## Exercise 7.1

Modify the above query to do those calculations for each customer\_id

``` sql
SELECT  customer_id,
        MAX(amount) AS max_amount, 
        MIN(amount) AS min_amount,
        AVG(amount) AS avg_amount,
        SUM(amount) AS sum_amount,
        COUNT(*)    AS counts
FROM payment
GROUP BY customer_id
ORDER BY counts DESC
```

<div class="knitsql-table">

| customer\_id | max\_amount | min\_amount | avg\_amount | sum\_amount | counts |
| -----------: | ----------: | ----------: | ----------: | ----------: | -----: |
|          197 |        3.99 |        0.99 |    2.615000 |       20.92 |      8 |
|          506 |        8.99 |        0.99 |    4.132857 |       28.93 |      7 |
|          109 |        7.99 |        0.99 |    3.990000 |       27.93 |      7 |
|          596 |        6.99 |        0.99 |    3.823333 |       22.94 |      6 |
|          371 |        6.99 |        0.99 |    4.323333 |       25.94 |      6 |
|          274 |        5.99 |        2.99 |    4.156667 |       24.94 |      6 |
|          269 |        6.99 |        0.99 |    3.156667 |       18.94 |      6 |
|          251 |        4.99 |        1.99 |    3.323333 |       19.94 |      6 |
|          245 |        8.99 |        0.99 |    4.823333 |       28.94 |      6 |
|          239 |        7.99 |        2.99 |    5.656667 |       33.94 |      6 |

Displaying records 1 - 10

</div>

## Exercise 7.2

Modify the above query to only keep the customer\_ids that have more
then 5 payments

``` sql
SELECT  customer_id,
        MAX(amount) AS max_amount, 
        MIN(amount) AS min_amount,
        AVG(amount) AS avg_amount,
        SUM(amount) AS sum_amount,
        COUNT(*)    AS counts
FROM payment
GROUP BY customer_id
HAVING counts > 5
ORDER BY counts DESC
```

<div class="knitsql-table">

| customer\_id | max\_amount | min\_amount | avg\_amount | sum\_amount | counts |
| -----------: | ----------: | ----------: | ----------: | ----------: | -----: |
|          197 |        3.99 |        0.99 |    2.615000 |       20.92 |      8 |
|          506 |        8.99 |        0.99 |    4.132857 |       28.93 |      7 |
|          109 |        7.99 |        0.99 |    3.990000 |       27.93 |      7 |
|          596 |        6.99 |        0.99 |    3.823333 |       22.94 |      6 |
|          371 |        6.99 |        0.99 |    4.323333 |       25.94 |      6 |
|          274 |        5.99 |        2.99 |    4.156667 |       24.94 |      6 |
|          269 |        6.99 |        0.99 |    3.156667 |       18.94 |      6 |
|          251 |        4.99 |        1.99 |    3.323333 |       19.94 |      6 |
|          245 |        8.99 |        0.99 |    4.823333 |       28.94 |      6 |
|          239 |        7.99 |        2.99 |    5.656667 |       33.94 |      6 |

Displaying records 1 - 10

</div>

# Cleanup

Run the following chunk to disconnect from the connection.

``` r
# clean up
dbDisconnect(con)
```
