Lecture 07 R Functions and Packages
================
Reina Bassil
10/23/2019

## Revisit our functions from last lecture

``` r
source("http://tinyurl.com/rescale-R")
```

``` r
rescale(1:10)
```

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000

``` r
#Inputting this prints automatic error "Error in x - rng[1] : non-numeric argument to binary operator"
#This isn't very helpful, so we can add a warning/stop function to our function to let users know when things could go/are going wrong.
```

\#Write a function both.na()

\#\#We want to write a function, called both\_na(),that counts how many
positions in two inputvectors, x and y, both have a missing value

``` r
# from StackOverflow
x <- c( 1, 2, NA, 3, NA)
y <- c(NA, 3, NA, 3, 4)

is.na(x)
```

    ## [1] FALSE FALSE  TRUE FALSE  TRUE

``` r
is.na(y)
```

    ## [1]  TRUE FALSE  TRUE FALSE FALSE

``` r
which( is.na(x))
```

    ## [1] 3 5

\#Working snippet of code

``` r
is.na(x) & is.na(y)
```

    ## [1] FALSE FALSE  TRUE FALSE FALSE

``` r
which(is.na(x) & is.na(y))
```

    ## [1] 3

\#Final function

``` r
both_na <- function(x,y) {
  sum(is.na(x) & is.na(y))
}
```

``` r
both_na(x,y)
```

    ## [1] 1

``` r
x <- c(NA, NA, NA)
y1 <- c( 1, NA, NA)
y2 <- c( 1, NA, NA, NA)
y3 <- c(1, NA, NA, NA, NA, NA)

both_na(x, y1)
```

    ## [1] 2

``` r
both_na(x, y2)
```

    ## Warning in is.na(x) & is.na(y): longer object length is not a multiple of
    ## shorter object length

    ## [1] 3

``` r
both_na(x, y3)
```

    ## [1] 5

``` r
#this produces no error because y3 has 6 values, and that is a multiple of x which has 3 values
```

``` r
both_na2 <- function(x, y) {
  if (length(x) != length(y)){
    stop("Error: X and Y are not the same length")
}  
  sum(is.na(x) & is.na(y))
}
```

\#Write a function grade() to determine an overall grade from a vector
of student homework assignment scores dropping the lowest single
alignment score.

``` r
# student 1
s1 <- c(100, 100, 100, 100, 100, 100, 100, 90)
# student 2
s2 <- c(100, NA, 90, 90, 90, 90, 97, 80)
# student 3 
s3 <- c(100, NA, NA, NA, NA, NA, NA, NA)
```

``` r
grade <- function(x) {
  x[is.na(x)] <- 0
    
  drop_lowest <- x[-which.min(x)]
  
  mean(drop_lowest)
}

grade(s3)
```

    ## [1] 14.28571
