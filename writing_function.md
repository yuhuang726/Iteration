Writing Function
================

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -2.4214274 -0.6392047  1.3404055  1.6150523 -0.3457357 -0.4780476
    ##  [7] -1.3667509  0.8853363 -0.0326273 -0.1779784  0.7777576  1.1363562
    ## [13]  0.2622966 -1.1576857 -0.6443700 -0.2811796  0.3658969  0.6193529
    ## [19]  1.7331700 -0.8375359 -0.2917525  1.9974777 -0.3883615 -1.2841310
    ## [25] -0.6342709 -0.3552315  0.3259086  0.6925200 -0.5180176  0.1027778

I want a function to compute z-score

``` r
z_score = function(x) {
  
  if(!is.numeric(x)) {
    stop("Imput must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Imput must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
}

z_score(x_vec)
```

    ##  [1] -2.4214274 -0.6392047  1.3404055  1.6150523 -0.3457357 -0.4780476
    ##  [7] -1.3667509  0.8853363 -0.0326273 -0.1779784  0.7777576  1.1363562
    ## [13]  0.2622966 -1.1576857 -0.6443700 -0.2811796  0.3658969  0.6193529
    ## [19]  1.7331700 -0.8375359 -0.2917525  1.9974777 -0.3883615 -1.2841310
    ## [25] -0.6342709 -0.3552315  0.3259086  0.6925200 -0.5180176  0.1027778

Try my function on some other things. This should give errors.

``` r
z_score(3)
```

    ## Error in z_score(3): Imput must have at least three numbers

``` r
z_score("my name is jeff")
```

    ## Error in z_score("my name is jeff"): Imput must be numeric

``` r
z_score(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_score(c(TRUE, TRUE, FALSE, TRUE)): Imput must be numeric

## Multiple outputs

``` r
mean_and_sd= function(x) {
  
  if(!is.numeric(x)) {
    stop("Imput must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Imput must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )

}
```

Check that the function works

``` r
x_vec = rnorm(100, mean =3, sd = 4)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.43  4.17

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(n = 100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.90  3.48

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 3) {
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
}
```

``` r
sim_mean_sd(100, 6, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.00  3.00

``` r
sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.86  2.85

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.40  3.09
