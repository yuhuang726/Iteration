Writing Function
================

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.962248699  0.080297485  0.246390449 -0.977049925  0.788133243
    ##  [6]  1.518781490  0.520521045  1.331014963 -1.017521663 -1.585197031
    ## [11]  0.788798555 -1.173207067 -0.598607405 -0.001353302 -1.946297664
    ## [16] -0.223767261 -0.164276561  0.373832831  0.709874293  0.267984549
    ## [21]  1.326838522  0.057989765  0.783237042 -1.364008938  0.224940116
    ## [26] -0.982137657  1.419523051  1.747480181 -0.062839332 -1.127125073

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

    ##  [1] -0.962248699  0.080297485  0.246390449 -0.977049925  0.788133243
    ##  [6]  1.518781490  0.520521045  1.331014963 -1.017521663 -1.585197031
    ## [11]  0.788798555 -1.173207067 -0.598607405 -0.001353302 -1.946297664
    ## [16] -0.223767261 -0.164276561  0.373832831  0.709874293  0.267984549
    ## [21]  1.326838522  0.057989765  0.783237042 -1.364008938  0.224940116
    ## [26] -0.982137657  1.419523051  1.747480181 -0.062839332 -1.127125073

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
