# Left Order Forms and Binary Matrices
Duncan Wadsworth  
August 8, 2014  


```r
suppressPackageStartupMessages(library(ggplot2))
```


```r
n_features = 200
n_observations = 1000
feature_probabilities = runif(n_features)
## use vector argument recycling for the probabilities
unordered_binary_matrix = matrix(rbinom(n_features * n_observations, 1, prob = sort(feature_probabilities, decreasing = T)), nrow = n_observations, byrow = T)
## check graphically
plot(colMeans(unordered_binary_matrix), sort(feature_probabilities, decreasing = T))
```

![plot of chunk generate_matrices](./lof-binary-matrices_files/figure-html/generate_matrices.png) 


```r
bin_char_vec = "0011010110"
binToInt = function(bin_char_vec){
  n_elements = length(bin_char_vec)
  seq(0, 2^12, by )
  
  
}
```
