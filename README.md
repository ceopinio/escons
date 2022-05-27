# The `escons` R package

`escons` is an R package from the Centre d'Estudis d'Opinio to
simulate seat allocations from electoral surveys using the Spanish
electoral system.

The package provides two main functions. `simulate` is the main tool
for simulation. It consumes the vote shares and an associated margin
of error for each party in each district and outputs an array of seat
allocations in each district. The package also provides a method of
`as.data.frame` to transform the output of `simulate` into a structure
that is easier to analyze.

# An example 

Electoral results from the Catalan elections of 2021. Note that the
function consumes data in proportions. 

```r
res2021 <- data.frame("Barcelona"=c(25.0, 20.4, 17.9, 7.8, 7.8, 6.3, 6.1, 4.0),
                      "Girona"=c(15.2, 21.8, 32.7, 6.1, 4.0, 9.0, 3.3, 2.0),
                      "Lleida"=c(15.0, 26.6, 28.0, 5.5, 3.2, 7.4, 3.2, 3.5),
                      "Tarragona"=c(20.0, 24.5, 19.4, 9.4, 4.9, 6.8, 5.2, 4.3))/100
```

The package provides a utility function `moe` to calculate the margin
of error for each vote share. Note that the N refers to the the sample
size for each district. 

```r
sig2021 <- moe(res2021, N=3000, level=.95)
```

Users have an option to declare the name of the parties 

```r
parties <- c("PSC", "ERC", "JxCat", "Vox", "ECP–PEC", "CUP–G", "Cs", "PP")
```

The default number of simulatins is 1,000. The function also provides
arguments to set the electoral threshold and the size of the district.
The defaults use the values for Catalonia -- an electoral threshold of
3% in each district and allocation of 85 seats for Barclona, 17 for
Girona, 15 for Tarragona, and 18 for Lleida.

```r
res <- simulate(res2021, sig2021, parties)
str(res)
```

The object `res` is an 3-dimensional array. To facilitate its use in
data analysis, the package offers a method `as.data.frame` that
creates "long" representation of the data

```r
res <- as.data.frame(res)
head(res)
```

## Warning

The (unexported) function `dhondt` that is invoked during the
simulation resolves ties in a district at random. The electoral system
allocates things differently.


# Installation

The package is not available on CRAN and must be installed from the
GitHub repository directly.

```R
install.packages("remotes")
remotes::install_github("griverorz/escons")
```
