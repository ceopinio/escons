# The `escons` R package
![R CMD check](https://github.com/griverorz/escons/actions/workflows/r.yml/badge.svg)

`escons` is an R package from the Centre d'Estudis d'Opinió to
simulate seat allocations using data from electoral surveys.

The package provides two main functions. `simulate` is the main tool
for simulation. It consumes the vote shares and an associated margin
of error for each party in each district and outputs an array of seat
allocations in each district using the Catalan electoral system. The
package also provides a method of `as.data.frame` to transform the
output of `simulate` into a structure that is easier to analyze.

# An example 

Consider, for instance, a dataset with the results of the February
2021 elections to fill the 135 seats of the Parliament of Catalonia.

```r
res2021 <- data.frame("Barcelona"=c(25.0, 20.4, 17.9, 7.8, 7.8, 6.3, 6.1, 4.0),
                      "Girona"=c(15.2, 21.8, 32.7, 6.1, 4.0, 9.0, 3.3, 2.0),
                      "Lleida"=c(15.0, 26.6, 28.0, 5.5, 3.2, 7.4, 3.2, 3.5),
                      "Tarragona"=c(20.0, 24.5, 19.4, 9.4, 4.9, 6.8, 5.2, 4.3))/100
```

Note that the package will consume data in proportions, which is why
the dataset is divided by 100.

The package provides a utility function `moe` to calculate the margin
of error for each vote share. The function takes two relevant
arguments. One defines the confidence level associated to the margin
of error. The other defines the sample size _for each district._

```r
sig2021 <- moe(res2021, N=3000, level=.95)
```

Users have the option to declare the name of the parties -- otherwise,
parties will be identified using the `row.names` attribute of the
data. 

```r
parties <- c("PSC", "ERC", "JxCat", "Vox", "ECP–PEC", "CUP–G", "Cs", "PP")
```

The main function of the package is `simulate`. It will make 1,000 (by
default) extractions from a normal distributino centered at the vote
share with standard deviation defined by the margin of error. The
function also provides arguments to set the electoral threshold and
the size of the district. The defaults values for these arguments use
the current values in the Catalan electoral system -- an electoral
threshold of 3% in each district and allocation of 85 seats for
Barclona, 17 for Girona, 18 for Tarragona, and 15 for Lleida.

```r
res <- simulate(res2021, sig2021, parties)
```

The resulting object from the simulation is a 3-dimensional array. To
facilitate the use of the simulations for data analysis, the package
exports a method `as.data.frame` that creates a "long" representation
of the data

```r
res <- as.data.frame(res)
```

The `as.data.frame` method will also attach the names of the parties
and calculate party totals across districts to the resulting object.

## Warning

The (unexported) function `dhondt` that is invoked during the
simulation resolves ties in a district at random. This is, of course,
different from how the electoral system resolves ties. Therefore,
there is a possibility of small differences between the seats
allocations produced by the package and the official ones -- a
difference that disappears across simulations.

# Installation

The package is not available on CRAN and must be installed from the
GitHub repository directly. 

```R
install.packages("remotes")
remotes::install_github("griverorz/escons", subdir="escons")
```
