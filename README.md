<!-- badges: start -->

[![pkgdiff version](https://www.r-pkg.org/badges/version/pkgdiff)](https://cran.r-project.org/package=pkgdiff)
[![pkgdiff lifecycle](https://img.shields.io/badge/lifecycle-stable-blue.svg)](https://cran.r-project.org/package=pkgdiff)
[![pkgdiff downloads](https://cranlogs.r-pkg.org/badges/pkgdiff)](https://cran.r-project.org/package=pkgdiff)
[![pkgdiff total downloads](https://cranlogs.r-pkg.org/badges/grand-total/pkgdiff)](https://cran.r-project.org/package=pkgdiff)
[![R-CMD-check](https://github.com/dbosak01/pkgdiff/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dbosak01/pkgdiff/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

# Introduction to **pkgdiff**
<img src="man/images/pkgdiff.png" align="left" height="138px" style="margin-right:10px;height:138px"
   alt="Pkgdiff package logo"/>

Open Source languages have a reputation for being unstable.  Package developers
can remove a function or parameter at any time, potentially breaking thousands
of programs that were using that function. The situation is quite
different from languages like SAS or C, which are famous for their
stability and backward compatibility. 

The purpose of **pkgdiff** is to offer an objective approach 
for anticipating and avoiding
breaking changes in R.  It gives you a tool to evaluate contributed 
packages in terms
of overall stability, and provides a way to identify and plan for future
breakages.

The hope is that organizations will have a better way of selecting stable
packages for their programmers, and programmers will cultivate higher standards
for the packages they use.  The end goal is to enable R's use in large scale,
production environments, reduce maintenance costs, and increased confidence
in the programs we write with R.

For an overview of the package, see the package web site
[here](https://pkgdiff.r-sassy.org/articles/pkgdiff.html), or run the package
vignette using the code `vignette("pkgdiff")`.


### Installation

To install **pkgdiff**, run the following command from your R console:

    install.packages("pkgdiff")


Then put the following line at the top of your script:

    library(pkgdiff)

The **pkgdiff** package will then be loaded, and available for use in your project.

For examples and usage information, please visit the **pkgdiff**
documentation site [here](https://pkgdiff.r-sassy.org/articles/pkgdiff.html).
These examples will demonstrate the 
extraordinary usefulness of the comparison functions, and give you many ideas
on how and where to use the **pkgdiff** package.

### Getting Help

If you need help with **pkgdiff**, the best place 
to turn to is the [pkgdiff](https://pkgdiff.r-sassy.org) web site. 
This web site offers many examples, and full
documentation on every function.  

If you want to look at the code for **pkgdiff**, visit the
github page [here](https://github.com/dbosak01/pkgdiff).

If you encounter a bug or have a feature request, please submit your
issue [here](https://github.com/dbosak01/pkgdiff/issues)


