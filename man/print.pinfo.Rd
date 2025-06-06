% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkgInfo.R
\name{print.pinfo}
\alias{print.pinfo}
\title{Print a Package Info Object}
\usage{
\method{print}{pinfo}(x, ..., verbose = FALSE)
}
\arguments{
\item{x}{The package info to print.}

\item{...}{Follow-on parameters to the print function.}

\item{verbose}{Whether to print all function names or not.
Default is FALSE.  When FALSE, only a count of the functions
will be printed.}
}
\value{
The package info object, invisibly.
}
\description{
Print routine for a package info object of class "pinfo".
}
\examples{
# Capture package info
res <- pkg_info("patchwork")

# View package info
print(res)
# # A package info object: patchwork package
# - Version: v1.3.0
# - Release Date: 2024-09-16
# - Title: The Composer of Plots
# - Maintainer: Thomas Lin Pedersen <thomasp85@gmail.com>
#   - License: MIT + file LICENSE
# - Description: The 'ggplot2' package provides a strong API for sequentially
# building up a plot, but does not concern itself with composition of multiple
# plots. 'patchwork' is a package that expands the API to allow for
# arbitrarily complex composition of plots by, among others, providing
# mathematical operators for combining multiple plots. Other packages that try
# to address this need (but with a different approach) are 'gridExtra' and
# 'cowplot'.
# - Imports: ggplot2 (>= 3.0.0), gtable, grid, stats, grDevices, utils,
# graphics, rlang (>= 1.0.0), cli, farver
# - Suggests: knitr, rmarkdown, gridGraphics, gridExtra, ragg,
# testthat (>= 2.1.0), vdiffr, covr, png, gt (>= 0.11.0)
# - Downloads/Month: 239555
# - Repository: CRAN
# - Cached: TRUE
# - Functions: 90
}
\seealso{
Other pdiff: 
\code{\link{pkg_cache}()},
\code{\link{pkg_diff}()},
\code{\link{pkg_info}()},
\code{\link{pkg_stability}()},
\code{\link{pkg_versions}()},
\code{\link{print.pcache}()},
\code{\link{print.pdiff}()},
\code{\link{print.pdiff_score}()}
}
\concept{pdiff}
