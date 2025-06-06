% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diff.R
\name{print.pdiff}
\alias{print.pdiff}
\title{Print a Package Difference Object}
\usage{
\method{print}{pdiff}(x, ..., verbose = TRUE)
}
\arguments{
\item{x}{The package difference to print}

\item{...}{Follow-on parameters to the print function}

\item{verbose}{Whether to print all added and removed functions
and function parameters.  Default is TRUE.  If FALSE, the function
will print removed functions and parameters, but only print
counts of added functions and parameters.}
}
\value{
The package difference object, invisibly.
}
\description{
Print routine for a package difference object of class "pdiff".
}
\examples{
# View versions
pkg_versions("rvest")
#    Package Version           FileName    Release Size
# 1    rvest   1.0.4 rvest_1.0.4.tar.gz 2024-02-12 113K
# 2    rvest   1.0.3 rvest_1.0.3.tar.gz 2022-08-20  92K
# 3    rvest   1.0.2 rvest_1.0.2.tar.gz 2021-10-17  92K
# 4    rvest   1.0.1 rvest_1.0.1.tar.gz 2021-07-26  92K
# 5    rvest   1.0.0 rvest_1.0.0.tar.gz 2021-03-09  92K
# 6    rvest   0.3.6 rvest_0.3.6.tar.gz 2020-07-25 2.7M
# 7    rvest   0.3.5 rvest_0.3.5.tar.gz 2019-11-09 1.1M
# 8    rvest   0.3.4 rvest_0.3.4.tar.gz 2019-05-15 1.6M
# 9    rvest   0.3.3 rvest_0.3.3.tar.gz 2019-04-11 1.6M
# 10   rvest   0.3.2 rvest_0.3.2.tar.gz 2016-06-17 1.5M
# 11   rvest   0.3.1 rvest_0.3.1.tar.gz 2015-11-11 1.5M
# 12   rvest   0.3.0 rvest_0.3.0.tar.gz 2015-09-23 1.5M
# 13   rvest   0.2.0 rvest_0.2.0.tar.gz 2015-01-01 5.2M
# 14   rvest   0.1.0 rvest_0.1.0.tar.gz 2014-11-22 5.1M

# Find differences in latest version
res <- pkg_diff("rvest", "1.0.3", "1.0.4")

# View differences
print(res)
# # A difference object: rvest package
# - Comparing: v1.0.3/v1.0.4
# - Breaking Changes: FALSE
# - Added Functions:
#   - html_element.LiveHTML()
#   - html_elements.LiveHTML()
#   - html_table.LiveHTML()
#   - LiveHTML()
#   - read_html_live()
}
\seealso{
Other pdiff: 
\code{\link{pkg_cache}()},
\code{\link{pkg_diff}()},
\code{\link{pkg_info}()},
\code{\link{pkg_stability}()},
\code{\link{pkg_versions}()},
\code{\link{print.pcache}()},
\code{\link{print.pdiff_score}()},
\code{\link{print.pinfo}()}
}
\concept{pdiff}
