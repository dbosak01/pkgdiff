% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reports.R
\name{repo_breakages}
\alias{repo_breakages}
\title{Identify Breakages for a Repository}
\usage{
repo_breakages(r1 = "current", r2 = "latest")
}
\arguments{
\item{r1}{A data frame that identifies the source repository packages
and versions. The default value is "current", which means
the function will use the current versions of all packages in
the current R repository.}

\item{r2}{A data frame that identifies the target repository packages
and versions. The default value is "latest", which means
the function will use the latest versions of all packages on CRAN.}
}
\value{
A list containing a summary table and a list of difference
objects. The summary table will contain one row for each package. The
columns show the package name, source and target versions, and whether
there are any breaking changes between the specified versions.  If
breaking changes are found, the difference object for that package
will be included in the details list.
}
\description{
The \code{repo_breakages} function generates a data frame
of breakage information for multiple packages.  This function can be used
to identify breakages for a small set of packages, or even an entire repository.
}
\details{
The \code{repo_breakages} function aims to help with repository upgrades.
The purpose is to identify packages that will break if you upgrade
your repository.

The function accepts two data frames as input.
Each data frame should have two columns: "Package" and "Version".
The "r1" data frame is for your current repo.  The "r2" data frame
is for the upgrade repo.

When executed, \code{repo_breakages} will then match up the
source and target versions, run a difference operation for each package,
and look for breaking changes.

The result will be a table of package names, the source and target version,
and whether or not there are any breakages. If there are
breakages, the difference object for that package will be
added to a "details" list.  This difference object can show you
exactly which functions or function parameters were removed.

The \code{repo_breakages} function is one of primary motivations for
the \strong{pkgdiff} package.  This mass-comparison ability did not
exist in any other R package prior to \strong{pkgdiff}.
}
\examples{
# Create package vector
pkgs <- c("curl", "dplyr", "crayon", "stringr")

# Backdated versions
r1 <- data.frame(Package = pkgs,
                 Version = c("5.2.1", "1.1.4", "1.5.2", "1.5.0"))

# More recent versions
r2 <- data.frame(Package = pkgs,
                 Version = c("6.2.1", "1.1.4", "1.5.3", "1.5.1"))

# Find any breaking changes
# res <- repo_breakages(r1, r2) # - Commented out to pass CRAN checks
# Comparing curl v5.2.1/v6.2.1
# Comparing crayon v1.5.2/v1.5.3
# Comparing stringr v1.5.0/v1.5.1

# View results
# res # - Commented out to pass CRAN checks
# # A repo breakages object
# - Run Datetime: 2025-03-01 17:47 UTC
# - Summary:
#   Package Version1 Version2 Breakages
# 1    curl    5.2.1    6.2.1      TRUE
# 2   dplyr    1.1.4    1.1.4     FALSE
# 3  crayon    1.5.2    1.5.3     FALSE
# 4 stringr    1.5.0    1.5.1     FALSE
# - Details:
#   # A difference object: curl package
#   - Comparing: v5.2.1/v6.2.1
# - Breaking Changes: TRUE
# - Added Functions: 1
# - Added Parameters: 3
# - Removed Parameters:
#   - multi_download(): timeout
}
\seealso{
Use the \code{\link{pkg_repo}} function to help gather
version information for each package set.  Also see \code{\link{repo_stability}}
to collect stability information on multiple packages.

Other prepo: 
\code{\link{pkg_repo}()},
\code{\link{print.prepo}()},
\code{\link{print.rbreak}()},
\code{\link{print.rstability}()},
\code{\link{repo_stability}()}
}
\concept{prepo}
