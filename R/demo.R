

# Goals:
# 1. Stability rating.
# 2. Breaking changes report for entire repository/list of packages.
# 3. Drill-down reports on packages with breaking changes.


# Existing resources
# https://diffify.com/R
# packageDiff https://cran.r-project.org/web/packages/packageDiff/index.html
# diffr https://cran.r-project.org/web/packages/diffr/index.html


# packageDiff demo
# @noRd
# demo <- function() {
#
# dir <- system.file("extdata", package = "sassy")
#
# library(packageDiff)
#
# # Compare two versions - Not many differences
# p1 <- pkgInfo("c:/packages/logr_1.3.7.tar.gz")
# p2 <- pkgInfo("c:/packages/logr_1.3.8.tar.gz")
#
# pkgDiff(p1, p2)
#
# # Compare two versions - Lots of differences
# p1 <- pkgInfo("c:/packages/logr_1.3.4.tar.gz")
# p2 <- pkgInfo("c:/packages/logr_1.3.8.tar.gz")
#
# pkgDiff(p1, p2)
#
#
# # Compare local with CRAN
# p1 <- pkgInfo("c:/packages/logr_1.3.6.tar.gz")
# p2 <- pkgInfo("https://cran.r-project.org/src/contrib/logr_1.3.8.tar.gz")
#
# pkgDiff(p1, p2)
#
#
# }
