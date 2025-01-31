# Existing resources
# https://diffify.com/R
# packageDiff https://cran.r-project.org/web/packages/packageDiff/index.html
# diffr https://cran.r-project.org/web/packages/diffr/index.html

# New package: pkgdiff

# Goals:
# 1. Stability rating.
# 2. Breaking changes report for entire repository/list of packages.
# 3. Drill-down reports on packages with breaking changes.
# 4. Ease portability with compatible versions instead of exact versions.
# 5. Scan code for known breaking changes.
#
#
#
# # Compare high level
# res1 <- get_diff("tibble", "3.1.8", "3.2.0")
#
# res1
#
# # Compare high level
# res2 <- get_diff("admiral", "1.0.2", "1.1.1")
#
# res2
#
# # Side by Side Comparison
# view_details(res1)
#
# # See stability over time
# res1 <- get_stability_score("tibble", releases = 10)
#
# res1
#
# # Upgrade list
# pkgs <- c("common", "admiral", "fmtr")
#
# # Get installed versions
# res3 <- get_installed_packages(pkgs)
#
# res3
#
# # Get latest versions
# res4 <- get_latest_data(pkgs)
#
# res4
#
# # Get Upgrade Report
# res4 <- report_breakages(res3, res4)
#
#
# # To do: Stability score
# res5 <- report_stability(pkgs, months = 12)
#

# To do: Code scan
# res5 <- scan_code(res4)

# To do: Create badge for stability score

# To do: Create website to publish package stability ratings



