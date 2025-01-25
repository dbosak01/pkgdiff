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
# See stability over time
# res1 <- get_stability_data("tibble", releases = 10)
#
# res1
#
# # Compare high level
# res2 <- get_diff("tibble", "3.1.8", "3.2.0")
#
# res2
#
# # Side by Side Comparison
# view_details(res2, documentation = TRUE)
#
# # Upgrade Repository
# res3 <- get_installed_packages()
#
# res3

# To do: Send into something
# res4 <- get_upgrade_report(res3)

# To do: Code scan
# res5 <- scan_code(res4)

# To do: Stability score
# res5 <- get_stability_score("fmtr")

# To do: Score vector of packages

# To do: Create badge for stability score

# To do: Create website to publish package stability ratings



