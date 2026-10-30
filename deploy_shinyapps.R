# Script to deploy Top-Journal Plot Gallery to shinyapps.io
# Usage:
# 1. If not yet configured, set your shinyapps.io token once:
#    rsconnect::setAccountInfo(name='<ACCOUNT>', token='<TOKEN>', secret='<SECRET>')
# 2. Run this script:
#    source("deploy_shinyapps.R")

if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect", repos = "https://cloud.r-project.org")
}

accs <- rsconnect::accounts()

if (nrow(accs) == 0) {
  cat("\n[!] No shinyapps.io account configured yet.\n")
  cat("Please configure your credentials first by running:\n")
  cat("  rsconnect::setAccountInfo(name = '<ACCOUNT_NAME>', token = '<TOKEN>', secret = '<SECRET>')\n")
  cat("You can find your token at: https://www.shinyapps.io/admin/#/tokens\n\n")
} else {
  cat("[+] Found configured account:", accs$name[1], "\n")
  cat("[+] Packaging and deploying 'top-journal-plot-gallery' to shinyapps.io...\n")
  
  rsconnect::deployApp(
    appDir = getwd(),
    appName = "top-journal-plot-gallery",
    appTitle = "Top-Journal Plot Gallery",
    appFiles = c(
      "app.R",
      "plot_catalog.csv",
      "theme_journal.R",
      "data",
      "plots",
      "www"
    ),
    launch.browser = TRUE,
    forceUpdate = TRUE
  )
}
