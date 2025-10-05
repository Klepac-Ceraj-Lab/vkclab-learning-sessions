install.packages("pls")
library(pls)

#| code-fold: true
#| code-summary: "Show dependency codeblock"
#| echo: true
options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("pls", "ggplot2", "gridExtra")
to_install <- packages[!packages %in% installed.packages()[,"Package"]]
if(length(to_install)) install.packages(to_install)
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))


#| code-fold: false
dat <- data.frame(
    "MGX_Shannon_Index"     = c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2.0, 1.0),
    "MGX_InvSimpson_Index"  = c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1),
    "MBX_FreeIron"          = c(0.5, 1.5, 0.3, 0.8, 0.4, 0.7, 1.2, 1.3),
    "MBX_Ferritin"          = c(1.0, 0.5, 1.2, 1.3, 1.0, 0.9, 1.5, 0.6),
    "AnemiaRiskScore"       = c(10.2,5.4,11.0, 9.1,12.5,10.4, 8.5, 6.0)
)
dat

#| code-fold: false
X <- dat[, c("MGX_Shannon_Index", "MGX_InvSimpson_Index", "MBX_FreeIron", "MBX_Ferritin")]
y <- dat[, "AnemiaRiskScore"]

print(summary(dat))

