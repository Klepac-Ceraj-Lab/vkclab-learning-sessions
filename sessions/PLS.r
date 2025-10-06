install.packages("pls")
install.packages("ggplot")
library(pls)
library(ggplot)
install.packages("ggpubr")
library(ggpubr)

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

summary(X1)

pls_model <- plsr(AnemiaRiskScore~MGX_Shannon_Index+MGX_InvSimpson_Index+MBX_FreeIron+MBX_Ferritin, data = dat, scale = TRUE, validation = "LOO"); summary(pls_model)
summary(pls_model)
predict_pls <- predict(pls_model, ncomp = pls_model$ncomp)
observed_pls <- y
pls_plot_data <- cbind(y, predict_pls)
colnames(pls_plot_data) <- c("Observed", "Predicted")
pls_plot_data <- as.data.frame(pls_plot_data)

pls_graph <- ggplot(pls_plot_data, aes(x=Observed, y=Predicted)) + 
geom_point(size = 3) +
stat_cor(method = "pearson", label.x = 6, label.y = 4.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Predicted vs Observed Anemia Risk Score",
    x = "Observed Anemia Risk Score",
    y = "Predicted Anemia Risk Score"
  )

pls_graph

ggsave("pls_graph.png", width = 6, height =4, dpi = 300)

pls_loadings <- data.frame(pls_model$loading.weights[,1:2], var = rownames(pls_model$loading.weights))
