install.packages("pls")
install.packages("ggplot2")
library(pls)
library(ggplot2)
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

#Building PLS Model
pls_model <- plsr(AnemiaRiskScore~MGX_Shannon_Index+MGX_InvSimpson_Index+MBX_FreeIron+MBX_Ferritin, data = dat, scale = TRUE, validation = "LOO"); summary(pls_model)
summary(pls_model)
predict_pls <- predict(pls_model, ncomp = 1)
predict_pls_2 <- predict(pls_model, ncomp = 2)
observed_pls <- y
pls_plot_data <- as.data.frame(cbind(y, predict_pls))
pls_plot_data_2 <- as.data.frame(cbind(y, predict_pls_2))
colnames(pls_plot_data) <- c("Observed", "Predicted")
colnames(pls_plot_data_2) <- c("Observed", "Predicted")
#PLS prediction with ncomp = 1
pls_graph_1 <- ggplot(pls_plot_data, aes(x=Observed, y=Predicted)) +
geom_point(size = 3) +
stat_cor(method = "pearson", label.x = 6, label.y = 4.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Predicted vs Observed Anemia Risk Score, ncomp = 1",
    x = "Observed Anemia Risk Score",
    y = "Predicted Anemia Risk Score"
  )
pls_graph_1
ggsave("pls_graph_1.png", width = 6, height =4, dpi = 300)
#PLS prediction with ncomp = 2
pls_graph_2 <- ggplot(pls_plot_data_2, aes(x=Observed, y=Predicted)) +
geom_point(size = 3) +
stat_cor(method = "pearson", label.x = 6, label.y = 4.4) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  theme_minimal() +
  labs(
    title = "Predicted vs Observed Anemia Risk Score, ncomp = 2",
    x = "Observed Anemia Risk Score",
    y = "Predicted Anemia Risk Score"
  )
pls_graph_2
ggsave("pls_graph_2.png", width = 6, height =4, dpi = 300)
pls_loadings <- data.frame(pls_model$loading.weights[,1:2], var = rownames(pls_model$loading.weights))

# PCR
X_scaled <- scale(X)
dat_scaled <- data.frame(X_scaled, y)
# run pcr with cross validation to see how many PCs to keep
pcr_model <- pcr(y ~ ., data = dat_scaled, scale =TRUE, validation = "LOO")
#print(summary(pcr_model))
print("pcr loadings: ")
pcr_loadings <- loadings(pcr_model)
print(pcr_loadings)
# mean squared error of prediction
validationplot(pcr_model, val.type = "MSEP")
#PCR Prediction using ncomp = 1
predicted_y <- as.vector(predict(pcr_model, ncomp=1))
results <- data.frame(
  Observed = y,
  Predicted = predicted_y
)
print(results)
ggplot(results, aes(x = Observed, y = Predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "PCR of Observed vs Predicted Anemia Risk, ncomp = 1",
    x = "Observed Anemia Risk Score",
    y = "Predicted Anemia Risk Score"
  ) +
  theme_minimal(base_size = 14)
# Save it
ggsave("pcr_1.png", width = 6, height = 4, dpi = 300)
#PCR Prediction using ncomp = 2
predicted_y_2 <- as.vector(predict(pcr_model, ncomp=2))
results_2 <- data.frame(
  Observed = y,
  Predicted = predicted_y_2
)
print(results_2)
ggplot(results_2, aes(x = Observed, y = Predicted)) +
  geom_point(size = 3, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "PCR of Observed vs Predicted Anemia Risk, ncomp = 2",
    x = "Observed Anemia Risk Score",
    y = "Predicted Anemia Risk Score"
  ) +
  theme_minimal(base_size = 14)
# Save it
ggsave("pcr_2.png", width = 6, height = 4, dpi = 300)


pcr_pred <- predict(pcr_model, ncomp = 1)
pls_pred <- predict(pls_model, ncomp = 1)
## Idea: let's vary the number of `ncomp` and see if they converge!

pred_df <- data.frame(
  y = y,
  PCR = as.numeric(pcr_pred),
  PLS = as.numeric(pls_pred)
)

library(gridExtra)
p3 <- ggplot(pred_df, aes(x = y)) +
  geom_point(aes(y = PCR), color = "blue", size = 3, shape = 16) +
  geom_point(aes(y = PLS), color = "red", size = 3, shape = 17) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  ylab("Predicted") + ggtitle("PCR vs PLS (1 component)")
p3
ggsave("pcr_pls_comparison.png", width = 6, height = 4, dpi = 300)
