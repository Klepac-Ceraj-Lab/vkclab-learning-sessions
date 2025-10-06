
# code set up / install packages:
options(repos = c(CRAN = "https://cloud.r-project.org"))
packages <- c("pls", "ggplot2", "gridExtra")
to_install <- packages[!packages %in% installed.packages()[,"Package"]]
if(length(to_install)) install.packages(to_install)
lapply(packages, library, character.only = TRUE) 

# buid dataset 
dat <- data.frame(
  "MGX_Shannon_Index"     = c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2.0, 1.0), #alpha diversity
  "MGX_InvSimpson_Index"  = c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1),# diversity
  "MBX_FreeIron"          = c(0.5, 1.5, 0.3, 0.8, 0.4, 0.7, 1.2, 1.3), # concentration of unbound iron
  "MBX_Ferritin"          = c(1.0, 0.5, 1.2, 1.3, 1.0, 0.9, 1.5, 0.6), #iron storage protein concentration 
  "AnemiaRiskScore"       = c(10.2,5.4,11.0, 9.1,12.5,10.4, 8.5, 6.0)
)

# split the data 
X <- dat[, c("MGX_Shannon_Index", "MGX_InvSimpson_Index", "MBX_FreeIron", "MBX_Ferritin")]
y <- dat[, "AnemiaRiskScore"]

# PCA

X_scaled <- scale(X)  # z score it 
pca_result <- prcomp(X_scaled) # pca it, get scores as results 
#rotation gets loadings 
#print("Loadings:")
#print(pca_result$rotation) 

#dev gets standard deviations of pc 
#print("Standard deviations:")
#print(pca_result$dev)

#print(summary(pca_result))

# get PCA scores
scores <- as.data.frame(pca_result$x)
#head(scores)
scores$y <- y

# scree plot -------------------------
var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

pca_df <- data.frame(
  PC = paste0("PC", 1:length(var_explained)),
  Variance = var_explained
)

ggplot(pca_df, aes(x = PC, y = Variance, group = 1)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(size = 3, color = "red") +
  geom_text(aes(label = paste0(round(Variance * 100, 1), "%")),
            vjust = -0.8, size = 4) +
  labs(
    title = "Scree Plot: Variance Explained by Each Principal Component",
    x = "Principal Component",
    y = "Proportion of Variance Explained"
  ) +
  theme_minimal(base_size = 14)

# Save plot
ggsave("pca_scree_plot.png", width = 6, height = 4, dpi = 300)

# extract the first two principal component loadings
loadings <- as.data.frame(pca_result$rotation[, 1:2])
loadings$Variable <- rownames(loadings)

#print(paste("pca loadings: "))
#print(loadings)
#print(loadings$Variable)
ggplot(loadings, aes(x = PC1, y = PC2, label = Variable)) +
  geom_point(size = 4, color = "red") +
  geom_text(vjust = -1.2, size = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +

  labs(
    title = "PCA Loadings Plot (Variables on PC1 and PC2)",
    x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "% variance)")
  ) +
  theme_minimal(base_size = 14)

  ggsave("pca_loadings_plot.png", width = 8, height = 6, dpi = 300)

# scatter plot PC1 vs PC2
ggplot(scores, aes(x=PC1, y=PC2, color = y)) +
  geom_point(size=3)+
  scale_color_gradient(low="blue", high ="red")+
  labs(
    title ="PCA colored by anemia risk",
    x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "% variance)"),
    y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "% variance)"),
    color = "Anemia Risk"
  )+
  theme_minimal(base_size=14)

ggsave("pca_plot.png", width = 6, height = 4, dpi = 300)


# PCR 
dat_scaled <- data.frame(X_scaled, y)

# run pcr with cross validation to see how many PCs to keep
pcr_model <- pcr(y ~ ., data = dat_scaled, scale =TRUE, validation = "LOO")
#print(summary(pcr_model))

print("pcr loadings: ")
loadings <- loadings(pcr_model)
print(loadings) 

# mean squared error of prediction 
validationplot(pcr_model, val.type = "MSEP")

# predict y using all the PCs 
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
    title = "PCR of Observed vs Predicted Anemia Risk",
    x = "Observed Anemia Risk Score",
    y = "Predicted Anemia Risk Score"
  ) +
  theme_minimal(base_size = 14)

# Save it
ggsave("pcr_1.png", width = 6, height = 4, dpi = 300)


# PLS 
# At some point 


