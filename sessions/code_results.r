library(ggplot2)

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
print("Loadings:")
print(pca_result$rotation) 
#dev gets standard deviations of pc 
print("Standard deviations:")
print(pca_result)

print(summary(pca_result))

# get PCA scores
scores <- as.data.frame(pca_result$x)
head(scores)
scores$y <- y

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


