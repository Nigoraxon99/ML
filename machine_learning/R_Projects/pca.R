# Sys.info()["machine"]
library(factoextra)
library(RWeka)
data <- read.arff("5year.arff")
correlation_matrix <- cor(data[, -1])
pca_result <- prcomp(data[, -1], center = TRUE, scale. = TRUE)
summary(pca_result)
fviz_eig(pca_result, addlabels = TRUE)
fviz_pca_biplot(pca_result, repel = TRUE)
cos2 <- pca_result$x[, 1:2]^2
fviz_pca_var(pca_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_pca_biplot(pca_result, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
