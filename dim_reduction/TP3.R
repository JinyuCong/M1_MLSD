library(FactoMineR)
library(factoextra)

data(decathlon2)
head(decathlon2)

decathlon_data <- decathlon2[, 1:10]

res.pca <- PCA(decathlon_data, scale.unit = TRUE, graph = FALSE)

# Tracer le plan des individus
fviz_pca_ind(
  res.pca, 
  geom.ind = "point", 
  col.ind = "cos2",
  gradient.cols = c("red", "blue", "yellow"),
  repel= TRUE
  )
