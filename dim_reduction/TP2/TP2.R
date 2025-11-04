install.packages(c("FactoMineR", "factoextra"))
library(FactoMineR)
library(factoextra)

data(decathlon2)
head(decathlon2)

decathlon_data <- decathlon2[, 1:10]
str(decathlon_data)
summary(decathlon_data)

res.pca <- PCA(decathlon_data, scale.unit = TRUE, graph = FALSE)
fviz_pca_var(
  res.pca, col.var = "contrib", 
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  rapel = TRUE
  )

res.pca$var$cor 
# Les variables fortement corrélées appartiennent au même groupe de performance, ou les gestes se resembles
# Par exemple 100m, 110m.hurdle, 400m ont besion de vitesse et explosivité
# Pole, vault: aptitude au saut
# discus, javeline: force et technique

fviz_pca_biplot(res.pca, rapel = TRUE)
# Premier axe (Dim1): performance globale / vitesse-puissance
# Deuxième axe (Dim2): endurance ou composante technique.

fviz_pca_var(res.pca)
fviz_pca_ind(res.pca)

# Sauvegarder
write.csv(res.pca$var$coord, "resultats_ACP_decathlon.csv", row.names = TRUE)
ggsave("cercle_correlation.png")
