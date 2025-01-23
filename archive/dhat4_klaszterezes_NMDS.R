# Overlap dhat4 matrix betoltese
olap.matrix.backup = as.matrix(read.csv2('D:/PhD/PROJECTS/TempSpatialOverlap_2015-2019/R/figures_manuscript_20231106/overlap/bnpi/full_year/olap_matrix.csv',
                                header = TRUE, stringsAsFactors = FALSE))
colnames(olap.matrix.backup) = apply(array(colnames(olap.matrix.backup)), 1, function(x){str_replace_all(x, '[.]', ' ')})
rownames(olap.matrix.backup) = colnames(olap.matrix.backup)
olap.matrix = olap.matrix.backup

# Adatok szukitese
olap.matrix = (1-olap.matrix)*lower.tri(olap.matrix)
olap.matrix = olap.matrix[-which(colnames(olap.matrix) %in% c('Meles meles', 'Rider')),
                          -which(colnames(olap.matrix) %in% c('Meles meles', 'Rider'))]

# Szükséges csomagok betöltése
library(ecodist)
library(ggplot2)
library(vegan)
library(smerc)

# Elbow módszerrel határozzuk meg az ideális klaszter számot
wss <- numeric(length = nrow(olap.matrix))
for (i in 1:(nrow(olap.matrix)-1)) {
  cat(i)
  kmeans_model <- kmeans(olap.matrix, centers = i)
  wss[i] <- sum((olap.matrix - kmeans_model$centers[kmeans_model$cluster,])^2)
}

# Megkeressük a könyök pontot
elbow_point <- elbow_point(x=1:length(wss),y=wss)

# Ábrázoljuk az inercia változását a klaszterek számának függvényében
plot(1:nrow(olap.matrix), wss, type = "b", pch = 19, frame = FALSE, main = "Elbow Method",
     xlab = "Number of Clusters", ylab = "Within Sum of Squares")
elbow_point
points(elbow_point$x, elbow_point$y, col = "red", cex = 2, pch = 4)
text(elbow_point$x, elbow_point$y, labels = sprintf("   %d", elbow_point$idx), pos = 4)

# Optimális klaszter szám
optimal_clusters <- elbow_point$idx

# Klaszterezési fa létrehozása
distance_matrix <- as.dist(olap.matrix)
hierarchical_clustering <- hclust(distance_matrix, method = "ward.D2")

# Színes klaszterek megjelenítése a dendrogramon
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
cols = gg_color_hue(optimal_clusters)
plot(hierarchical_clustering, main = "Dendrogram with Colored Clusters", xlab = "Elemek", sub = NULL)
rect.hclust(hierarchical_clustering, k = optimal_clusters, border = cols) #2:optimal_clusters)

# NMDS ábra frissítése az optimális klaszterek számával
nmds_result_optimal <- metaMDS(distance_matrix, k = 2)

# Klaszterek hozzáadása
nmds_result_optimal$clust <- cutree(hierarchical_clustering, k = optimal_clusters)

# Ábra készítése
g <- ggplot(data = data.frame(nmds_result_optimal$points, clust = factor(nmds_result_optimal$clust)),
       aes(x = nmds_result_optimal$points[, 1], y = nmds_result_optimal$points[, 2], color = clust, label = row.names(nmds_result_optimal$points))) +
  geom_point(size = 3) +
  geom_text(size = 3, hjust = 0, vjust = 0) +
  ggtitle("NMDS Plot with Cluster Coloring") +
  theme_minimal()
g
