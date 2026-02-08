# Кластеризация IRIS

library(ggplot2)
library(cluster)

data(iris)

x <- iris[, 1:4]
x_scaled <- scale(x)

d <- dist(x_scaled, method = "euclidean")
hc <- hclust(d, method = "ward.D2")

jpeg("dendrogram_iris.jpg", width = 2000, height = 1600, res = 300)

plot(hc, labels = FALSE, main = "Дендрограмма iris")
rect.hclust(hc, k = 2, border = "red")
rect.hclust(hc, k = 3, border = "blue")
rect.hclust(hc, k = 4, border = "green")

dev.off()

cl2 <- cutree(hc, k = 2)
cl3 <- cutree(hc, k = 3)
cl4 <- cutree(hc, k = 4)
pca <- prcomp(x_scaled)

within_sse <- function(x, cl) {
  cl <- as.integer(cl)
  sse <- 0
  for (g in sort(unique(cl))) {
    idx <- which(cl == g)
    Xg <- x[idx, , drop = FALSE]
    cg <- colMeans(Xg)
    sse <- sse + sum(rowSums((Xg - matrix(cg, nrow(Xg), ncol(Xg), byrow = TRUE))^2))
  }
  sse
}

k_grid <- 2:10

sse_vec <- sapply(k_grid, function(k) {
  cl <- cutree(hc, k = k)
  within_sse(x_scaled, cl)
})

sil_vec <- sapply(k_grid, function(k) {
  cl <- cutree(hc, k = k)
  mean(silhouette(cl, d)[, 3])
})

metrics <- data.frame(
  k = k_grid,
  SSE_within = sse_vec,
  Silhouette = sil_vec
)

p_elbow <- ggplot(metrics, aes(k, SSE_within)) +
  geom_line() + geom_point(size = 2.5) +
  theme_minimal() +
  ggtitle("Метод локтя (Ward / hclust)") +
  xlab("Число кластеров k") +
  ylab("Внутрикластерная сумма квадратов (SSE)")

ggsave("elbow_ward.jpg", p_elbow, width = 6, height = 6, dpi = 600)

p_sil <- ggplot(metrics, aes(k, Silhouette)) +
  geom_line() + geom_point(size = 2.5) +
  theme_minimal() +
  ggtitle("Silhouette score (Ward / hclust)") +
  xlab("Число кластеров k") +
  ylab("Средний silhouette score")

ggsave("silhouette_ward.jpg", p_sil, width = 6, height = 6, dpi = 600)

mean_sil <- function(cl, d) mean(silhouette(cl, d)[, 3])

sse2 <- within_sse(x_scaled, cl2)
sil2 <- mean_sil(cl2, d)

sse3 <- within_sse(x_scaled, cl3)
sil3 <- mean_sil(cl3, d)

sse4 <- within_sse(x_scaled, cl4)
sil4 <- mean_sil(cl4, d)


df2 <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  cluster = factor(cl2)
)

p2 <- ggplot(df2, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 3) +
  stat_ellipse() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "PCA (PC1 vs PC2), Ward clustering, k = 2",
    subtitle = paste0(
      "mean silhouette = ", round(sil2, 3),
      " ; within-SSE = ", round(sse2, 1)
    )
  )


ggsave(
  filename = "pca_k2.jpg",
  plot = p2,
  width = 6,
  height = 6,
  dpi = 600
)


df3 <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  cluster = factor(cl3)
)

p3 <- ggplot(df3, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 3) +
  stat_ellipse() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "PCA (PC1 vs PC2), Ward clustering, k = 3",
    subtitle = paste0(
      "mean silhouette = ", round(sil3, 3),
      "; within-SSE = ", round(sse3, 1)
    )
  )


ggsave(
  filename = "pca_k3.jpg",
  plot = p3,
  width = 6,
  height = 6,
  dpi = 600
)


df4 <- data.frame(
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  cluster = factor(cl4)
)

p4 <- ggplot(df4, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 3) +
  stat_ellipse() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    title = "PCA (PC1 vs PC2), Ward clustering, k = 4",
    subtitle = paste0(
      "mean silhouette = ", round(sil4, 3),
      " ; within-SSE = ", round(sse4, 1)
    )
  )


ggsave(
  filename = "pca_k4.jpg",
  plot = p4,
  width = 6,
  height = 6,
  dpi = 600
)

df4$Species <- factor( iris$Species, levels = c("setosa", "versicolor", "virginica"), labels = c("Ирис щетинистый", "Ирис разноцветный", "Ирис виргинский") )

p_true <- ggplot(df4, aes(PC1, PC2, color = Species)) +
  geom_point(size = 3) +
  stat_ellipse(aes(group = Species)) +
  theme_minimal() +
  ggtitle("PCA (PC1 vs PC2) раскраска по видам iris") + 
  labs(color = "Вид")

ggsave(
  filename = "pca_true.jpg",
  plot = p_true,
  width = 6,
  height = 6,
  dpi = 600
)
