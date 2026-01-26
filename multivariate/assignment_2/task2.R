#Task 2
library(mclust)      
    
load("beer.Rdata")
beer_mat <- as.matrix(beer)   
dim(beer_mat)  
#399  12
sum(is.na(beer_mat)) #no nulls
head(beer_mat)

#Hierarchical clustering - Ward's method 
dist_beer <- dist(t(beer_mat), method = "euclidean",diag = TRUE, upper = TRUE)^2
hc_beer <- hclust(dist_beer, method = "ward.D2")

# Dendrogram
plot(hc_beer, main = "Dendrogram Ward's Method")
rect.hclust(hc_beer, k = 3, border = 2:4)  

library(pheatmap)
pheatmap(as.matrix(dist_beer), clustering_method = "ward.D2", labels_row = colnames(beer_mat), labels_col = colnames(beer_mat))


#Split data
set.seed(1000)
trn_idx <- seq(1, nrow(beer_mat), 2)  
val_idx <- seq(2, nrow(beer_mat), 2) 
trn_mat <- beer_mat[trn_idx, ]
val_mat <- beer_mat[val_idx, ]

dim(trn_mat)# 200  12
dim(val_mat)# 199  12

#HC
hc_trn   <- hclust(dist(t(trn_mat))^2, method = "ward.D2")
hc_val   <- hclust(dist(t(val_mat))^2, method = "ward.D2")

hc_trn3 <- cutree(hc_trn, k = 3)
hc_trn4 <- cutree(hc_trn, k = 4)
hc_val3   <- cutree(hc_val, k = 3)
hc_val4   <- cutree(hc_val, k = 4)

par(mfrow = c(1,2))
plot(hc_trn, main = "HC Ward Train", sub = "", xlab = "")
rect.hclust(hc_trn, k = 3, border = "red")
rect.hclust(hc_trn, k = 4, border = "blue") 
plot(hc_val, main = "HC Ward Val", sub = "", xlab = "")
rect.hclust(hc_val, k = 3, border = "red")
rect.hclust(hc_val, k = 4, border = "blue") 

# K-means clustering 
set.seed(1000)
km_trn3 <- kmeans(t(trn_mat), centers = 3, nstart = 100)
km_trn4 <- kmeans(t(trn_mat), centers = 4, nstart = 100)
km_val3   <- kmeans(t(val_mat), centers = 3, nstart = 100)
km_val4   <- kmeans(t(val_mat), centers = 4, nstart = 100)

# Ward + K-means 
#compute centroids from Ward clusters
centroids <- function(data, k)
  {
  hc <- hclust(dist(t(data))^2, method = "ward.D2")
  cent <- cutree(hc, k)
  cents <- rowsum(t(data), cent) / as.vector(table(cent))
  km <- kmeans(t(data), centers = cents)
  }

comb_trn3 <- centroids(trn_mat, 3)
comb_trn4 <- centroids(trn_mat, 4)
comb_val3   <- centroids(val_mat, 3)
comb_val4   <- centroids(val_mat, 4)

#ARI
library(mclust)  

clusters <- list(
  hc_train3 = hc_trn3, hc_train4 = hc_trn4,
  hc_val3   = hc_val3,   hc_val4   = hc_val4,
  km_train3 = km_trn3$cluster, km_train4 = km_trn4$cluster,
  km_val3   = km_val3$cluster,   km_val4   = km_val4$cluster,
  comb_train3 = comb_trn3$cluster, comb_train4 = comb_trn4$cluster,
  comb_val3   = comb_val3$cluster,   comb_val4   = comb_val4$cluster
)

ari_res <- matrix(NA, nrow = 3, ncol = 2,dimnames = list(c("HC", "K", "W+K"), c("k=3","k=4")))

ari_res["HC", "k=3"]  <- adjustedRandIndex(clusters$hc_train3, clusters$hc_val3)
ari_res["HC", "k=4"]  <- adjustedRandIndex(clusters$hc_train4, clusters$hc_val4)
ari_res["K", "k=3"]   <- adjustedRandIndex(clusters$km_train3, clusters$km_val3)
ari_res["K", "k=4"]   <- adjustedRandIndex(clusters$km_train4, clusters$km_val4)
ari_res["W+K", "k=3"] <- adjustedRandIndex(clusters$comb_train3, clusters$comb_val3)
ari_res["W+K", "k=4"] <- adjustedRandIndex(clusters$comb_train4, clusters$comb_val4)

ari_res
#for K=3 K-means and W+K give perfect stability with HC slightly less stable (0.737),still good
#for K=4 K-means has high stability with HC and W+K less stable (0.6)
#highest stability overall -> K-means with K=3

#Best clustering result K=3
# --- Final Clustering: Ward + K-means with k = 3 ---


hc_full <- hclust(dist(t(beer_mat))^2, method = "ward.D2")
cut_3 <- cutree(hc_full, k = 3)
cents_3 <- rowsum(t(beer_mat), cut_3) / as.vector(table(cut_3))

# Step 4: Run k-means with these Ward centroids as start points
final <- kmeans(t(beer_mat), centers = cents_3)

# Final cluster assignments
fin_clusters <- final$cluster
fin_clusters



library(ggplot2)
library(reshape2)

beer_cl <- data.frame(Beer = colnames(beer),Cluster = factor(fin_clusters))
beer_df <- melt(as.data.frame(beer),variable.name = "Beer",value.name = "Rank")
beer_df <- merge(beer_df, beer_cl, by = "Beer")


ggplot(beer_df, aes(x = Beer, y = Rank, fill = Cluster)) +
  geom_violin() +
  labs(
    title = "Final Clusters (k=3)",
    x = "Beer",
    y = "Rank - 1 = Best"
  ) +
  scale_fill_manual(values = c("lightblue", "blue", "darkblue")) +
  scale_y_continuous(limits = c(0, 13)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

pc_beers <- prcomp(t(beer), center = TRUE, scale. = FALSE)
pc_sc <- as.data.frame(pc_beers$x[, 1:2])
pc_sc$Cluster <- factor(fin_clusters)

ggplot(pc_sc, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "PCA of Beer Rankings") +
  scale_color_manual(values = c("#1f77b4", "#2ca02c", "#d62728")) +
  theme_minimal()


#ORCU
library(smacof)
delta_mat <- max(beer_mat) + 1 - beer_mat
unf <- unfolding(delta_mat, type = "ordinal", conditionality = "row", itmax = 3000, omega = 0.1)

attributes(unf)
person <- as.data.frame(unf$conf.row)  
beer_co   <- as.data.frame(unf$conf.col)  

dim(person)  #399 x 2
dim(beer_co) #12 x 2

colnames(person) <- c("Dimension_1", "Dimension_2")
colnames(beer_co) <- c("Dimension_1", "Dimension_2")

# Compute distance matrix
all_co <- rbind(person, beer_co)
dist_mat <- as.matrix(dist(all_co))
dist_mat <- dist_mat[1:nrow(person),(nrow(person)+1):(nrow(person)+nrow(beer_co))]
colnames(dist_mat) <- rownames(beer_co)

# k-means on person 
person_clusters <- kmeans(person[,1:2], centers = 3)$cluster
person$Cluster <- factor(person_clusters)
person$PrefBeer <- colnames(dist_mat)[apply(dist_mat, 1, which.min)]

#Unfolding helps place people and beer in SD space to make the distances represent preferences
#ordinal row unfolding uses the order of rankings with each choice relative to users preference and not others
#beer is colored with the cluster whose centroid closest to it


#centroids of the clusters
library(dplyr)
library(ggplot2)
library(ggrepel)

centroids <- person %>%
  group_by(Cluster) %>%
  summarise(
    Centroid_1 = mean(Dimension_1),
    Centroid_2 = mean(Dimension_2)
          )

beer_co$Cluster <- apply(beer_co[, c("Dimension_1", "Dimension_2")], 1, function(b)
  {
  d <- sqrt( (b[1] - centroids$Centroid_1)^2 +
               (b[2] - centroids$Centroid_2)^2 )
  which.min(d)
  })

beer_co$Cluster <- factor(beer_co$Cluster)

my_cols <- c("1"="darkblue", "2"="darkgreen", "3"="darkred")

ggplot() + 
  geom_point(data = person,aes(Dimension_1, Dimension_2, color = Cluster),alpha = 0.4,size=1) +
  geom_point(data = centroids,aes(Centroid_1, Centroid_2, color = Cluster),size = 5, shape = 17) +
  geom_text_repel(data = beer_co,aes(Dimension_1, Dimension_2, label = rownames(beer_co)),
                  size = 3, color = "black",box.padding = 0.4, point.padding = 0.4) +
  scale_color_manual(values = my_cols) + theme_bw() +
  labs(title = "ORCU Unfolding with People",x = "Dim 1", y = "Dim 2")

my_cols_2 <- c("1"="blue", "2"="green", "3"="red")

ggplot() +
  geom_point(data = beer_co,aes(Dimension_1, Dimension_2, color = Cluster),size = 3) +
  geom_text_repel(data = beer_co,aes(Dimension_1, Dimension_2, label = rownames(beer_co)),
                  size = 4,color = "black",box.padding = 0.5,point.padding = 0.5,max.overlaps = Inf) +
  scale_color_manual(values = my_cols_2) + theme_minimal() +
  labs(title = "Beer Positions in Space",x = "Dim 1", y = "Dim 2")

#dimensions are abstract but their relative positions reflect similarities in preference patterns
#Beers closer together are preferred similarly by respondents

#people closer to a beer show a stronger preference for that beer.

#Beers grouped in same cluster are liked by respondents who like one beer 
#Jupiler Pils and Stella Artois (red cluster area) are on the right-hand side → preferred by Cluster 3.
#Westvleteren 12 and Westmalle Dubbel (green cluster area) are on the left → preferred by Cluster 2.
#Leffe Blond and Grimbergen Blond (bottom) are closer to Cluster 1 → preferred by Cluster 1 respondents.

#Cluster blue--> lighter or Belgian blond beers (Leffe Blond, Grimbergen Blond, Maes Pils).
#Cluster green--> strong/traditional Belgian abbey beers (Westvleteren 12, Westmalle Dubbel, Chimay Blauw).
#Cluster red--> pils/lagers (Jupiler Pils, Stella Artois, Duvel).

#Cluster 1 → bottom-left  
#(-0.1, -0.25) – Leffe / Grimbergen / Westmalle
#traditional abbey-style, dark or heavy beers

#Cluster 2 → right  (+ Dim1)
#(0.39, -0.07) – Stella Artois / Jupiler Pils / Maes Pils
#mass-market Belgian pilsners which are light lagers.
#strong separation from Cluster 1 

#Cluster 3 → upper-left  (- Dim1, + Dim2)
#(-0.36, +0.10) - La Chouffe / Chimay Blauw / Westvleteren 12 / Duvel
#strong, premium, craft, Trappist and high-alcohol specialty beers.
#very different from Cluster 2 and somewhat different from Cluster 1 
