library(paran)
library(car)
library(ggplot2)
library(ggrepel)
library(tidyverse)

# loading the provided data set
load("Data/life.Rdata")
sum(is.na(life)) # checking for missing values

# exploring the data set
life_long <- life %>% pivot_longer(cols = everything(),
                                   names_to = "variable",
                                   values_to = "scores")

# box plot to see the distribution of the data
ggplot(life_long, aes(x = variable, y = scores, fill = variable)) + 
  geom_boxplot() + 
  theme_classic() + 
  labs(x = "", y = "", 
       title = "Items Scores on Likert Scale (1 to 4)") + 
  theme(legend.position = "none") + 
  geom_abline(intercept = 4, slope = 0, lty = 2) +
  geom_abline(intercept = 1, slope = 0, lty = 2) + 
  scale_y_continuous(breaks = 1:4, limits = c(1, 4))

summary(life)

## part a

# standardize data
life_standard <- scale(life, center = TRUE, scale = TRUE)

# conducting PCA
life_pca <- prcomp(life_standard)
print(life_pca)

# calculating matrix of component scores
standard_scores <- life_standard %*% life_pca$rotation %*% diag(1 / life_pca$sdev)
round(standard_scores, 3)

# computing eigenvalues and explained variance
summary(life_pca)

# scree plot to determine number of components
screeplot(life_pca, type = "lines", main = "Scree plot", 
          xlim = c(0.75, 6.25), ylim = c(0, 3))

# Horn's Procedure
# comparing against the mean
set.seed(17)
paran(
  life_standard,
  iterations = 5000,
  graph = TRUE,
  cfa = FALSE,
  centile = 0
)

# comparing against the 95th percentile
set.seed(21)
paran(
  life_standard,
  iterations = 5000,
  graph = TRUE,
  cfa = FALSE,
  centile = 95
)

# NOTE: both methods suggest using the first two principal components

# computing components loading
loadings <- life_pca$rotation %*% diag(life_pca$sdev)
round(loadings, 3)

## part b

# flipping the loadings of the second component for better interpretation
life_pca$rotation[, 2] <- life_pca$rotation[, 2] * -1
life_pca$x[, 2] <- life_pca$x[, 2] * -1


# extracting the factor loadings
# rotation vectors are multiplied by two only for visualization purposes
loadings <- tibble(
  variable = colnames(life),
  load1 = life_pca$rotation[, 1] * 2,
  load2 = life_pca$rotation[, 2] * 2
)

# extracting factor scores
scores <- tibble(
  country = rownames(life),
  scores1 = life_pca$x[, 1] / life_pca$sdev[1],
  scores2 = life_pca$x[, 2] / life_pca$sdev[2]
)

# the bi-plot
theme_biplot <- theme_classic() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    text = element_text(family = "serif", size = 12),
    plot.title = element_text(face = "bold", size = 14),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    axis.ticks.x.top = element_line(colour = "red"),
    axis.text.x.top = element_text(colour = "red"),
    axis.ticks.y.right = element_line(colour = "red"),
    axis.text.y.right = element_text(colour = "red")
  )

ggplot() +
  scale_x_continuous(
    limits = c(-2.5, 2.5),
    name = "PC1 (39.99%)",
    sec.axis = dup_axis( ~ . / 2, name = NULL)
  ) +
  scale_y_continuous(
    limits = c(-2.5, 2.5),
    name = "PC2 (26.48%)",
    sec.axis = dup_axis( ~ . / 2, name = NULL)
  ) +
  geom_point(
    data = scores,
    aes(x = scores1, y = scores2),
    size = 1.5
  ) + 
  geom_text_repel(
    data = scores,
    aes(x = scores1, y = scores2, label = country),
    color = "black",
    size = 2
  ) +
  geom_segment(
    data = loadings,
    aes(
      x = 0,
      y = 0,
      xend = load1,
      yend = load2
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    color = "red",
    linewidth = 0.5
  ) +
  geom_text_repel(
    data = loadings,
    aes(
      x = load1,
      y = load2,
      label = variable
    ),
    color = "red",
    size = 2.5
  ) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.3
  ) +
  geom_vline(
    xintercept = 0,
    color = "black",
    linewidth = 0.5,
    linetype = "dashed",
    alpha = 0.3
  ) +
  ggtitle("Standardized scores on the first two principal components (66.48%)") +
  theme_biplot

  