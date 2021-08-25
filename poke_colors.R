library(colordistance)
library(ggplot2)
library(stringr)

# Find K-means clusters


kmeans.clusters <- colordistance::getKMeanColors("150.png"
, n = 3, plotting = FALSE)
color_clusters <- colordistance::extractClusters(kmeans.clusters)
clusters <- color_clusters
hex <- function(clusters){
  dec2hex <-
    function(d){
      as.character(as.hexmode(d))}
  number <- nrow(clusters)
  r <- sapply(clusters[,1], function(x) dec2hex(round(x * 255)))
  g <- sapply(clusters[,2], function(x) dec2hex(round(x * 255)))
  b <- sapply(clusters[,3], function(x) dec2hex(round(x * 255)))
  r <- ifelse(r %>% str_length() != c(2, 2, 2), paste0(0, r), r)
  g <- ifelse(g %>% str_length() != c(2, 2, 2), paste0(0, g), g)
  b <- ifelse(b %>% str_length() != c(2, 2, 2), paste0(0, b), b)
  return(paste0("#", toupper(r), toupper(g), toupper(b)))
} 
hex(color_clusters)

ggplot(iris, aes(x = Petal.Length, 
                 y = Petal.Width, 
                 size = Petal.Length*Petal.Width, 
                 color = Species)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank()) +
  scale_color_manual(values = hex(color_clusters))
