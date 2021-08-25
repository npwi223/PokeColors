library(colordistance)
library(ggplot2)
library(stringr)

# Set your working directory to where the png images are located (remove comment below this)
# setwd("User/file/path/here")
# Identify the number of the pokemon based on the filename of the png

poke_colors <- function(NPD_Number, ncolors) {

NPD_Number <- ifelse(NPD_Number %>% str_length() == 1, paste0(0, 0, NPD_Number), NPD_Number)
NPD_Number <- ifelse(NPD_Number %>% str_length() == 2, paste0(0, NPD_Number), NPD_Number)

# Find K-means clusters
kmeans.clusters <- colordistance::getKMeanColors(paste0(NPD_Number,".png"),
                                                 n = ncolors,     
                                                 plotting = FALSE)
color_clusters <- colordistance::extractClusters(kmeans.clusters)

hex <- function(color_clusters){
  dec2hex <-
    function(d){
      as.character(as.hexmode(d))}
  number <- nrow(color_clusters)
  r <- sapply(color_clusters[,1], function(x) dec2hex(round(x * 255)))
  g <- sapply(color_clusters[,2], function(x) dec2hex(round(x * 255)))
  b <- sapply(color_clusters[,3], function(x) dec2hex(round(x * 255)))
  r <- ifelse(r %>% str_length() != 2, paste0(0, r), r)
  g <- ifelse(g %>% str_length() != 2, paste0(0, g), g)
  b <- ifelse(b %>% str_length() != 2, paste0(0, b), b)
  return(paste0("#", toupper(r), toupper(g), toupper(b)))
} 
return(hex(color_clusters))
}

# Example plot of Iris with Mewtwo colors
ggplot(iris, aes(x = Petal.Length, 
                 y = Petal.Width, 
                 size = Petal.Length*Petal.Width, 
                 color = Species)) +
  geom_point() +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank()) +
  scale_color_manual(values = poke_colors("150", 3)) # Choose the number of the pokemon you want your color palette based on (in quotes)
                                                     # Choose the number of colors you need (not in quotes)
                                                     # Your color palette is updated in scale_color_manual
