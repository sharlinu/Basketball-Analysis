library(data.table)
library(BasketballAnalyzeR)
library(stringr)
library(ggplot2)
#devtools::install_github("AckerDWM/gg3D")
library("gg3D")
library(plotly)

setwd("/Users/Mac/Documents/Bocconi/SA")
# MDS Plotting

MDS <- function(data, k=2, std=TRUE ) {
  if (!is.matrix(data) & !is.data.frame(data) & (!inherits(data, "dist"))) {
    stop("'data' must be a matrix, a data frame, or a distance matrix")
  }
  
  if (!inherits(data, "dist")) {
    if (std) {
      data_for_dist <- scale(data)
    } else {
      data_for_dist <- data
    }
    dist.mat <- dist(data_for_dist)
  } else {
    dist.mat <- data
  }
  
  out <- MASS::isoMDS(dist.mat, k, y=cmdscale(dist.mat, k), maxit=100, trace=FALSE)
  out[["data"]] <- data
  out[["dist"]] <- dist.mat
  out[["std"]] <- std
  class(out) <- append("MDSmap", class(out))
  return(out)
}


df = fread("Pbox_24042020.csv")
mMIN = 500 
id = df$Cluster[df$MIN>=mMIN]
data = subset(df, df$MIN>=mMIN  , select =c("FT%", "2P%", "3P%", "difficulty",  "OR", "DR", "USG", "AST%", "FGM%A", "TPP", "STLm", "BLKm"))


# Measuring the stress index
for (i in seq(5)){
  print(paste("Stressindex for", i, "dimensions is:", round(MDS(data, i)[["stress"]], digits=2)))
}
w.var = as.factor(df$Cluster)

# Plotting
mMIN = 500 
ID = id
mds = MDS(df_mds, k=3)
points = data.frame(mds$points)
points= cbind(points, df_mds)
points= cbind(points, ID)