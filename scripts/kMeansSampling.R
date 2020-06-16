#### Script to create a 'representative' sample 

## K-means clustering of data points, for training/testing/validation sets
kMeansTrainSet <- function(d, seed = NULL, k = 5,
                           trainProp = 0.8){
  ## Provide a data frame, a number of clusters,
  ## and a training proportion.
  ## Returns a set of indices that are samples proportionally from
  ## the clustered groupings.
  ## library(plyr) # loaded from masterLibrary.R
  # d <- data.frame("X"=rnorm(100),"Y"=rnorm(100),"Z"=rnorm(100))
  # seed <- 1111; k = 5; trainProp = 0.8
  if(!is.null(seed) & is.numeric(seed)) set.seed(seed)
  kClusters <- stats::kmeans(d, centers = k)
  df <- data.frame("Index" = 1:nrow(d),
                   "Cluster" = kClusters$cluster)
  sizeClust <- as.vector(table(df$Cluster))
  nSampsClust <- ceiling(trainProp*sizeClust)
  trainList <- dlply(df, .(Cluster), function(x){
    # x <- subset(df, Cluster == 1)
    sampInd <- unique(x$Cluster)
    sample(x$Index, nSampsClust[sampInd], replace = FALSE)
  })
  trainInds <- unlist(trainList) # Returns indices from the data frame
  return(trainInds)
}

## Testing
# d <- data.frame("X"=rnorm(100),"Y"=rnorm(100),"Z"=rnorm(100))
# trainSetInds <- kMeansTrainSet(d, seed = 1111, k = 5, trainProp = 0.8)
# str(trainSetInds)
