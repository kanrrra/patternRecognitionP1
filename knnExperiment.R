result <- function(features.training, features.testing){
  library(class)
  
  #learn the multinom model
  features.knn.pred <- knn(train = features.training[,-1], test = features.testing[,-1], cl = features.training[,1], k = 5)
  
  #show the performance
  features.knn.conftable <- table(features.testing[,1], features.knn.pred)
  precision <- sum(diag(features.knn.conftable))/nrow(features.testing)
  
  print(features.knn.conftable)
  print(precision)
  
  result <- list(model = NULL, pred = features.knn.pred, conftable = features.knn.conftable, precision = precision)
  return(result)
}