varsel <- function(tmporder, x, nbcluster)
{
  S <- tmporder[1]
  bicclust <- log(0)
  bicclustj <- log(0)
  tmp <- mixmodCluster(x[,S], nbCluster= nbcluster, models = mixmodGaussianModel(family = "diagonal"))
  if(!tmp@error)
    bicclust <- -tmp@bestResult@criterionValue

  tmp <- mixmodCluster(data.frame(x[,append(S,tmporder[2])]),  nbCluster= nbcluster, models = mixmodGaussianModel(family = "diagonal"))
  if(!tmp@error)
    bicclustj<- -tmp@bestResult@criterionValue

  bicregj <- -bic_j(tmporder[2], x, S)
  j <- 2
  cp <- 0
  while((j < ncol(x)) && (bicclustj - bicclust - bicregj > 0) || (cp <= 50))
  {
    cp <- cp + 1
    if(bicclustj - bicclust - bicregj > 0)
      cp <- 0
    S <- append(S,tmporder[j])
    j <- j+1
    bicclust <- log(0)
    bicclustj <- log(0)
    tmp <- mixmodCluster(data.frame(x[,S]), nbCluster= nbcluster, models = mixmodGaussianModel(family = "diagonal"))
    if(class(tmp)!="try-error")
      bicclust<- -tmp@bestResult@criterionValue
    tmp <- mixmodCluster(data.frame(x[,append(S,tmporder[j])]),  nbCluster= nbcluster, models = mixmodGaussianModel(family = "diagonal"))
    if(!tmp@error)
      bicclustj<- -tmp@bestResult@criterionValue

    bicregj <-  -bic_j(tmporder[j], x, S)

  }
  U <- setdiff(tmporder, S)
  clustpart  <- mixmodCluster(data.frame(x[,S]), nbCluster= nbcluster, models = mixmodGaussianModel(family = "diagonal"))
  bicreg  <- -sum(unlist(mclapply(X=as.list(setdiff(1:ncol(x), S)),
                                 FUN = bic_j,
                                 x=x,
                                 mc.cores = min(length(setdiff(1:ncol(x), S)), detectCores(all.tests=FALSE, logical=FALSE)),
                                 mc.preschedule = TRUE,
                                 mc.cleanup = TRUE)))

  return(list(S=S, U=U, clustpart= clustpart, bicreg=bicreg))
}
