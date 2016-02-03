orderlik<- function(x, nbcluster, nbcores=detectCores(all.tests = FALSE, logical = FALSE))
{

  loglikwrapper <-function(j,x,nbcluster){
    # pour traiter les warnings comme des erreurs.
    #options(warn=2)
    #mclust.options(warn=TRUE)
    tmp <- mixmodCluster(x[,j], nbCluster = nbcluster)
    critval <- log(0)
    if(!tmp@error)
       critval <- tmp@bestResult@likelihood
       #critval <- -tmp@bestResult@criterionValue
     return(critval)
  }

  junk <-  mclapply(X=as.list(1:ncol(x)),
                    FUN = loglikwrapper,
                    x=x,
                    nbcluster=nbcluster,
                    mc.preschedule = TRUE,
                    mc.silent = TRUE,
                    mc.cleanup = TRUE,
                    mc.cores = min(nbcores, ncol(x)))

  return(order(simplify2array(junk), decreasing = TRUE))
}
