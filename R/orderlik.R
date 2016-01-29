orderlik<- function(x, nbcluster, nbcores=detectCores(all.tests = FALSE, logical = FALSE))
{
  
  loglikwrapper <-function(j,x,nbcluster){return(Mclust(x[,j], G=nbcluster)$loglik)}
  return(order(simplify2array(mclapply(X=as.list(1:ncol(x)), 
                                FUN = loglikwrapper, 
                                x=x,
                                nbcluster=nbcluster,
                                mc.preschedule = TRUE, 
                                mc.silent = TRUE, 
                                mc.cleanup = TRUE, 
                                mc.cores = min(nbcores, ncol(x)))), decreasing = TRUE))
}