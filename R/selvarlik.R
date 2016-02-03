selvarlik <- function(x, G, hs=50)
{
matrixorder <- matrix(NA, length(G), ncol(x))
# la fonction orderlik est en parallel donc
for(g in 1:length(nbcluster))
  matrixorder[g,] <- orderlik(x, G[g])



 mymodels <- mclapply(X=as.list(1:length(G)),
                     FUN = varsel,
                     x=x,
                     matrixorder=matrixorder,
                     G=G,
                     hs=hs,
                     mc.cores = min(length(G), detectCores(all.tests=FALSE, logical=FALSE)),
                     mc.preschedule = TRUE,
                     mc.cleanup = TRUE)




for(g in 1:length(G))
{
 mymodels[[g]] <- selmodel(g, x, G,mymodels)
}

 return(mymodels)
}
