selvarlik <- function(x, nbcluster)
{
tmporder <- matrix(NA, length(nbcluster), ncol(x))
for(g in 1:length(nbcluster))
  tmporder[g,] <- orderlik(x, nbcluster[g])

 #print(order)
 mymodels <- list()
 for(g in 1:length(nbcluster))
   mymodels[[g]] <- varsel(tmporder[g,], x, nbcluster[g])


 return(mymodels)
}
