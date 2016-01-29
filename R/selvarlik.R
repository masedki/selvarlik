selvarlik <- function(x, nbcluster)
{
order <- matrix(NA, length(nbcluster), ncol(x))  
cp <- 1
for(g in 1:length(nbcluster))  
  order[g,] <- orderlik(x, nbcluster[g])

print(order)
 mymodels <- list()
 for(g in 1:length(nbcluster))
   mymodels[[g]] <- varsel(order[g,], x, nbcluster[g])


 return(mymodels)
}