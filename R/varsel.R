varsel <- function(order, x, nbcluster)
{

j <- 1 ; S <- c(order[j])
j <- j + 1
bicclust <- Mclust(x[,order[j]],  G=nbcluster)$bic
bicclustj <- Mclust(x[,c(S,order[j])],  G=nbcluster)$bic
bicregj <- bic_j(order[j], x, S)
bicregj <- sum(unlist(mclapply(X=as.list(setdiff(1:ncol(x), S)),
                               FUN = bic_j,
                               x=x,
                               both = TRUE,
                               discrim=discrim,
                               mc.cores = min(length(setdiff(1:ncol(x), S)), detectCores(all.tests=FALSE, logical=FALSE)),
                               mc.preschedule = TRUE,
                               mc.cleanup = TRUE)))

while(bicclustj - (bicclust + bicregj))
{
  S <- c(S,order[j])
  j <- j+1
  bicclust <- Mclust(x[,order[1]],  G=nbcluster)$bic
  bicclustj <- Mclust(x[,order[1:j]],  G=nbcluster)$bic
  bicregj <-  bic_j(order[j], x, S)


}
return(list(S=S, O=setdiff(order, S)))
}
