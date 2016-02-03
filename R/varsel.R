varsel <- function(g, matrixorder, x, G, hs=50)
{
  tmporder <- matrixorder[g,]
  s <- tmporder[1]
  bicclust <- log(0)
  bicclustj <- log(0)
  tmp <- mixmodCluster(x[,s], nbCluster= G[g], models = mixmodGaussianModel(family = "diagonal"))
  if(!tmp@error)
    bicclust <- -tmp@bestResult@criterionValue

  tmp <- mixmodCluster(data.frame(x[,append(s,tmporder[2])]),  nbCluster= G[g], models = mixmodGaussianModel(family = "diagonal"))
  if(!tmp@error)
    bicclustj<- -tmp@bestResult@criterionValue

  bicregj <- -bic_j(tmporder[2], x, s)
  j <- 2
  cp <- 0
  while((j < ncol(x)) && (bicclustj - bicclust - bicregj > 0) || (cp <= hs))
  {
    cp <- cp + 1
    if(bicclustj - bicclust - bicregj > 0)
    {
      cp <- 0
      s <- append(s,tmporder[j])
    }
    j <- j+1
    bicclust <- log(0)
    bicclustj <- log(0)
    tmp <- mixmodCluster(data.frame(x[,s]), nbCluster= G[g], models = mixmodGaussianModel(family = "diagonal"))
    if(class(tmp)!="try-error")
      bicclust<- -tmp@bestResult@criterionValue
    tmp <- mixmodCluster(data.frame(x[,append(s,tmporder[j])]),  nbCluster= G[g], models = mixmodGaussianModel(family = "diagonal"))
    if(!tmp@error)
      bicclustj<- -tmp@bestResult@criterionValue
    bicregj <-  -bic_j(tmporder[j], x, s)

  }


  u <- setdiff(tmporder, s)
  return(list(s=s, u=u))
}






