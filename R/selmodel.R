selmodel <- function(g, x, G,mymodels)
{
s <-mymodels[[g]]$s
u <- mymodels[[g]]$u

clustpart  <- mixmodCluster(data.frame(x[,s]), nbCluster= G[g], models = mixmodGaussianModel(family = "diagonal"))
junk  <- mclapply(X=as.list(u),
                  FUN = bic_j,
                  x=x,
                  S=S,
                  mc.cores = min(length(u), detectCores(all.tests=FALSE, logical=FALSE)),
                  mc.preschedule = TRUE,
                  mc.cleanup = TRUE)
return(list(s=s,u=u, clustpart= clustpart, bicreg=-sum(simplify2array(junk)), bic = -clustpart@bestResult@criterionValue - sum(simplify2array(junk))))
}
