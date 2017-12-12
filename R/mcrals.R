"mcrals" <- function(data,comp=0) {
  if (comp == 0) {
    singular_value(data)
    comp <- as.integer(readline(prompt="Number of components?"))
  }
  C <- efa(data,comp)
  invisible(readline(prompt="Press any key to start."))
  test0 <- als(CList=list(C),S=matrix(1,nrow=dim(data)[2],ncol=comp),
               PsiList=list(data),normS=1, nonnegC=T,nonnegS=T,
               uniC=T,uniS=T,maxiter=500,thresh=0.0001)
  matplot(test0$CList[[1]],type="l")
  invisible(readline(prompt="Optimized concentration profiles. Press any key to continue."))
  matplot(test0$S,type="l")
  invisible(print("Optimized spectra."))
  print("Lack of fit:")
  dataopt <- t(test0$S %*% t(test0$CList[[1]]))
  print(sqrt(sum((data-dataopt)^2)/sum(data^2)) * 100)
  return(test0)
}