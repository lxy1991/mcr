"singular_value" <- function(data,maxrows=10) {
  # Compute singular value decomposition for the
  # determination of components.
  av <- svd(data)$d
  pav <- cumsum(av)/sum(av)*100
  ava <- av[2:length(av)] / av[1:length(av)-1]
  ava <- c(NA,ava)
  logav <- log10(av)
  ret = cbind(av,logav,pav,ava)
  plot(av)
  print("AutoValues Matrix:")
  print(head(ret,maxrows))
  return(ret)
}