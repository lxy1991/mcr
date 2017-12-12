"efa" <- function(data, comp = 0) {
  # Compute evolving factor analysis of matrix data to estimate the
  # concentration profiles of comp components.
	n <- dim(data)[1]
	ef <- matrix(0,nrow=n,ncol=n)
	eb <- matrix(0,nrow=n,ncol=n)
	datab <- data[n:1,]
	for (i in 1:n) { 
		temp <- svd(data[1:i,])
		ef[1:i,i] <- temp$d
		temp <- svd(datab[1:i,])
		eb[1:i,i] <- temp$d
	}
	eb <- eb[,n:1]
	matplot(log(t(ef)),type="l")
	matlines(log(t(eb)),type="l")
	if (comp == 0) {
  	comp <- as.integer(readline(prompt="Enter number of components:"))
	} else {
	  invisible(readline(prompt="Press a key to continue."))
	}
	C <- matrix(0,nrow=n,ncol=comp)
	for (i in 1:comp) {
	  C[,i] <- pmin(ef[i,],eb[comp-i+1,])
	}
	matplot(C,type="l")
	return(C)
}