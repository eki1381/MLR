multinomial <- function(y,x){
  library(Matrix)
  
  y.design.1 <- model.matrix(~-1 + .,data = y)
  x.design.1 <- model.matrix(~.,data = x)
  N <- nrow(y.design.1)
  K <- ncol(x.design.1) - 1
  J <- ncol(y.design.1)
  y.design.2 <- y.design.1[,1:(J-1)]
  list <- rep(list(x.design.1),J-1)
  x.design.2 <- as.matrix(bdiag(list))
  
  beta.1.temp <- matrix(0,K+1,J-1)
  beta.1 <- as.vector(beta.1.temp)
  beta.2 <- rep(-Inf,length(beta.1))
  diff.beta <- sqrt(sum((beta.1-beta.2)^2))
  
  return(diff.beta)
}
