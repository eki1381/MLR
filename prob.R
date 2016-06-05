prob <- function(x.design.1,beta.1.temp){
  nom <- matrix(NA,nrow(x.design.1),ncol(beta.1.temp)-1)
  for(i in 1:nrow(x.design.1)){
    for(j in 1:(ncol(beta.1.temp)-1)){
      nom.temp <- 0
      for(k in 1:ncol(x.design.1)){
        nom.temp <- nom.temp + (x.design.1[i,k]*beta.1.temp[k,j])
      }
      nom[i,j] <- exp(nom.temp)
    }
  }
  
  denom <- matrix(NA,nrow(x.design.1),1)
  for(i in 1:nrow(x.design.1)){
    for(j in 1:(ncol(beta.1.temp)-1)){
      for(k in 1:ncol(x.design.1)){
        
      }
    }
  }
}
