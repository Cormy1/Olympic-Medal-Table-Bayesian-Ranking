jags_model <- function(model){
  if (model == "beta"){
    
return("model {
  for (i in 1:n) {
  
    # Likelihood models for different types of medal winner 
    M1[i] ~ dpois(N[i] * p1[i])
    M2[i] ~ dpois(N[i] * p2[i])
    M3[i] ~ dpois(N[i] * p3[i])
    M4[i] ~ dpois(N[i] * p4[i])
    
    
    #Prior for probability of being a medal winner
    p[i] ~ dbeta(a, b) T(10^-9, 1)

    # Derived probabilities of individual medals 
    p1[i] = p[i]*(1-X1) #probabaility of exactly 1 medal = prob of a medal - prob of 2 or more
    p2[i] = p[i]*X1*(1-X2) #probabaility of exactly 2 medals = prob of 2 or more - prob of 3 or more
    p3[i] = p[i]*X1*X2*(1-X3)
    p4[i] = p[i]*X1*X2*X3

    
  }
  # Prior probability (uninformed) of tranistioning from m medals to m + 1 medals 
    X1 ~ dbeta(1, 1) 
    X2 ~ dbeta(1, 1)
    X3 ~ dbeta(1, 1)
  
  # Hyperpriors for beta prior for probability of being a medal winner
  a ~ dunif(0, 1)
  b ~ dunif(10^4, 10^8) 
}")
  }
  
 else if (model == "beta-2"){
    
    return("model {
  for (i in 1:n) {
  
    # Likelihood models for different types of medal winner 
    M1[i] ~ dpois(N[i] * p1[i])
    M2[i] ~ dpois(N[i] * p2[i])
    M3[i] ~ dpois(N[i] * p3[i])
    M4[i] ~ dpois(N[i] * p4[i])
    
    
    #Prior for probability of being a medal winner
    p[i] ~ dbeta(a, b) T(10^-9, 1)
    
   # Prior probability of tranistioning from m medals to m + 1 medals 
    X1[i] ~ dbeta(a1, b1) # now country-specific
    X2[i] ~ dbeta(a2, b2) #now country-specific
   

    # Derived probabilities of individual medals 
    p1[i] <- p[i]*(1-X1[i]) #probabaility of exactly 1 medal = prob of a medal - prob of 2 or more
    p2[i] <- p[i]*X1[i]*(1-X2[i]) #probabaility of exactly 2 medals = prob of 2 or more - prob of 3 or more
    p3[i] <- p[i]*X1[i]*X2[i]*(1-X3)
    p4[i] <- p[i]*X1[i]*X2[i]*X3

    
  }
 X3 ~ dbeta(1, 1)
  
  # Hyperpriors for beta prior for probability of being a medal winner
  a ~ dunif(0, 1)
  b ~ dunif(10^4, 10^8) 
  
  a1 ~ dunif(0,1)
  b1 ~ dunif(10^3, 10^9)
  
  a2 ~ dunif(0,1)
  b2 ~ dunif(10^3, 10^9)
}")
  }
  
  else if(model == "logit-normal"){
    return("model {
  for (i in 1:n) {

    # Likelihood models for different types of medal winners
    M1[i] ~ dpois(N[i] * p1[i])
    M2[i] ~ dpois(N[i] * p2[i])
    M3[i] ~ dpois(N[i] * p3[i])
    M4[i] ~ dpois(N[i] * p4[i])

    # Logit-normal prior on transformed scale for the medal-winning probability:
    theta[i] ~ dnorm(mu, tau)
    p[i] <- 1 / (1 + exp(-theta[i]))

    # Derived probabilities for medal counts
    p1[i] <- p[i] * (1 - X1)
    p2[i] <- p[i] * X1 * (1 - X2)
    p3[i] <- p[i] * X1 * X2 * (1 - X3)
    p4[i] <- p[i] * X1 * X2 * X3
  }

  # Prior for transition probabilities between medal counts
  X1 ~ dbeta(1, 1)
  X2 ~ dbeta(1, 1)
  X3 ~ dbeta(1, 1)

  # Hyperpriors for the logit-normal prior on p[i]
  mu ~ dnorm(-15, 0.3)      
  tau ~ dgamma(0.001, 0.001)   
}")
    
  }
  else if(model == "mixture-beta"){
    return(
      "model {
#defining mixture weights for each component
epsilon <- 1.0E-6
  w[1:3] ~ ddirch(alpha_w[]) 
 
# stick breaking for a[i] to try and fix identifiability issues
for (k in 1:3) {
    u[k] ~ dunif(0, 1)
}
  
  a[1] <- u[1]
  a[2] <- u[1] + (1 - u[1]) * u[2]
  a[3] <- u[1] + (1 - u[1]) * u[2] + (1 - (u[1] + (1 - u[1]) * u[2])) * u[3]

# hyperpriors for beta distribution indexed by component of mixture
for(k in 1:3) {
  log_b[k] ~ dnorm(log(1e6), 1) T(0, 20)
  b[k] <- exp(log_b[k]) #trying to make mcmc smpling more efficient and explore entire parameter space
}

  for (i in 1:n) {
  #decide which mixture component to use
  z[i] ~ dcat(w[])
  
    # Likelihood models for different types of medal winner 
    M1[i] ~ dpois(N[i] * p1[i])
    M2[i] ~ dpois(N[i] * p2[i])
    M3[i] ~ dpois(N[i] * p3[i])
    M4[i] ~ dpois(N[i] * p4[i])
    
    
    #Prior for probability of being a medal winner, adding epsilon to a instead of truncation
    p[i] ~ dbeta(epsilon + a[z[i]], b[z[i]]) 

    # Derived probabilities of individual medals 
    p1[i] = p[i]*(1-X1) #probabaility of exactly 1 medal = prob of a medal - prob of 2 or more
    p2[i] = p[i]*X1*(1-X2) #probabaility of exactly 2 medals = prob of 2 or more - prob of 3 or more
    p3[i] = p[i]*X1*X2*(1-X3)
    p4[i] = p[i]*X1*X2*X3

    
  }
  # Prior probability (uninformed) of tranistioning from m medals to m + 1 medals 
    X1 ~ dbeta(1, 1) 
    X2 ~ dbeta(1, 1)
    X3 ~ dbeta(1, 1)
  
  #hyperpriors for weights
   alpha_w[1] <- 1
   alpha_w[2] <- 1
   alpha_w[3] <- 1

}"
    )
  }
else{
  stop("Model type not recognized. Choose `beta`, `logit-normal` or `mixture-beta`.")
}
}