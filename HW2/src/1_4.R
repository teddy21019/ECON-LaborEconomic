s1 = 2
s2 = 5

# DGE
ys = numeric(2)
for(i in 1:2){
  ys[i] = rnorm(1, 0, s1) + rnorm(1, 0, s2)
}

lh = function(sigma){
  orig = 0
  sigma_1 = sigma[1]
  sigma_2 = sigma[2]
  
  for(y in ys){
    orig = orig - log(dnorm(y, mean = 0, sd = sqrt(sigma_1^2 + sigma_2^2)))
  }
  return(orig)
}

op = optim(c(1,2), fn = lh, method = "BFGS")

op
