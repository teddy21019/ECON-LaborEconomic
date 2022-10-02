library(dplyr)

sigma_ep = 2
sigma_xi = 4
v0 = 10
rho = 0.8
N=1e5

yt = numeric(N)
vt = numeric(N)

for(i in 1:N){
  if (i==1){
    vt[i] = v0 * rho + rnorm(1, 0, sigma_xi)
  }else{
    vt[i] = vt[i-1] * rho + rnorm(1, 0, sigma_xi)
  }
  
  yt[i] = vt[i] + rnorm(1, 0, sigma_ep)
}

yt = data.frame(yt) %>% mutate(y_t1 = lag(yt, 1))

yt = yt[-1,]
View(yt)


model_1 = lm(yt~0 + y_t1, data = yt)

summary(model_1)

Ey = mean(yt$yt)
Ey1 = mean(yt$y_t1)
print(Ey/Ey1)


