library(MASS)
library(data.table)
library(dplyr)
library(xtable)
library(Matrix)
library(stargazer)
setwd("C:/Users/tedb0/Documents/111-1/Labor Economic/HW2/src")

    ## Parameters settings
N = 1e7
mu0 = 10
mu1 = 15
sigma0 = 3
sigma1 = 4.5
sigma_01 = 2
sigma_matrix = matrix(c(sigma0^2, sigma_01, 
                        sigma_01, sigma1^2), 
                      ncol=2)
C = 3

## ===== New Parameter

beta1 = 0.5
beta2 = -1
mux1 = 3
mux2 = 6

sigma_matrix = bdiag(sigma_matrix, diag(1,2,2)) 

    ## Creating error terms, saving to data.table 
wage = data.table(
  mvrnorm(n=N,mu=c(0, 0, mux1, mux2 ), Sigma=sigma_matrix)
)

  ## Rename
setnames(wage, "V1", "e0")
setnames(wage, "V2", "e1")
setnames(wage, "V3", "x1")
setnames(wage, "V4", "x2")

    ## Creating variables
wage[, W0 := e0 + x1* beta1 + mu0]
wage[, W1 := e1 + x1* beta1 + x2* beta2 + mu1]
wage[, Nu := e1 - e0]           
wage[, D  := W1 > W0 + C]
wage[, W  := W1 * D + W0 * (1-D)]
    ## Calculate the theoretical value

## Calculate rho_nu 
sigma_nu = sqrt( sigma0^2 + sigma1^2 - 2 * sigma_01 )

wage[, p_score_theor := dnorm((mu1 - mu0 + beta2 * x2 - C)/sigma_nu) ]

## Estimate logit to get propensity score

wage[, p_score_est := predict(
  glm(D~x2, family = binomial(link = "logit"))
  , type = "response"),]

cor_p_score = wage[,cor(p_score_theor, p_score_est),]

wage[, IPW_theor := W/ifelse(D, p_score_theor, -1 + p_score_theor)]
wage[, IPW_est :=  W/ifelse(D, p_score_est, -1 + p_score_est)]

ATE_theor = mu1 - mu0 + beta2 * mux2
ATE_prop_theor = wage[, mean(IPW_theor)]
ATE_prop_est = wage[, mean(IPW_est)]


model_ols_1 = wage[,lm(W~D)]
stargazer(model_ols_1,model_ols_2, out = "OLS_D.tex", table.placement = "h", 
          keep.stat = c("rsq"), label = "tab:reg")


