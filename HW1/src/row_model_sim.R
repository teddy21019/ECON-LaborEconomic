library(MASS)
library(data.table)
library(dplyr)
library(xtable)
setwd("~/111-1/Labor Economic/HW1/src")

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

    ## Creating error terms, saving to data.table 
wage = data.table(
  mvrnorm(n=N,mu=c(0, 0),Sigma=sigma_matrix)
)

  ## Rename
setnames(wage, "V1", "e0")
setnames(wage, "V2", "e1")

    ## Creating variables
wage[, W0 := e0 + mu0]
wage[, W1 := e1 + mu1]
wage[, Nu := e1 - e0]           
wage[, I  := W1 > W0 + C]

    ## Calculating the empirical conditional mean
E_w0_I = wage[I==T, mean(W0)]
E_w1_I = wage[I==T, mean(W1)]

    ## Calculate the theoretical value

## Calculate rho_nu 
rho = sigma_01/(sigma0 * sigma1)
sigma_nu = sqrt( sigma0^2 + sigma1^2 - 2 * sigma_01 )

## calculate z
z = (mu0 - mu1 + C)/sigma_nu
IMR = dnorm(z)/(1-pnorm(z))

## According to formula
calc_Q0 = mu0 + (sigma0 * sigma1)/sigma_nu * (rho - sigma0/sigma1) * IMR
calc_Q1 = mu1 + (sigma0 * sigma1)/sigma_nu * (sigma1/sigma0 - rho) * IMR

## Compare
result = data.frame("Source" = c(E_w0_I, calc_Q0), "Host" = c(E_w1_I, calc_Q1))
rownames(result) = c("Simulation", "Theoretical")

print(
  xtable(result, 
         caption = "Simulation result versus the theoretical result",
         label = "tab:sim_res",
         digits=5),
       floating = TRUE, latex.environments = "center",
  file="roy_sim.tex")
