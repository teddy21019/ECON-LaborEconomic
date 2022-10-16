library(tidyverse)
library(plm)
library(did)
mu = 2 # the real treatment effect

nobs = 100
year_range =  2000:2022
nstates = 40
years_treat = c(2005, 2010, 2015, 2020)


#############################
# Data generating process
#############################

DGP = function(){
  alpha_i = tibble(
    unit = 1:nobs,
    state = sample(1:nstates, nobs, replace = T),
    unit_fe = rnorm(nobs, state/5, 1)
  )
  
  alpha_t = tibble(
    year = year_range, 
    year_fe = rnorm(length(year), 0, 1)
  )
  
  # out of 40 states, there are some that belongs to some treatment group
  state_treatments = tibble(
    state = sample(1:nstates, nstates, replace = F),  # shuffle
    year_of_treatment = rep(years_treat, nstates/length(years_treat))
  )
  
  
  ## combine all together.
  ## first create a big table of n_obs * n_years rows
  expand.grid(unit = 1:nobs, year = year_range) %>% 
    left_join(., alpha_i, by="unit") %>% 
    left_join(., alpha_t, by="year") %>% 
    left_join(., state_treatments, by="state") %>% 
    mutate(
      treat = ifelse(year >= year_of_treatment, 1,0),
      tau = ifelse(treat==1, mu, 0)*(year - year_of_treatment +1),
      error = rnorm(n(),0,1),
      year_fe = year_fe + 0.1 * (year - year_of_treatment),
      y_value = (2022 - year_of_treatment) + unit_fe + year_fe + tau + error
    )
}


#############################
# Visualizing the DGE
#############################

df = DGP()
plot1 <- df %>% 
  ggplot(aes(x = year, y = y_value, group = unit)) + 
  geom_line(alpha = 1/8, color = "grey") + 
  geom_line(data = df %>% 
              group_by(year_of_treatment, year) %>% 
              summarize(y_value = mean(y_value)),
            aes(x = year, y = y_value, group = factor(year_of_treatment),
                color = factor(year_of_treatment)),
            size = 2) + 
  labs(x = "Year", y = "Value", color = "Treatment group   ") +
  theme(legend.position = 'bottom')

plot1
#ggsave(filname="sim_twfe.eps",  plot1)



#############################
# Monte Carlo Simulation
#############################


N_sim = 1000
coef_results = matrix(0, nrow = N_sim, ncol = 1)

for(s in 1:N_sim){
  df = DGP()
  twfe = plm(y_value~treat, data=df, model = 'within',effect='twoways')
  coef_results[s,1] = coef(twfe)["treat"]
}
monte_sim = data.frame(coef = coef_results) %>% ggplot() +
  geom_histogram(aes(x = coef)) 
t.test(coef_results, mu=mu)

#ggsave("monte_sim.eps", monte_sim)


#############################
# Using the did package
#############################

df = DGP() %>% select(unit, year, year_of_treatment, y_value)

CS.ATT = att_gt(
  yname = "y_value",
  tname = "year",
  idname = "unit",
  gname = "year_of_treatment",
  control_group = "notyettreated",
  bstrap = FALSE,
  data = df)

event_std = aggte(CS.ATT, type = 'dynamic')

did_package_result = 
  data.frame(TE = event_std$att.egt, year = -14:14, 
             CI_UP = event_std$att.egt + event_std$se.egt,
             CI_LO = event_std$att.egt - event_std$se.egt) %>% 
  ggplot() +
  geom_point(aes(x = year, y=TE), size=2) +
  geom_ribbon(aes(x = year, ymin = CI_LO, ymax = CI_UP), alpha=0.2, color = "lightgrey")+
  xlab("Relative year")+ylab("ATT(t,g) Estimated")
did_package_result

#ggsave("did_package_result.eps", did_package_result)
