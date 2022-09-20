library(haven)
library(tidyverse)

df <- read_dta("111-1/Labor Economic/HW1/src/SRDA/C00377_1/psfd_rr2020_v202208_stata.dta")

work_info = df %>% filter(w03 %in% c(1,2,3)) %>% 
  mutate( age = cut(111-a02a, breaks=seq(0,100,by=10))) %>%
  mutate( has_work = ifelse(w03 != 3, 1 , 0)) %>% 
  select(age,has_work, w03) %>%
  as.data.frame()


p = work_info %>% group_by(age) %>% summarise(working_rate = mean(has_work)) %>% 
  ggplot() +
  geom_bar(aes(x=age, y=working_rate),stat="identity") +
  ggtitle("Rate of work - Age")+
  ylab("Rate of Working")+
  xlab("Age")+
  coord_flip() 

setwd('C:\\Users\\tedb0\\Documents\\111-1\\Labor Economic\\HW1\\src\\SRDA\\Result')
ggsave(filename = "Work_age.eps", p)
