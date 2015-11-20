source("data_init.R")

ems_lw <- ems_diet %>% distinct(fid) %>% 
  select(fid,tl,w_wt) %>% 
  mutate(logl=log(tl),
         logw=log(w_wt))

fit1 <- lm(logw~logl,data=ems_lw)
predict(fit1,data.frame(logl=log(62)),interval="confidence")
coef(fit1)
