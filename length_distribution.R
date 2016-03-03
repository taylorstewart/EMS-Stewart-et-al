source("data_init.R")

ggplot(ems_cal,aes(tl)) +
  geom_histogram(binwidth=10) +
  scale_x_continuous(limits=c(40,100)) +
  theme_bw() +
  facet_wrap(month~basin)

ggplot(filter(ems_diet,food_item!="Empty"),aes(tl)) +
  geom_histogram(binwidth=10) +
  scale_x_continuous(limits=c(40,100)) +
  theme_bw() +
  facet_wrap(month~basin)
