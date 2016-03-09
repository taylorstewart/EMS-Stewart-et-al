source("data_init.R")

## cal tl
ggplot(ems_cal,aes(tl)) +
  geom_histogram(binwidth=10) +
  scale_x_continuous(limits=c(40,100)) +
  theme_bw() +
  facet_wrap(month~basin)

## cal wt
ggplot(ems_cal,aes(wet_wt)) +
  geom_histogram(binwidth=1) +
  scale_x_continuous(limits=c(0,7)) +
  theme_bw() +
  facet_wrap(month~basin)

## diet tl
ggplot(filter(ems_diet,food_item!="Empty"),aes(tl)) +
  geom_histogram(binwidth=10) +
  scale_x_continuous(limits=c(40,100)) +
  theme_bw() +
  facet_wrap(month~basin)

## diet wt
ggplot(filter(ems_diet,food_item!="Empty"),aes(w_wt)) +
  geom_histogram(binwidth=1) +
  scale_x_continuous(limits=c(0,7)) +
  theme_bw() +
  facet_wrap(month~basin)
