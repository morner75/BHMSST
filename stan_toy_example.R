packages <- c("tidyverse","lubridate","evd","rstan")
sapply(packages,require,character.only=TRUE)

rm(list=ls())

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

dat <- readRDS("data/SST_toy_data.rds")

N <- dat %>% nrow()
n_point <- dat %>% pull(point) %>% unique() %>% length()
point_id <- dat %>% pull(point_no)
y <- dat %>% pull(MST)


SST_data <- list(N=N,n_point=n_point, point_id=point_id,y=y)

fit1 <- stan(
  file = "stan/SST_simple.stan",  # Stan program
  data = SST_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 4              # number of cores (could use one per chain)
)

fit1
plot(fit1,pars=c("mu", "sigma_y", "sigma_gamma"))
traceplot(fit1,pars=c("mu", "sigma_y", "sigma_gamma"))
res <- extract(fit1,pars="y_rep")

as.data.frame(res$y_rep)
