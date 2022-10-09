packages <- c("tidyverse","lubridate","evd","rstan")
sapply(packages,require,character.only=TRUE)

rm(list=ls())

rstan_options(auto_write = TRUE)
options(mc.cores = 4)

dat <- readRDS("data/SST_toy_data.rds") %>% 
  mutate(obs_no=row_number())


N <- dat %>% nrow()
n_point <- dat %>% pull(point) %>% unique() %>% length()
point_id <- dat %>% pull(point_no)
y <- dat %>% pull(MST)
X <- dat %>% select(AST,SD) %>% mutate(const=1,.before=1) %>% as.matrix()


SST_data <- list(N=N,n_point=n_point, point_id=point_id,y=y,n_indvar=ncol(X), X=X)

fit1 <- stan(
  file = "stan/SST_simple1.stan",  # Stan program
  data = SST_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 4              # number of cores (could use one per chain)
)

fit1
plot(fit1,pars=c("sigma_y", "sigma_beta1"))
traceplot(fit1,pars=c("sigma_y", "sigma_beta1"))
res <- extract(fit1,pars="y_rep")
y_rep <- as.data.frame(res$y_rep)

post_sample <- 
  y_rep %>% 
  pivot_longer(everything(),
               names_to="obs_no",
               names_prefix = "V",
               names_transform = as.integer,
               values_to="post_sample")  %>% 
  left_join(dat, by="obs_no")
  
temp1 <- post_sample %>% filter(year %in%c(2000,2020) & point %in% c("22101","22102","22103","22104"))  

temp1 %>% pull(year) %>% unique()
ggplot(temp1,aes(post_sample,group=point))+
  geom_density(aes(col=year))+
  facet_grid(year~point)
