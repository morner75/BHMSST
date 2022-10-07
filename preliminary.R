packages <- c("tidyverse","lubridate","evd")
sapply(packages,require,character.only=TRUE)

rm(list=ls())

# 기상청 해양부이 자료
data_1 <- read_csv("data/OBS_BUOY_DD1.csv",col_types = "cDddddddddddd")
data_2 <- read_csv("data/OBS_BUOY_DD2.csv",col_types = "cDddddddddddd")
data_3 <- read_csv("data/OBS_BUOY_DD3.csv",col_types = "cDddddddddddd")

data <- do.call(bind_rows, list(data_1,data_2,data_3))

# 일평균 수온의 연간 최고치
data1 <-
  data %>%
  mutate(year=year(일시)) %>% 
  group_by(지점,year) %>% 
  summarise(MST=max(`평균 수온(°C)`,na.rm=TRUE),.groups="drop") 


# boxplot
data11 <- data1 %>% 
  mutate(지점=as.factor(지점), year=NULL) %>% 
  drop_na() %>% 
ggplot(aes(지점,MST))+
  geom_boxplot()+
  theme_bw()


# 관측지점별 시계열 
data2 <- data1 %>% 
  pivot_wider(names_from=지점,values_from=MST) %>% 
  arrange(year)


# 관측치 10개이상의 데이터 요약
summary(data2 %>% select(where(~sum(!is.na(.x))>=10)))


# 지점별 Gumbel v.s. GEV fitting

# 지점 21101(덕적도)

# GEV
mod1_1 <- fgev(pull(data2,3))
# Gumbel
mod1_2 <- fgev(pull(data2,3),shape=0)

# Goodness of fitness
par(mfrow=c(2,2))
plot(mod1_1)
plot(mod1_2)
dev.off()
# anova test
anova(mod1_1,mod1_2)


# 지점 21102(칠발도)

# GEV
mod2_1 <- fgev(pull(data2,4))
# Gumbel
mod2_2 <- fgev(pull(data2,4),shape=0)

# Goodness of fitness
par(mfrow=c(2,4),mar=c(3,3,2,1))
plot(mod2_1)
plot(mod2_2)
dev.off()
# anova test
anova(mod2_1,mod2_2)


