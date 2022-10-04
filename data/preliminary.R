packages <- c("tidyverse","lubridate","evd")
sapply(packages,require,character.only=TRUE)

rm(list=ls())


data_1 <- read_csv("data/OBS_BUOY_DD1.csv",col_types = "cDddddddddddd")
data_2 <- read_csv("data/OBS_BUOY_DD2.csv",col_types = "cDddddddddddd")
data_3 <- read_csv("data/OBS_BUOY_DD3.csv",col_types = "cDddddddddddd")

data <- do.call(bind_rows, list(data_1,data_2,data_3))


data2 <-
  data %>%
  mutate(year=year(일시)) %>% 
  group_by(지점,year) %>% 
  summarise(MST=max(`평균 수온(°C)`,na.rm=TRUE),.groups="drop") %>% 
  pivot_wider(names_from=지점,values_from=MST) %>% 
  arrange(year)
