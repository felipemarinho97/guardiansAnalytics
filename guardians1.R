library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("~/guardians")
dados <- read_csv("logs.txt", col_names = c("mes", "dia_do_mes", "hora", "maquina", "status", "usuario"))

dados <- dados %>% mutate(data = paste("2017", mes, dia_do_mes, sep = "-"), dia_da_semana = wday(data, label = T))
sessoes_abertas <- dados %>% filter(status == "opened")

sessoes <- sessoes_abertas %>% subset(select=c("dia_da_semana", "dia_do_mes"))
sessoes <- sessoes %>% data.frame()

#plotar o grafico
sessoes_abertas %>% ggplot(aes(x = dia_da_semana)) +  geom_bar()
sessoes %>% ggplot(aes(x = dia_da_semana)) +  geom_bar()
