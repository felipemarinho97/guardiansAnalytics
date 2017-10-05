library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

setwd("~/Área de Trabalho/guardians")
dados <- read_csv("dados/logs.txt", col_names = c("mes", "dia_do_mes", "hora", "maquina", "status", "usuario"))

dados <- dados %>% mutate(data = paste("2017", mes, dia_do_mes, sep = "-"), dia_da_semana = wday(data, label = T))
sessoes_abertas <- dados %>% filter(status == "opened")

# acessos pelo dia do mes e da semana
sessoes <- sessoes_abertas %>% subset(select=c("dia_da_semana", "dia_do_mes"))
freq_dia <- sessoes %>% group_by(dia_da_semana, dia_do_mes) %>% summarise(num_acessos = n())
freq_dia %>% ggplot(aes(x = dia_do_mes, y = num_acessos, fill = dia_da_semana)) + geom_bar(stat = "identity")

# acessos no mês de acordo com o dia da semana
freq_mensal <- freq_dia %>% group_by(dia_da_semana) %>% summarise(media_acessos = mean(num_acessos))
freq_mensal %>% ggplot(aes(x = dia_da_semana, y = media_acessos)) + geom_bar(stat = "identity")
