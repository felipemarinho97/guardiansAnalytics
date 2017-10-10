library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

#setwd("~/guardians") seu workspace
setwd("/Users/amandaluna/Documents/guardians")
dados <- read_csv("logs.txt", col_names = c("mes", "dia_do_mes", "hora", "maquina", "status", "usuario"))

dados <- dados %>% mutate(data = paste("2017", mes, dia_do_mes, sep = "-"), dia_da_semana = wday(data, label = T))
sessoes_abertas <- dados %>% filter(status == "opened")

# acessos pelo dia do mes e da semana
sessoes <- sessoes_abertas %>% subset(select=c("dia_da_semana", "dia_do_mes"))
freq_dia <- sessoes %>% group_by(dia_da_semana, dia_do_mes) %>% summarise(num_acessos = n())
freq_dia %>% ggplot(aes(x = dia_do_mes, y = num_acessos, fill = dia_da_semana)) + geom_bar(stat = "identity")

# acessos no mês de acordo com o dia da semana
freq_mensal <- freq_dia %>% group_by(dia_da_semana) %>% summarise(media_acessos = mean(num_acessos))
freq_mensal %>% ggplot(aes(x = dia_da_semana, y = media_acessos)) + geom_bar(stat = "identity")

# extraindo colunas do lab, da "hora pura", do turno
r <- setNames(do.call(rbind.data.frame, strsplit(sessoes_abertas$maquina, "-")), c("lab", "maquina"))
sessoes_abertas <- sessoes_abertas %>% mutate(lab = r$lab)
sessoes_abertas <- sessoes_abertas %>% mutate(hora_pura = hour(sessoes_abertas$hora))
sessoes_abertas <- sessoes_abertas %>% mutate(turno = if_else(hora_pura %in% 05:11, "manha",
                                                              if_else(hora_pura %in% 12:17, "tarde",
                                                                      if_else(hora_pura %in% 18:24, "noite", "madrugada"))))

# graficos do turno de acesso por lcc
sessoes_abertas %>% ggplot(aes(x = lab)) + geom_bar()
sessoes_abertas %>% ggplot(aes(x = turno)) + geom_bar()
sessoes_abertas %>% ggplot(aes(x = hora_pura)) + geom_bar() # ao contrario do que se esperava, 

#o horario de mais acesso ao lcc é as 10 hrs
lcc2_acesso <- sessoes_abertas %>% filter(lab == "lcc2")
lcc2_acesso %>% ggplot(aes(x = hora_pura)) + geom_bar()

lcc1_acesso <- sessoes_abertas %>% filter(lab == "lcc1")
lcc1_acesso %>% ggplot(aes(x = hora_pura)) + geom_bar() # maior quantudade de acesso as 14 hrs
lcc1_acesso %>% ggplot(aes(x = dia_da_semana)) + geom_bar()

# usuarios que mais logaram nas maquinas
sessoes_por_usuario <- sessoes_abertas %>% subset(select=c("usuario"))
acessos_usuarios <- sessoes_por_usuario %>% group_by(usuario) %>% summarise(num_acessos = n())
acessos_usuarios <- acessos_usuarios[order(acessos_usuarios$num_acessos,decreasing = T),]

# maquinas que tiveram mais acessos
sessoes_por_maquina <- sessoes_abertas %>% subset(select=c("maquina"))
acessos_maquinas <- sessoes_por_maquina %>% group_by(maquina) %>% summarise(num_acessos = n())
acessos_maquinas <- acessos_maquinas[order(acessos_maquinas$num_acessos,decreasing = T),]
