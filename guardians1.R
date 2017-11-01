library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(cluster) # Adicionei pra plotar o kmeans
library(fpc) # Adicionei pra plotar o kmeans

#setwd("~/Área de Trabalho/guardians/guardiansAnalytics/") # workspace Livia
setwd("/Users/amandaluna/Documents/guardiansAnalytics") # workspace Amanda
dados <- read_csv("logs.txt", col_names = c("mes", "dia_do_mes", "hora", "maquina", "status", "usuario"))

dados <- dados %>% mutate(data = paste("2017", mes, dia_do_mes, sep = "-"), dia_da_semana = wday(data, label = T))
sessoes_abertas <- dados %>% filter(status == "opened")

# super data frame com todos os dados
super <- setNames(do.call(rbind.data.frame, strsplit(sessoes_abertas$maquina, "-")), c("lab", "maquina"))
super <- sessoes_abertas %>% mutate(hora_pura = hour(sessoes_abertas$hora))
super <- super %>% mutate(turno = if_else(hora_pura %in% 05:11, "manha",
                                          if_else(hora_pura %in% 12:17, "tarde",
                                                  if_else(hora_pura %in% 18:24, "noite", "madrugada"))))
super <- super %>% mutate(horario = if_else(hora_pura %in% 08:09, "08-10", # 14h as 16h maior pico
                                            if_else(hora_pura %in% 10:11, "10-12",
                                                    if_else(hora_pura %in% 12:14, "12-13",
                                                            if_else(hora_pura %in% 14:15, "14-16",
                                                                if_else(hora_pura %in% 16:17, "16-18",
                                                                    if_else(hora_pura %in% 18:24, "18h+", "06-08")))))))
r <- setNames(do.call(rbind.data.frame, strsplit(sessoes_abertas$maquina, "-")), c("lab", "maquina"))
super <- super %>% mutate(lab = r$lab)

# acessos por lab
lab <- super
lab<- lab %>% group_by(lab, usuario) %>% mutate(n_acessos=n()) #%>%
  inner_join(lab, by='usuario')
lab
# acessos pelo dia do mes e da semana
sessoes <- sessoes_abertas %>% subset(select=c("dia_da_semana", "dia_do_mes"))
freq_dia <- sessoes %>% group_by(dia_da_semana, dia_do_mes) %>% summarise(num_acessos = n())
freq_dia %>% ggplot(aes(x = dia_do_mes, y = num_acessos, fill = dia_da_semana)) + geom_bar(stat = "identity")

# acessos no mês de acordo com o dia da semana
freq_mensal <- freq_dia %>% group_by(dia_da_semana) %>% summarise(media_acessos = mean(num_acessos)) # Os mts de lp1 não estão computados 
freq_mensal %>% ggplot(aes(x = dia_da_semana, y = media_acessos)) + geom_bar(stat = "identity")

# extraindo colunas do lab, da "hora pura", do turno
r <- setNames(do.call(rbind.data.frame, strsplit(sessoes_abertas$maquina, "-")), c("lab", "maquina"))
sessoes_abertas <- sessoes_abertas %>% mutate(lab = r$lab)
sessoes_abertas <- sessoes_abertas %>% mutate(hora_pura = hour(sessoes_abertas$hora))
sessoes_abertas <- sessoes_abertas %>% mutate(turno = if_else(hora_pura %in% 05:11, "manha",
                                                              if_else(hora_pura %in% 12:17, "tarde",
                                                                      if_else(hora_pura %in% 18:24, "noite", "madrugada"))))

# sessoes por blocos de 2 horas
sessoes_abertas <- sessoes_abertas %>% mutate(horario = if_else(hora_pura %in% 08:09, "08-10", # 14h as 16h maior pico
                                                              if_else(hora_pura %in% 10:11, "10-12",
                                                                      if_else(hora_pura %in% 12:13, "12-13",
                                                                        if_else(hora_pura %in% 14:15, "14-16",
                                                                          if_else(hora_pura %in% 16:17, "16-18",
                                                                            if_else(hora_pura %in% 18:24, "18h+", "06-08")))))))

sessoes_abertas %>% ggplot(aes(x = horario)) + geom_bar() # 14h as 16h é o horário de pico 

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
acessos_usuarios <- sessoes_abertas %>% group_by(usuario) %>% summarise(num_acessos = n())
acessos_usuarios <- acessos_usuarios[order(acessos_usuarios$num_acessos,decreasing = T),]

# maquinas que tiveram mais acessos
sessoes_por_maquina <- sessoes_abertas %>% subset(select=c("maquina"))
acessos_maquinas <- sessoes_por_maquina %>% group_by(maquina) %>% summarise(num_acessos = n())
acessos_maquinas <- acessos_maquinas[order(acessos_maquinas$num_acessos,decreasing = T),] # As do começo do lcc2

########################### SETUP PRA FAZER A TABELA DO KMEANS ###############################
########################### SE MEXER MORRE ###################################################
teste <- super %>% subset(select = c("usuario","lab"))
teste <- teste %>% group_by(usuario,lab) %>% summarise(num_acessos = n())
a <- teste %>% filter(lab == "lcc1")
names(a)[3] <- c("lcc1")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,3][is.na(acessos_usuarios[,3])] <- 0

teste <- super %>% subset(select = c("usuario","lab"))
teste <- teste %>% group_by(usuario,lab) %>% summarise(num_acessos = n())
a <- teste %>% filter(lab == "lcc2")
names(a)[3] <- c("lcc2")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,4][is.na(acessos_usuarios[,4])] <- 0


teste <- super %>% subset(select = c("usuario","dia_da_semana"))
teste <- teste %>% group_by(usuario,dia_da_semana) %>% summarise(num_acessos = n())
a <- teste %>% filter(dia_da_semana == "Mon")
names(a)[3] <- c("Mon")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,5][is.na(acessos_usuarios[,5])] <- 0

teste <- super %>% subset(select = c("usuario","dia_da_semana"))
teste <- teste %>% group_by(usuario,dia_da_semana) %>% summarise(num_acessos = n())
a <- teste %>% filter(dia_da_semana == "Tues")
names(a)[3] <- c("Tues")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,6][is.na(acessos_usuarios[,6])] <- 0


teste <- super %>% subset(select = c("usuario","dia_da_semana"))
teste <- teste %>% group_by(usuario,dia_da_semana) %>% summarise(num_acessos = n())
a <- teste %>% filter(dia_da_semana == "Wed")
names(a)[3] <- c("Wed")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,7][is.na(acessos_usuarios[,7])] <- 0


teste <- super %>% subset(select = c("usuario","dia_da_semana"))
teste <- teste %>% group_by(usuario,dia_da_semana) %>% summarise(num_acessos = n())
a <- teste %>% filter(dia_da_semana == "Thurs")
names(a)[3] <- c("Thurs")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,8][is.na(acessos_usuarios[,8])] <- 0


teste <- super %>% subset(select = c("usuario","dia_da_semana"))
teste <- teste %>% group_by(usuario,dia_da_semana) %>% summarise(num_acessos = n())
a <- teste %>% filter(dia_da_semana == "Fri")
names(a)[3] <- c("Fri")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,9][is.na(acessos_usuarios[,9])] <- 0


teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "06-08")
names(a)[3] <- c("6h-8h")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,10][is.na(acessos_usuarios[,10])] <- 0

teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "08-10")
names(a)[3] <- c("8h-10h")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,11][is.na(acessos_usuarios[,11])] <- 0

teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "10-12")
names(a)[3] <- c("10h-12h")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,12][is.na(acessos_usuarios[,12])] <- 0

teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "12-13")
names(a)[3] <- c("12h-14h")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,13][is.na(acessos_usuarios[,13])] <- 0

teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "14-16")
names(a)[3] <- c("14h-16h")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,14][is.na(acessos_usuarios[,14])] <- 0

teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "16-18")
names(a)[3] <- c("16h-18h")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,15][is.na(acessos_usuarios[,15])] <- 0

teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "18h+")
names(a)[3] <- c("18h+")
a <- a[,-2]
acessos_usuarios <- full_join(acessos_usuarios,a, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")
acessos_usuarios[,16][is.na(acessos_usuarios[,16])] <- 0

teste <- super %>% subset(select = c("usuario","maquina"))
teste <- teste %>% group_by(usuario) %>% summarise(num_acessos = n())
names(teste)[2] <- c("acesso__por_maquina")
acessos_usuarios <- full_join(acessos_usuarios,teste, by = "usuario")
#tabela <- full_join(a,b,by = "usuario")


dataKmeans <- acessos_usuarios %>% select(usuario,num_acessos,acesso__por_maquina,lcc1,lcc2,Mon,Tues,Wed,Thurs,Fri,`6h-8h`,`8h-10h`,`10h-12h`,`12h-14h`,`14h-16h`,`16h-18h`,`18h+`) 
names(dataKmeans)[2] <- c("acessos_totais")
names(dataKmeans)[3] <- c("qtd_maquinas")
######################################################
clus <- kmeans(acessos_usuarios,centers = 4,nstart = 25) # Só deu pra fazer kmeans disso :/
clusplot(acessos_usuarios,clus$cluster,color = T,shade = T,labels = 2,lines = 0) # Deu esse ngç bugado

