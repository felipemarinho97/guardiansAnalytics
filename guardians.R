library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(cluster) # Adicionei pra plotar o kmeans
library(fpc) # Adicionei pra plotar o kmeans

setwd("~/guardians") # workspace Livia
#setwd("/Users/amandaluna/Documents/guardians") # workspace Amanda
dados <- read_csv("logs.txt", col_names = c("mes", "dia_do_mes", "hora", "maquina", "status", "usuario"))

dados <- dados %>% mutate(data = paste("2017", mes, dia_do_mes, sep = "-"), dia_da_semana = wday(data, label = T))
sessoes_abertas <- dados %>% filter(status == "opened")

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
                                                                          if_else(hora_pura %in% 18:24, "18h+", "06-08"))))))

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
acessos_usuarios <- sessoes_por_usuario %>% group_by(usuario) %>% summarise(num_acessos = n())
acessos_usuarios <- acessos_usuarios[order(acessos_usuarios$num_acessos,decreasing = T),]

# maquinas que tiveram mais acessos
sessoes_por_maquina <- sessoes_abertas %>% subset(select=c("maquina"))
acessos_maquinas <- sessoes_por_maquina %>% group_by(maquina) %>% summarise(num_acessos = n())
acessos_maquinas <- acessos_maquinas[order(acessos_maquinas$num_acessos,decreasing = T),] # As do começo do lcc2

######################################################
acessos_usuarios.stand <- scale(acessos_usuarios[-1])
k.means.fit <- kmeans(acessos_usuarios.stand, 2) # k = 3
attributes(k.means.fit)
k.means.fit$centers
k.means.fit$cluster
k.means.fit$size

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(acessos_usuarios)-1)*sum(apply(acessos_usuarios,2, FUN = var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")}

wssplot(acessos_usuarios.stand, nc=6)
clusplot(acessos_usuarios.stand,k.means.fit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=6)
table(unlist(acessos_usuarios[,1]), k.means.fit$cluster)
dist(acessos_usuarios.stand, method = "euclidean")
table(unlist(acessos_usuarios[,1]),groups)
# Como considerar atributos hora e maquina p o kmeans(dica de design)
######################################### 
clus <- kmeans(acessos_usuarios,centers = 4,nstart = 25) # Só deu pra fazer kmeans disso :/
clus
clusplot(acessos_usuarios,clus$cluster,color = T,shade = T,labels = 2,lines = 0) # Deu esse ngç bugado

