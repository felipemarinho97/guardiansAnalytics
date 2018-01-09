library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(cluster)
library(fpc)
library(Gmedian)

dados <- read_csv("dados/logs.txt", col_names = c("mes", "dia_do_mes", "hora", "maquina", "status", "usuario"))

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
lab <- lab %>% group_by(lab, usuario) %>% mutate(n_acessos=n()) #%>%
#inner_join(lab, by='usuario')
head(lab)
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

########################### setup para o dataframe do kmeans ###############################
# Cada bloco representa uma coluna sendo adicionada ao dataframe acessos_usuario

teste <- super %>% subset(select = c("usuario","lab"))# Seleciona apenas os atributos que me interessam
teste <- teste %>% group_by(usuario,lab) %>% summarise(num_acessos = n()) # Sumarizo os acessos
a <- teste %>% filter(lab == "lcc1") # Filtro de acordo com o meu interesse
names(a)[3] <- c("lcc1") # Nomeio a coluna
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario") # Agrupo com o dataframe principal

a <- teste %>% filter(lab == "lcc2")
names(a)[3] <- c("lcc2")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

teste <- super %>% subset(select = c("usuario","dia_da_semana"))
teste <- teste %>% group_by(usuario,dia_da_semana) %>% summarise(num_acessos = n())
a <- teste %>% filter(dia_da_semana == "Seg")
names(a)[3] <- c("Seg")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(dia_da_semana == "Ter")
names(a)[3] <- c("Ter")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(dia_da_semana == "Qua")
names(a)[3] <- c("Qua")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(dia_da_semana == "Qui")
names(a)[3] <- c("Qui")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(dia_da_semana == "Sex")
names(a)[3] <- c("Sex")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

teste <- super %>% subset(select = c("usuario","horario"))
teste <- teste %>% group_by(usuario,horario) %>% summarise(num_acessos = n())
a <- teste %>% filter(horario == "06-08")
names(a)[3] <- c("06h-08h")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(horario == "08-10")
names(a)[3] <- c("08h-10h")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(horario == "10-12")
names(a)[3] <- c("10h-12h")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(horario == "12-13")
names(a)[3] <- c("12h-14h")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(horario == "14-16")
names(a)[3] <- c("14h-16h")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(horario == "16-18")
names(a)[3] <- c("16h-18h")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

a <- teste %>% filter(horario == "18h+")
names(a)[3] <- c("18h+")
acessos_usuarios <- full_join(acessos_usuarios,a[-2], by = "usuario")

acessos_usuarios[,1:16][is.na(acessos_usuarios[,1:16])] <- 0 # Substituindo os NA por zero

dataKmeans <- acessos_usuarios %>% subset(select = c("num_acessos","06h-08h","08h-10h","10h-12h","12h-14h","14h-16h","16h-18h","18h+")) # filtrando para apenas número de acessos e horários

# Tentando identificar possíveis outliers com um boxplot
mediana_acessos <- dataKmeans %>% mutate(Mediana = median(num_acessos))
mediana_acessos %>% ggplot(aes(x = Mediana, y = num_acessos)) + geom_boxplot(alpha = 1/8, na.rm = T)

######################### Vendo o silhouette #############################
distancia = as.matrix(dist(dataKmeans)) # Matriz de dissimilaridade
clus <- kmeans(dataKmeans,centers = 2) 
sil1 <- mean(silhouette(clus$cluster,dmatrix = distancia^2)[,3]) # Silhouette com 2 centros
dataSil <- data.frame(centros = 2,distSil = sil1)

for(i in 3:10){ # Silhouette de 3 a 10 centros
  set.seed(10)
  clus = kmeans(dataKmeans,centers = i)
  sill <- mean(silhouette(clus$cluster,dmatrix = distancia^2)[,3])
  dfAux <-  data.frame(centros = i,distSil = sill)
  dataSil <- rbind(dataSil,dfAux)
}

dataSil %>% ggplot(aes(x = centros,y = distSil)) + geom_line() # plotando o gráfico

############ K-MEANS ############
clus <- kmeans(dataKmeans,centers = 2) # kmeans com 2 centros
clus$centers # Verificando os centros
table(clus$cluster) # Vendo quantos pontos estão em cada grupo 
clusplot(dataKmeans,clus$cluster,color = T,shade = T) # plotando o gráfico do kmeans
clus$withinss

########### Pegando os dados do número de acessos até o  3 quartil ###################
summary(dataKmeans) # Verificando média,mediana e quartis
dadosMenores = dataKmeans %>% filter(num_acessos <= 19) # Filtrando o acesso até o 3 quartil

# Tentando identificar possíveis outliers com um boxplot
mediana_acessosMenores <- dadosMenores %>% mutate(Mediana = median(num_acessos))
mediana_acessosMenores %>% ggplot(aes(x = Mediana, y = num_acessos)) + geom_boxplot(alpha = 1/8, na.rm = T)

######### Silhouette de dadosMenores ############
distancia = as.matrix(dist(dadosMenores)) # Matriz de dissimilaridade
clus2 <- kmeans(dadosMenores,centers = 2) 
sil11 <- mean(silhouette(clus2$cluster,dmatrix = distancia^2)[,3]) # Silhouette com 2 centros
dfSil <- data.frame(centros = 2,distS = sil11)

set.seed(12)
for(i in 3:10){ # Silhouette de 3 a 10 centros
  clus2 = kmeans(dadosMenores,centers = i)
  silT <- mean(silhouette(clus2$cluster,dmatrix = distancia^2)[,3])
  df2 <-  data.frame(centros = i,distS = silT)
  dfSil <- rbind(dfSil,df2)
}

dfSil %>% ggplot(aes(x = centros,y = distS)) + geom_line() # plotando o gráfico

############# kmeans do dadosMenores ############
clus2 <- kmeans(dadosMenores, centers = 2) # kmeans desse df com 2 centros
clusplot(dadosMenores,clus2$cluster,color = T, shade = T) # Plotagem do kmeans
table(clus2$cluster) # Quantidade de usuários em cada cluster
clus2$centers # Verificando os dados dos centros 
# acessoCluster <- kcca(acessos_usuarios$num_acessos, k=3, family=kccaFamily("kmedians"),save.data=TRUE)
# acessoCluster
# plot(acessoCluster)
# acessoCluster <- kcca(dataKmeans, k=8, family=kccaFamily("kmedians"),save.data=TRUE)
acessoCluster1 <- kGmedian(dataKmeans, ncenters=2)

plot(dataKmeans, col = acessoCluster1$cluster, main="kmedian")
points(acessoCluster1$centers, col = 1:2, pch = 8, cex = 2)
acessoCluster1$centers
acessoCluster1$size

##### Olhando usuários com apenas um acesso ######
super <- super %>% group_by(usuario) %>% mutate(n_acessos=n()) # adicionando a coluna n_acessos no super
dadosUmAcesso <- super %>% group_by(usuario) %>% filter(n_acessos == 1) # filtando usuários com apenas um acesso
dadosUmAcesso %>% ggplot(aes(x = dia_do_mes)) + geom_bar() # plotando dia do mês
dadosUmAcesso %>% ggplot(aes(x = dia_da_semana)) + geom_bar() # plotando dia da semana
dadosUmAcesso %>% ggplot(aes(x = lab)) + geom_bar() # plotando lab
dadosUmAcesso %>% ggplot(aes(x = horario)) + geom_bar() # plotando horário(blocos de 2 horas)

##### Quem compõe a maior parte dos nossos usuários? #####
# Usuários com 0 a 10 acessos são maioria (sendo 0 a 5 maioria massiva)
# Seria melhor fazer de 0 a 5?
hist(acessos_usuarios$num_acessos,main = "Histograma do número de acessos", xlab = "Número de acessos", ylab = "Frequência") # Histograma dos acessos
maioria = acessos_usuarios %>% filter(num_acessos <= 10) # Filtrando os usuários com até 10 acessos
dias <- data.frame(dia = c("Segunda","Terca","Quarta","Quinta","Sexta"), acessos = c(sum(maioria$Seg),sum(maioria$Ter),sum(maioria$Qua),sum(maioria$Qui),sum(maioria$Sex))) 
dias$dia <- factor(dias1$dia, levels = dias$dia[order(dias$acessos)]) #faz com que o plot seja na ordem da coluna dia do data frame
horarios <- data.frame(hora = c("06h-08h","08h-10h","10h-12h","12h-14h","14h-16h","16h-18h","18h+"), acessos = c(sum(maioria$`06h-8h`),sum(maioria$`08h-10h`),sum(maioria$`10h-12h`),sum(maioria$`12h-14h`),sum(maioria$`14h-16h`),sum(maioria$`16h-18h`),sum(maioria$`18h+`)))
dias %>% ggplot(aes(x = dia,y = acessos)) + theme_bw() + geom_bar(stat = "identity")
horarios %>% ggplot(aes(x = hora,y = acessos)) + geom_bar(stat = "identity")

usuarios <- dados 
usuarios <- usuarios %>% mutate(hora_pura = hour(usuarios$hora))
usuarios <- usuarios[order(usuarios$usuario),]
tempo_sessao <- as.data.frame(matrix(nrow = 3809, ncol = 4))
indice = 1
7618
for( i in 1:7618) {
  if(usuarios[i+1,"status"] == "closed" && usuarios[i,"status"] == "opened") {
    if(usuarios[i+1,"usuario"] == usuarios[i,"usuario"]){
    tempo_sessao[indice,1] <- ((usuarios[i+1,"hora"] - usuarios[i,"hora"]) / 60)
    tempo_sessao[indice,2] <- usuarios[i+1,"usuario"]
    tempo_sessao[indice,3] <- usuarios[i+1,"dia_da_semana"]
    tempo_sessao[indice,4] <- usuarios[i+1,"hora_pura"]
    indice = indice + 1
    i = i + 1 
  }
  }
}

tempo_sessao <- tempo_sessao %>% na.omit %>% rename("tempo" = V1, "usuario" = V2, "dia" = V3, "hora" = V4)
