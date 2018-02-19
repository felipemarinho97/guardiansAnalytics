# -*- coding: utf-8 -*-
"""
Análise dos LCCs

Amanda V. A. de Luna e Costa
Lívia Calvalcanti Juliao

"""
# Importando bibliotecas
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import datetime as dt

# Função para pegar o turno do acesso
def pega_turno(num):
    if(num <= 12):
        return "manha"
    elif(num > 12) and (num < 18):
        return "tarde"
    else:
        return "noite"
  
# Função para pegar o intervalo do acesso    
def pega_intervalo(num):
    if(num < 8):
        return "6h-8h"
    elif(num >= 8 and num < 10):
        return "8h-10h"
    elif(num >= 10 and num < 12):
        return "10h-12h"
    elif(num >= 12 and num < 14):
        return "12h-14h"
    elif(num >= 14 and num < 16):
        return "14h-16h"
    elif(num >= 16 and num < 18):
        return "16h-18h"
    else:
        return "18h+"
 
# Função para pegar o dia da semana
def pega_dia(data):
    data = dt.datetime.strptime(data, '%d/%m/%Y')
    dias = ["Seg","Ter","Qua","Qui","Sex","Sab","Dom"]
    dia_semana = dias[data.weekday()]
    return dia_semana

# Lendo o data frame
df = pd.read_csv("/Users/amandaluna/Documents/guardiansAnalytics/saida.csv",names = ["data","hora","maquina","usuario"],sep = ";")

#### Ajeitando o data frame ####

# Acrescentando as colunas ano, mês, dia e dia da semana
df["ano"] = df["data"].apply(lambda x:int(x[6:]))
df["mes"] = df["data"].apply(lambda x:int(x[3:5]))
df["dia"] = df["data"].apply(lambda x:int(x[:2]))
df["dia_da_semana"] = df["data"].apply(lambda x:pega_dia(x))


# Acrescentando as colunas de hora pura,turno e intervalo
df["hora_pura"] = df["hora"].apply(lambda x:int(x[:2]))
df["turno"] = df["hora_pura"].apply(lambda x:pega_turno(x))
df["intervalo"] = df["hora_pura"].apply(lambda x:pega_intervalo(x))

# Pegando o número da máquina, apenas
df["numero_maquina"] = df["maquina"].apply(lambda x:int(x[5:]))

#### Gerando gráficos ####








