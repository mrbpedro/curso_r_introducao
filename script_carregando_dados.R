#Carregando os dados
#Vamos começar com o clássico
rm(list = ls())

#Vamos dizer para o R em que pasta estão os dados
#ctrl + shift + h
#ou

setwd("C:/Users/mrbar/OneDrive/Desktop/Curso R Estatistica/Aula 1")

#com o comando dir() observamos o que tem dentro da past
dir()

#O dado que trabalharei é o swiid.csv

#O método tradicional é 

df <- read.csv("swiid.csv")

#Se fosse um arquivo excel seria necessária instalar um pacote
#readxl

#No entanto, o pacote rio facilita tudo. Ele importa praticamente
#todos os formatos apenas com a função import
install.packages("rio")
library(rio)

df <- import("swiid.csv")

#Para arquivos do próprio R basta a função load
df <- load("swiid.RData")

#==============================================
# Conhecendo os dados
#=============================================
df

#função para dar uma olha nas primeiras linhas
head(df, n = 10)

#função para listar os nomes das variáveis
names(df)

#função para detalhar a classe das variáveis
library(tidyverse)
glimpse(df)

#Para observarmos valores dentro da variável
df$country

library(janitor)
table(df$country)
tabyl(df$country)

#Summary te dá o sumário estatístico do R
summary(df)

#Mas é possível olhar estatísticas separadamente

#Média
mean(df$gini_disp)
#Mediana
median(df$gini_disp)
#valor máximo
max(df$gini_disp)
#valor mínimo
min(df$gini_disp)
#Missings
sum(is.na(df$gini_disp))


#ordenando valores
#linguagem tradicional
sort(df$gini_disp)

#tidyverse
df %>% 
  arrange(desc(gini_disp))

#ordenando valores por países
df %>% 
  group_by(country) %>% #agrupando por países
  arrange(desc(gini_disp)) %>%  #ordenando os valores de gini
  select(country, year, gini_disp) #selecionando as variáveis do output

#Ordenando valores de gini_disp por países, filtrado pelo ano 2015
df %>% 
  group_by(country) %>% #agrupando por países
  arrange(desc(gini_disp)) %>%  #ordenando os valores de gini
  select(country, year, gini_disp) %>% #selecionando as variáveis do output
  filter(year == 2015)

# "=" é atribuição "==" é operado númerico de igual

#Voltando para a apresentação

#=======================================
#Transformando os dados
#=======================================

#Vamos olhar os dados de distribuição de renda
#por quintis na América Latina
# Dados CEPAL

dir()#observando os dados no diretório

library(rio) #carregando o pacote rio

df <- import("quintil_renda.xlsx") #importando os dados

names(df)
#Observem que há letras maísculas e espaços, o que diculta
##na operacionalização dos dados daqui pra frente

# Uma função muito útil nesses casos é clean_names do pacote janitor
# Ele padroniza e tira os espaços
library(janitor)

#Dica: nunca modifique os dados originais.
#Sempre crie versões

#lt
df1 <- clean_names(df) 

#tidyverse
df1 <- df %>% 
  clean_names()

#Para fazer o pipe, basta pressionar ctrl + shift + m
names(df1)
#===================================
#2 Renomear as variáveis
#===================================
#O pacote dplyr possui uma função rename


#tidyverse
df1 <- df1 %>% 
  rename("country" = country_estandar,
         "year" = years_estandar)

#======================================
#3 Selecionar e filtrando variáveis
#======================================

#Ir para apresentação

#selecionando
df2 <- df1 %>% 
  select(year, country, quintile, value)


#excluindo variáveis
df2 <- df1 %>% 
  select(-geographical_area,
         -unit,
         -notes_ids,
         -source_id,
         -indicator)

#filtrando variáveis
 df2015 <- df2 %>% 
  filter(year == 2015)
 
#excluindo por ano 2015
 dfs2015 <- df2 %>% 
   filter(year != 2015)

 #filtrando por dois anos 2000 e 2020
 df2000_2020 <- df2 %>% 
   filter(year %in% c(2000, 2020))
 
 #O comando acima é equivalente a
 df2000_2020 <- df2 %>% 
   filter(year >= 2000 & year <= 2020)
   
 #excluindo os dois anos
 dfs2000_2020 <- df2 %>% 
   filter(year %in% !c(2000, 2020))
 
 #filtrando de 2010 a 2020
 df2010_2020 <- df2 %>% 
   filter(year %in% c(2000:2020))
 
 #filtrando pr duas variáveis
 df2 %>% 
   filter(year == 2015 & quintile == "Quintile 1")
 
 #filtrando o fatiando por linha
 
 df2 %>% 
   slice(1)

 #conjugando diferentes funções
 
 #Do maior para o menor valor do quintile 1 no ano de 2019
 df2 %>% 
   select(country, year, quintile, value) %>% 
   filter(year == 2019 & quintile == "Quintile 1" ) %>% 
   arrange(desc(value))
   
 
 #salvando os dados modificados
 
 save(df2, file = "df2.RData")
 write.csv(df2, file = "df2.csv")
 
 
