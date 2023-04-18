#==============================================================
# Antes de tudo
#===============================================================
# 1) Antes de tudo, crie uma pasta para o seu projeto de pesquisa.

# Saiba exatamente o endereço desta pasta.

# É preciso informar ao R esta pasta, pois nela serão armazenados os scripts,
# os dados e os outputs produzidos nas análises feitas.

# Isso é feito a partir do comando setwd

# Exemplo no meu IOS windows:
setwd("C:/Users/mrbar/OneDrive/Desktop/Curso R Estatistica/Aula 1")
# ctrl + shift + h

# Use sempre ctrl + enter para rodar o comando (Windows)
# Cmd + Return (Mac)

# Método alternativo ctrl + shift + h 


#=============================================================
# Script
#=============================================================

# Este é o script do R. Ele é o roteiro de todo os passos tomados em sua análise.

# O script é o principal recurso que permite a reproducibilidade de sua análise.

# O "#" avisa para o R que isso é um comentário. Portanto, o R ignora o que é 
## é escrito a partir do #.

# E é sempre importante rechear seus scripts de comentários para torná-los compreensíveis
## É um favor que se faz a coautores e ao seu "eu" do futuro.

# Explore o RStudio: tools, output, console e layout.

# Vale sempre iniciar um script com o comando abaixo que limpa o que está na memória
## e evita que você sobreponha dados e projetos
rm(list = ls())

#=======================================================
# Instalando pacotes
#========================================================

# 1 Método clássico

install.packages("pacman")
install.packages("tidyverse")
install.packages("tidylog")
# Utiliza a função install.packpages para a instalação do 
## pacote pacman

# Após instalar é preciso executar o pacote com a função library
library(pacman)
library(tidyverse)
library(tidylog)

# 2 Método alternativo
# tools 


# 3 Método produtivo (instala e executa de uma vez)
pacman::p_load(tidyverse, tidylog)


#=====================================================
# Operadores
#=====================================================

# O R entende operações matemáticas
# Adição
2 + 2

# Multiplicação
2 * 2

# Divisão 
2/2

# Exponencial
2^2

#Para raíz quadrada usamos a função 
sqrt(4)

#Para logarítmo usamos a função
log(36, base =6)

#Sempre que tiver dúvida de uma função/pacote utilize
help(log)

#Em operações mais complexas utilize o parântese para a ordem das operações
(3+5/78)^3*7

3+5/78^3*7

#Voltar para a apresentação

#===================================================================
# Objetos
#=================================================================


# Criamos um objeto a partir do operado <-
# Um atalho para este operador é alt e - 

quatro <- 4
quatro

#Se você criar outro objeto com o mesmo nome, ele vai sobrescrever o anterior

quatro <- 5
quatro

#Se quisermos armazenar texto é preciso colocar entre aspas

dog <- "vira_lata" 
dog

#Para armazenar mais valores utiliza-se c
times <- c("Atlético-MG", "São Paulo", "Flamengo", "Cruzeiro")
titulos <- c(2, 6, 7, 3) 

#A função cbind (colummbind) combina objetos por colunas
ob <- cbind(times, titulos)
ob

#O que te parece? Temos um dataframe

#Podemos colar também a partir de linhas

times <- "Internacional"
titulos <- 4

ob2 <- cbind(times, titulos) 

ob3 <- rbind(ob, ob2)
ob3

#Mas podemos criar dataframes diretamente

df <- data.frame(times = c("Atlético-MG", "São Paulo", "Flamengo", "Cruzeiro",
                           "Internacional"),
                 titulos = c(2, 6, 7, 3,4))
df
#Para observar a coluna do dataframe utilizamos $
df$times

#De volta à apresentação.

