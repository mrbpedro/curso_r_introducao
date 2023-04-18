rm(list = ls())

pacman::p_load(tidylog, janitor, rio)
#carregando os pacotes

#======================================================================
#Continuando manipulação dos bancos
#======================================================================

setwd("C:/Users/mrbar/OneDrive/Desktop/Curso R Estatistica/Aula2")

dir()

#dados sobre gastos públicos

df <- import("SPEED2019.xls", sheet = 2)


names(df)
#Ele não reconhece a primeira linha como variável


#Com o comando row_to_names eu indico a linha que dever ser a head
df <- df %>% 
  janitor::row_to_names(row_number = 1)


names(df)
#Problemas no banco:

#1) Os anos vem com y na frente. Isso será um problema para o R interpretar esse 
## como número que tem uma ordem crescente.

#Como editar os dados diretamente no R?
fix(df)
teste <- DataEditR::data_edit(df)


glimpse(df)

#2) Os valores numéricos estão como texto

tabyl(df$varname)
# O banco têm diferentes variáveis, vale a pena focar naquelas de interesse.
# Como cientista social, estou interessado nas políticas sociais
# Enfoco o gasto em proporção com o PIB em saúde, educação e seguridade social
# Vamos filtrar o banco então por esssa variáveis

df <- df %>% 
  filter(varname %in% c("edu_pctgdp", "hl_pctgdp", "sp_pctgdp"))

#Pergunta: como é que eu sei que são estas as variáveis que procuro?

#E vamos cortar o banco para as variáveis de interesses

df <- df %>% 
  select(-sector, -unit)


#Existem diferentes formas para lidar com os problemas apontados acima.

#Eu vou optar por transformar o banco de wide para long
#Isto é: agregar os valores em duas colunas

#A função gather ajuda a fazer isso.
#Eu vou criar a colunar year e ao lado dela colocar os valores
## correspondentes de cada ano por linha

dfl <- df %>% 
  gather(year, value, 6:43)

#Funções mais recentes

dfl <- df %>%
  pivot_longer(cols = 6:43, 
               names_to = "year", 
               values_to = "values") 


#Agora vamos fazer o exerício contrário. 
#Transformar de long para wide a coluna varname

dfl1 <- dfl %>% 
  spread(varname, values)

dfl1 <- dfl %>% 
  pivot_wider(names_from = "varname",
              values_from = "values")

#Precisamos extrair agora o y do year 
library(stringr)

dfl1$year <- str_sub(dfl1$year, start=2)

#A função mutate modifica a variável
dfl1 <- dfl1 %>% 
  mutate(year = str_sub(year, start=2))


#Transformando a classe das variáveis

dfl1 <- dfl1 %>% 
  mutate(year = as.numeric(year)) #para numérico

#Calcular o gasto total
dfl1 <- dfl1 %>% 
  mutate(total_social = (hl_pctgdp + edu_pctgdp + sp_pctgdp))
#por que erro?

#Vamos transformar várias variáveis ao mesmo tempo
dfl1 <- dfl1 %>% 
  mutate_at(6:8, as.numeric)


dfl1 <- dfl1 %>% 
  mutate(total_social = (hl_pctgdp + edu_pctgdp + sp_pctgdp))

#vamos observar os missings

summary(dfl1)

#vamos excluir por todos os missings de total social

df_sm <- dfl1 %>% 
  filter(!is.na(total_social))

#Agora vamos calcular a média do gasto social total por região ao longo do tempo

tabyl(df_sm$region)

df_sm <- df_sm %>% 
  group_by(wb_region, year) %>% # agrupar por região e por ano
  mutate(mean_gasto = mean(total_social, na.rm = TRUE))
  

library(dplyr)
library(ggplot2)

df_sm %>%
 ggplot() +
 aes(x = year, y = mean_gasto, fill = wb_region, colour = wb_region) +
 geom_line() +
 scale_fill_hue(direction = 1) +
 scale_color_hue(direction = 1) +
 theme_bw() +
 theme(legend.position = "bottom",
       legend.title = element_blank()) +
  xlab("") +
  ylab("") +
  ylim(0, 28)+
  labs(title = "Gasto social (% GDP) por região")

#agora vamos codificar uma outra variável com base no em gasto total
#de acordo com os seus níveis: alto, baixo e médio.
summary(df_sm$mean_gasto)

df_sm <- df_sm %>%
  group_by(wb_region) %>% 
  mutate(gasto_nivel = case_when(mean_gasto <= 10.68 ~ "baixo",
                                 mean_gasto <= 21.307~ "intermediário",
                                 TRUE ~ "alto"))
  
df_sm %>% 
  select(year, wb_region, isocode, total_social, gasto_nivel) %>% 
  filter(gasto_nivel == "intermediário" & year == 2015) %>% 
  arrange(desc(total_social))

# A gente pode estabelecer uma ordem hierárquica entre alto, intermediário e alto

df_sm <- df_sm %>% 
  mutate(gasto_nivel = factor(gasto_nivel, levels = c("baixo","intermediário", "alto", ordered = FALSE)))


df_sm %>% 
  select(isocode, year, gasto_nivel, total_social) %>% 
  filter(isocode %in% c("SWE","USA","BGD") & year == 2014) %>% 
  arrange(desc(gasto_nivel))

#salvando o banco em formato R
save(df_sm, file = "dfm_sm.RData")
#salvando o banco em formato csv
write.csv(df_sm, file = "df_sm.csv")

# Ir para apresentação

#=================================================================================
# Junção de bancos
#===================================================================================

rm(list = ls())

#carregar os pacotes que serão utilizados
pacman::p_load(tidyverse, janitor, rio)


#Suponhamos que queiramos observar a relação entre desigualdade e gasto em políticas sociais

#É preciso juntar o banco de desigualdade (SWIID) com o de gasto social

dir()

#importar o banco do SWIID
sd <- import("swiid.csv")

#importar o banco SPEED já limpo
load("dfm_sm.RData")

#Primeiro vamos observar as colunas e os nomes
## É preciso que tenham colunas equivalentes para ocorrer a colagem

names(sd)
glimpse(sd)

names(df_sm)
glimpse(df_sm)

#As variáveis country e year são compatíveis (estão na mesma classe)
## e serão as chaves de colagem

tabyl(df_sm$country)
tabyl(sd$country)

#Alguns nomes de países variam
## Por exemplo: Korea e Republic of Korea
## Bolivia ou Bolivia (Plurinational state of)
## United Kingdom of Great Britan ou United Kingdom

# O R reconhece nomes diferentes como observações diferentes.

# Se aplicarmos, por exemplo, a função full_join ele vai duplicar 

#full_join retêm todos os valores, todas as linhas

df <- full_join(df_sm, sd, by = c("country", "year"))

tabyl(df$country)

#Portanto, ele repete Bolivia e Bolivia (Plurination state)
#O problema disso é que não cruzamos os dados para estes casos

#O inner_join retem somente as linhas nos dois bancos
df2 <- inner_join(df_sm, sd, by = c("country", "year"))

tabyl(df2$country)

#Agora temos os dados cruzados, porém perdemos United Kingdom, Bolivia, Korea etc

#Na grande maioria dos casos, o left_join ou right_join é o mais produtivo.

#Neste caso, é interessante procurar mecanismos de padronização

#Uma estratégia usual é renomear os países

#O pacote stringr ajuda

library(stringr)
tabyl(df_sm$country)

df_sm <- df_sm %>% 
  mutate(country = str_replace_all(country,"Republic of Korea", "Korea"))

tabyl(df_sm$country)

#Felizmente para o caso de países, alguém criou um pacote que facilita isso

install.packages("countrycode")
library(countrycode)

sd <- sd %>% 
  mutate(isocode = countrycode(country, origin = 'country.name', destination = 'iso3c'))

#Agora para juntar vale excluir a variável country de sd

sd <- sd %>% 
  select(-country)

#Agora vamos juntar

dado <- left_join(df_sm, sd, by = c("year", "isocode"))

#calculando correlação de pierson
cor(dado$gini_disp, dado$total_social, use='complete.obs', method = "pearson")
    
save(dado, file = "gini_gasto.RData")

#Voltar para a apresentação...

#=============================================================
# Visualização
#===============================================================
#pacote esquisseR

install.packages("esquisse")
library(esquisse)
