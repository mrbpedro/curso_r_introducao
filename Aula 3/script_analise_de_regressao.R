rm(list = ls())

#Vamos trabalhar com os dados do V-Dem (v-dem.net)
load("vdem1.RData")

# Vamos estimar um modelo para prever quais os fatores que influenciam na proporção de mulheres no legislativo. 
# Nossa principal hipótese é de que isso é predito por cotas que determinam participação mínima
## nas câmaras baixas
# mu_sociedc (índice de participação de mulheres na sociedade civil)


names(vdem1)
#=======================================================================
# Variáveis
#==========================================================================
# Variável Dependente
# mu_legist (proporção de mulheres no legislativo)


##Variável independentes
# pib_capit (PIB per capita)
# media_educ (escolaridade média)
# desigua (desigualdade econômica)
# democra (democracia)

vdem1 <- vdem1 %>% 
  rename(mu_legist = v2lgfemleg,
         cota_leg = v2lgqugen,
         media_educ = e_peaveduc,
         pib_capit = e_migdppcln,
         democra = e_chga_demo,
         desigua = v2xeg_eqdr ,
         mu_sociedc = v2x_gencs)
#===========================================================================
# Análise exploratória
#============================================================================

# Antes de rodar o modelo, vamos conhecer nossos dados.
# 1) Verificar a classe das variáveis. VD precisa ser contínua
glimpse(vdem1)

# 2) Estatística descritiva
summary(vdem1) # A variável dependente varia? E a independente?
# Temos alguns NAs (missings) na VD e na VI

#Extraindo NAs
vdem2 <- vdem1 %>% 
  filter(!is.na(mu_legist) & !is.na(pib_capit))

#===================================================================
# Relação linear entre os parâmetros
#===================================================================
#vamos observar se há de fato uma relação linear entre a Variável dependente e Variável independente

cor(vdem2$mu_legist, vdem2$mu_sociedc, use = "complete.obs")
cor(vdem2$mu_legist, vdem2$cota_leg, use = "complete.obs")


#==================================================================
# Outliers
#=================================================================


# Vamos observar se há outliers (valores extremos que enviesam a estimação)

#Outliers enviesam a análise, pois superestimam ou sobrestimam as médias condicionais.

library(ggplot2)

vdem2 %>% 
ggplot(aes(y = mu_legist, x = mu_sociedc)) +
  stat_regline_equation(
    formula = y ~ x,
    label.x.npc = "right",
    label.y.npc = "top",
    output.type = "expression") +
  geom_point() +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
  geom_smooth(method = "lm") +
  ylab("Proporção de mulheres no legislativo (%)") +
  xlab("Índice de participação das mulheres na sociedade civil")


boxplot(vdem2$mu_legist)

# A gente observa dois pontos que são muito extremos
# Há diferentes formas de lidar com outliers; aqui vamos, simplesmente exclui-los.

vdem2 %>% 
  arrange(desc(mu_legist)) %>% 
  select(mu_legist) %>% 
  head(n = 10)


#extraindo o outlier
vdem3 <- vdem2 %>% 
  filter(mu_legist <= 56.25)

hist(vdem2$mu_legist)

vdem3 %>% 
  ggplot(aes(y = mu_legist, x = mu_sociedc)) +
  stat_regline_equation(
    formula = y ~ x,
    label.x.npc = "right",
    label.y.npc = "top",
    output.type = "expression")+
  geom_point() +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
  geom_smooth(method = "lm") +
  ylab("Proporção de mulheres no legislativo (%)") +
  xlab("Índice de participação das mulheres na sociedade civil")

# Se fizermos uma transformação em mu_legist, vemos que há uma aproximação da reta
vdem3 <- vdem3 %>% 
  mutate(log_legist = log10(mu_legist + 10))

vdem3 %>% 
  ggplot(aes(y = log_legist, x = mu_sociedc)) +
  stat_regline_equation(
    formula = y ~ x,
    label.x.npc = "right",
    label.y.npc = "top",
    output.type = "expression")+
  geom_point() +
  stat_cor(p.accuracy = 0.001, r.accuracy = 0.01) +
  geom_smooth(method = "lm") +
  ylab("Proporção de mulheres no legislativo (%)") +
  xlab("Índice de participação das mulheres na sociedade civil")



#=====================================================================
# Estimação do modelo
#======================================================================
m1 <- lm(data = vdem3, log_legist ~  mu_sociedc)
summary(m1)

m2 <- lm(data = vdem3, log_legist ~  mu_sociedc + democra)
summary(m2)

m3 <- lm(data = vdem3, log_legist ~  mu_sociedc + democra + cota_leg)
summary(m3)

m4 <- lm(data = vdem3, log_legist ~  mu_sociedc + cota_leg + democra + pib_capit)
summary(m4)

m5 <- lm(data = vdem3, log_legist ~  mu_sociedc + cota_leg + democra + e_migdpgro)
summary(m5)

m6 <- lm(data = vdem3, log_legist ~  mu_sociedc + cota_leg + democra + pib_capit + democra*pib_capit)
summary(m6)

hist(residuals.lm(m4))

qqnorm(m4$residuals)
qqline(m4$residuals)

shapiro.test(m3$residuals)

#hipótese nula de que os residuos são dritribuídos normalmente

#teste de homocedasticidade
plot(m4)



