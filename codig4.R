#termo interativo

# manips ####
# primeiro vou fazer as manips - no fim escrevo consideraçoes e o que fiz

library(readxl)
set.seed(666333)
library(tidyverse)
dados_ <- read_excel("novos_dados_analise_3_19 bairros.xlsx")
d <- rnorm(560, mean= 56.07,sd=5.97)#Albertina # gariba
e <- rnorm(560, mean= 81.70,sd=5.21)#hobus
f <- rnorm(560, mean = 2430.43, sd=704.414)
Albertina <- data.frame(d,e,f)# d e e
Albertina$enchente <- 1
Albertina$Bairro <- "Albertina"

d <- rnorm(2518, mean=60.54, sd=5.97)#boavista
e <- rnorm(2518, mean= 74.02,sd=5.21)
f <- rnorm(2518, mean = 2430.43, sd=704.414)
BoaVista<- data.frame(d,e,f)
BoaVista$enchente <- 0
BoaVista$Bairro <- "Boa Vista"

summary(BoaVista)
summary(Albertina)
df <- full_join(Albertina, BoaVista)
summary(df)
table(df$Bairro)# deu certo

d <- rnorm(564, mean= 46.80,sd=5.97)#BarraItoup
e <- rnorm(564, mean= 81.30,sd=5.21)
f <- rnorm(564, mean = 2585.71, sd=704.414)
Barraitoup<- data.frame(d,e,f)
Barraitoup$enchente <- 1
Barraitoup$Bairro <- "Barra Itoupava"
df <- full_join(df,Barraitoup)
table(df$Bairro)#deu certo

d <- rnorm(1783, mean= 57.76,sd=5.97)#Barra do Trombudo
e <- rnorm(1783, mean= 69.08,sd=5.21)
f <- rnorm(1783, mean = 2024.38, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1 
d$Bairro <- "Barra do Trombudo"
df<- full_join(df,d) 

table(df$Bairro)

d <- rnorm(1777, mean= 62.97,sd=5.97)#Barragem
e <- rnorm(1777, mean= 74.54,sd=5.21)
f <- rnorm(1777, mean = 2177.17, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Barragem"
df <- full_join(df,d)

d <- rnorm(2220, mean= 51.35,sd=5.97)#Bela Aliança
e <- rnorm(2220, mean= 85.25,sd=5.21)
f <- rnorm(2220, mean = 2313.22, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Bela Aliança"
df <- full_join(df,d)

d <- rnorm(1903, mean= 57.59,sd=5.97)#Budag
e <- rnorm(1903, mean= 79.03,sd=5.21)
f <- rnorm(1903, mean = 2809.06, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Budag"
df <- full_join(df,d)

d <- rnorm(2967, mean= 52.54,sd=5.97)#Canta Galo
e <- rnorm(2967, mean= 78.62,sd=5.21)
f <- rnorm(2967, mean = 2929.20, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Canta Galo"
df <- full_join(df,d)

d <- rnorm(6454, mean= 49.50,sd=5.97)#Centro
e <- rnorm(6454, mean= 85.79,sd=5.21)
f <- rnorm(6454, mean = 4244.76, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Centro"
df <- full_join(df,d)

d <- rnorm(1794, mean= 46.15, sd=5.97)#Fundo Canoas
e <- rnorm(1794, mean= 85.38,sd=5.21)
f <- rnorm(1794, mean = 2759.02, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Fundo Canoas"
df <- full_join(df,d)

d <- rnorm(454, mean= 61.89, sd=5.97)#Jardim Alexandro
e <- rnorm(454, mean= 65.93,sd=5.21)
f <- rnorm(454, mean = 2853.32, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Jardim Alexandro"
df <- full_join(df,d)

d <- rnorm(3151, mean= 56.68, sd=5.97)#Laranjeiras
e <- rnorm(3151, mean= 80.67,sd=5.21)
f <- rnorm(3151, mean = 2142.43, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Laranjeiras"
df <- full_join(df,d)

d <- rnorm(2314, mean= 48.87, sd=5.97)#Progresso
e <- rnorm(2314, mean= 82.67,sd=5.21)
f <- rnorm(2314, mean = 2594.47, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Progresso"
df <- full_join(df,d)

d <- rnorm(1012, mean= 52.96, sd=5.97)#Santa Rita
e <- rnorm(1012, mean= 78.42,sd=5.21)
f <- rnorm(1012, mean = 1847.79, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Santa Rita"
df <- full_join(df,d)

d <- rnorm(2494, mean= 58.34, sd=5.97)#Santana
e <- rnorm(2494, mean= 80.00,sd=5.21)
f <- rnorm(2494, mean = 3237.88, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Santana"
df <- full_join(df,d)

d <- rnorm(2072, mean= 53.18, sd=5.97)#Sumaré
e <- rnorm(2072, mean= 84.35,sd=5.21)
f <- rnorm(2072, mean = 4485.69, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 0
d$Bairro <- "Sumaré"
df <- full_join(df,d)

d <- rnorm(628, mean= 56.21, sd=5.97)#Taboão
e <- rnorm(628, mean= 79.56,sd=5.21)
f <- rnorm(628, mean = 2688.64, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Taboão"
df <- full_join(df,d)

d <- rnorm(410, mean= 37.80, sd=5.97)#Valada Itoupava
e <- rnorm(410, mean= 83.50,sd=5.21)
f <- rnorm(410, mean = 2675.85, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Valada Itoupava"
df <- full_join(df,d)

d <- rnorm(885, mean= 50.39, sd=5.97)#Valada São Paulo
e <- rnorm(885, mean= 78.97,sd=5.21)
f <- rnorm(885, mean = 1781.26, sd=704.414)
d<- data.frame(d,e,f)
d$enchente <- 1
d$Bairro <- "Valada São Paulo"
df <- full_join(df,d)

df$Gariba2012 <- df$d
df$Hobus2008 <- df$e
df$renda_simulada <- df$f
df$Bairro <- as.factor(df$Bairro)
df <- subset(df, select=c(Bairro, Gariba2012, Hobus2008, renda_simulada,
                          enchente))

dados_ <- subset(dados_, select=c(Bairro,Hobus2008, Gariba2012,eleitorado_2012,
                                  enchente,renda2010))

rm(d, Albertina, Barraitoup, BoaVista, d,e,f)







# agora ######

#df é o dado simulado igual em codig
# dados_ são os dados oficiais de 21 bairros

# agora criar faixas de renda e interagir com atingimento enchente

mean(df$renda_simulada)
mean(dados_$renda2010)


# com dois níveis de renda
dados_$faixa_renda <- ntile(dados_$renda2010, 2)
dados_$faixa_renda <- as.factor(dados_$faixa_renda)
levels(dados_$faixa_renda)
levels(dados_$faixa_renda) <- c('baixa','alta')

dados_$enchente <- as.factor(dados_$enchente)
levels(dados_$enchente)
levels(dados_$enchente) <- c('Não','Sim')

summary(dados_$enchente)

oficial <- lm(Gariba2012 ~ faixa_renda + enchente + faixa_renda*enchente,
              data=dados_)
summary(oficial)

df$faixa_renda_simulada <- ntile(df$renda_simulada, 2)
df$faixa_renda_simulada <- as.factor(df$faixa_renda_simulada)
levels(df$faixa_renda_simulada)
levels(df$faixa_renda_simulada) <- c('baixa','alta')

df$enchente <- as.factor(df$enchente)
levels(df$enchente)
levels(df$enchente) <- c('Não','Sim')

simulado <- lm(Gariba2012 ~ faixa_renda_simulada +
                 enchente + faixa_renda_simulada*enchente,
              data=df)
summary(simulado)


# com três níveis de renda
dados_$faixa_renda <- ntile(dados_$renda2010, 3)
dados_$faixa_renda <- as.factor(dados_$faixa_renda)
levels(dados_$faixa_renda)
levels(dados_$faixa_renda) <- c('baixa','média','alta')

dados_$enchente <- as.factor(dados_$enchente)
levels(dados_$enchente)
levels(dados_$enchente) <- c('Não','Sim')

summary(dados_$enchente)

oficial <- lm(Gariba2012 ~ faixa_renda + enchente + faixa_renda*enchente,
              data=dados_)
summary(oficial)

df$faixa_renda_simulada <- ntile(df$renda_simulada, 3)
df$faixa_renda_simulada <- as.factor(df$faixa_renda_simulada)
levels(df$faixa_renda_simulada)
levels(df$faixa_renda_simulada) <- c('baixa','média','alta')

df$enchente <- as.factor(df$enchente)
levels(df$enchente)
levels(df$enchente) <- c('Não','Sim')

simulado <- lm(Gariba2012 ~ faixa_renda_simulada +
                 enchente + faixa_renda_simulada*enchente,
               data=df)
summary(simulado)


#com 4

df$faixa_renda_simulada <- ntile(df$renda_simulada, 4)
df$faixa_renda_simulada <- as.factor(df$faixa_renda_simulada)
levels(df$faixa_renda_simulada)
levels(df$faixa_renda_simulada) <- c('baixa','média baixa','média alta','alta')

df$enchente <- as.factor(df$enchente)
levels(df$enchente)
levels(df$enchente) <- c('Não','Sim')

simulado <- lm(Gariba2012 ~ faixa_renda_simulada +
                 enchente + faixa_renda_simulada*enchente,
               data=df)
summary(simulado)

library(ggplot2)
g2 <- ggplot(df, aes(enchente,Gariba2012))
g2 <- g2+ geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="point",
               shape=24, color="blue",
               fill="blue")
g2 + facet_wrap(.~faixa_renda_simulada)


# só entre os que pegaram enchente
df_e <- df%>%
  filter(enchente == "Sim")

simulado_enchentados <- lm(Gariba2012 ~ faixa_renda_simulada,
               data=df_e)
summary(simulado_enchentados)#ok

g2 <- ggplot(df, aes(faixa_renda_simulada,Gariba2012))
g2 <- g2+ geom_boxplot(alpha=0.7) +
  stat_summary(fun=mean, geom="number",
               shape=24, color="blue",
               fill="blue")
g2 + facet_wrap(.~enchente)

df$faixa_renda_simulada <- relevel(df$faixa_renda_simulada,
                                   "alta")
df$enchente<- relevel(df$enchente,
                                   "Não")
simulado22 <- lm(Gariba2012 ~ faixa_renda_simulada +
                 enchente + faixa_renda_simulada*enchente,
               data=df)
summary(simulado22)
