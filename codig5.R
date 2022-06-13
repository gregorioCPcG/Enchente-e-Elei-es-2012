# codign5 os mesmos bairros de codig2 - só que com faixas (quartis de Hobus 2008)
# mesma estratégia de codig4, com dados de codig2
# em codig2 trabalhamos com 20 dos 21, lá eu nao detalhei quantos pro centro isso é do total (no codig 3, eu o fiz
library(readxl)
library(tidyverse)

hob <- read_excel("novos_dados_analise_2.xlsx")
dados_gerais <- read_excel("dados_gerais.xlsx")

sum(hob$eleitorado_2012)
sum(dados_gerais$eleitorado_2012)
(37800*100)/38063
# 99.30% do total

# now recods os mesmos de codig 2

set.seed(666333)
d <- rnorm(560, mean= 56.07,sd=5.97)#Albertina # gariba
e <- rnorm(560, mean= 81.70,sd=5.21)#hobus
Albertina <- data.frame(d,e)# d e e
Albertina$enchente <- 1
Albertina$Bairro <- "Albertina"

d <- rnorm(2518, mean=60.54, sd=5.97)#boavista
e <- rnorm(2518, mean= 74.02,sd=5.21)
BoaVista<- data.frame(d,e)
BoaVista$enchente <- 0
BoaVista$Bairro <- "Boa Vista"

summary(BoaVista)
summary(Albertina)
df <- full_join(Albertina, BoaVista)
summary(df)
table(df$Bairro)# deu certo

d <- rnorm(564, mean= 46.80,sd=5.97)#BarraItoup
e <- rnorm(564, mean= 81.30,sd=5.21)
Barraitoup<- data.frame(d,e)
Barraitoup$enchente <- 1
Barraitoup$Bairro <- "Barra Itoupava"
df <- full_join(df,Barraitoup)
table(df$Bairro)#deu certo

d <- rnorm(1783, mean= 57.76,sd=5.97)#Barra do Trombudo
e <- rnorm(1783, mean= 69.08,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1 
d$Bairro <- "Barra do Trombudo"
df<- full_join(df,d) 

table(df$Bairro)

d <- rnorm(1840, mean= 58.31,sd=5.97)#Barra Taboão
e <- rnorm(1840, mean= 80.45,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Barra Taboão"
df <- full_join(df,d)


d <- rnorm(1777, mean= 62.97,sd=5.97)#Barragem
e <- rnorm(1777, mean= 74.54,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Barragem"
df <- full_join(df,d)

d <- rnorm(2220, mean= 51.35,sd=5.97)#Bela Aliança
e <- rnorm(2220, mean= 85.25,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Bela Aliança"
df <- full_join(df,d)

d <- rnorm(1903, mean= 57.59,sd=5.97)#Budag
e <- rnorm(1903, mean= 79.03,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Budag"
df <- full_join(df,d)

d <- rnorm(2967, mean= 52.54,sd=5.97)#Canta Galo
e <- rnorm(2967, mean= 78.62,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Canta Galo"
df <- full_join(df,d)

d <- rnorm(6454, mean= 49.50,sd=5.97)#Centro
e <- rnorm(6454, mean= 85.79,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Centro"
df <- full_join(df,d)

d <- rnorm(1794, mean= 46.15, sd=5.97)#Fundo Canoas
e <- rnorm(1794, mean= 85.38,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Fundo Canoas"
df <- full_join(df,d)

d <- rnorm(454, mean= 61.89, sd=5.97)#Jardim Alexandro
e <- rnorm(454, mean= 65.93,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Jardim Alexandro"
df <- full_join(df,d)

d <- rnorm(3151, mean= 56.68, sd=5.97)#Laranjeiras
e <- rnorm(3151, mean= 80.67,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Laranjeiras"
df <- full_join(df,d)

d <- rnorm(2314, mean= 48.87, sd=5.97)#Progresso
e <- rnorm(2314, mean= 82.67,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Progresso"
df <- full_join(df,d)

d <- rnorm(1012, mean= 52.96, sd=5.97)#Santa Rita
e <- rnorm(1012, mean= 78.42,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Santa Rita"
df <- full_join(df,d)

d <- rnorm(2494, mean= 58.34, sd=5.97)#Santana
e <- rnorm(2494, mean= 80.00,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Santana"
df <- full_join(df,d)

d <- rnorm(2072, mean= 53.18, sd=5.97)#Sumaré
e <- rnorm(2072, mean= 84.35,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 0
d$Bairro <- "Sumaré"
df <- full_join(df,d)

d <- rnorm(628, mean= 56.21, sd=5.97)#Taboão
e <- rnorm(628, mean= 79.56,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Taboão"
df <- full_join(df,d)

d <- rnorm(410, mean= 37.80, sd=5.97)#Valada Itoupava
e <- rnorm(410, mean= 83.50,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Valada Itoupava"
df <- full_join(df,d)

d <- rnorm(885, mean= 50.39, sd=5.97)#Valada São Paulo
e <- rnorm(885, mean= 78.97,sd=5.21)
d<- data.frame(d,e)
d$enchente <- 1
d$Bairro <- "Valada São Paulo"
df <- full_join(df,d)


#verificar se deu certo:
summary(as.factor(df$Bairro))
summary(as.factor(df$enchente))
by(df$d, df$Bairro, mean)
mean(df$d)
mean(hob$Gariba2012)
by(df$e, df$Bairro, mean)
mean(df$e)
mean(hob$Hobus2008)
df$Gariba2012 <- df$d
df$Hobus2008 <- df$e
df$Bairro <- as.factor(df$Bairro)
df <- subset(df, select=c(Bairro, Gariba2012, Hobus2008, enchente))

# ok agora trabalhar com dois fatores explicativos para Gariba
# enchente 
# quartis de votação para Hobus 2008
# enchente interagindo com esses quartis

df$QuartisHobus2008 <- ntile(df$Hobus2008, 4)
df$QuartisHobus2008<- as.factor(df$QuartisHobus2008)
levels(df$QuartisHobus2008)
levels(df$QuartisHobus2008) <- c('1','2',
                                 '3','4')

df$enchente <- as.factor(df$enchente)
levels(df$enchente)
levels(df$enchente) <- c('Não','Sim')

summary(df)

simulado55 <- lm(Gariba2012 ~ QuartisHobus2008 +
                 enchente + QuartisHobus2008*enchente,
               data=df)
summary(simulado55)

library(huxtable)
huxreg(simulado55)

# Vou colocar bonitinho no arquivo Codig5apresentacao.pdf a análise e outras cositas mais