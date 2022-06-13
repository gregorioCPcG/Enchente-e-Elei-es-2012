


# em codig 6 eu faço o mesmo que em codig 5 (só que com os dados de Jailson 2004 e nao com os de Hobus2008)
# e com os dados de cerca de 89% do eleitorado e nao de 99,30%



# manips ############
library(tidyverse)
set.seed(666333)
library(readxl)
dados<- read_excel("dados_para_rodar_em_codig3.xlsx")


d <- rnorm(1783, mean= 57.76,sd=5.97)#Barra do Trombudo
e <- rnorm(1783, mean= 69.08,sd=5.21)
f <- rnorm(1783, mean = 2024.38, sd=704.414)
g <- rnorm(1783, mean=64.89, sd=5.823166)#Jailson
df<- data.frame(d,e,f,g)
df$enchente <- 1 
df$Bairro <- "Barra do Trombudo"




d <- rnorm(1777, mean= 62.97,sd=5.97)#Barragem
e <- rnorm(1777, mean= 74.54,sd=5.21)
f <- rnorm(1777, mean = 2177.17, sd=704.414)
g <- rnorm(1777, mean=57.68, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Barragem"
df <- full_join(df,d)

table(df$Bairro)# deu certo?
###


d <- rnorm(2220, mean= 51.35,sd=5.97)#Bela Aliança
e <- rnorm(2220, mean= 85.25,sd=5.21)
f <- rnorm(2220, mean = 2313.22, sd=704.414)
g <- rnorm(2220, mean=44.91, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Bela Aliança"
df <- full_join(df,d)

d <- rnorm(2518, mean=60.54, sd=5.97)#boavista
e <- rnorm(2518, mean= 74.02,sd=5.21)
f <- rnorm(2518, mean = 2430.43, sd=704.414)
g <- rnorm(2518, mean=55.79, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 0
d$Bairro <- "Boa Vista"
df <- full_join(df,d)

table(df$Bairro)

d <- rnorm(1903, mean= 57.59,sd=5.97)#Budag
e <- rnorm(1903, mean= 79.03,sd=5.21)
f <- rnorm(1903, mean = 2809.06, sd=704.414)
g <- rnorm(1903, mean=52.64, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Budag"
df <- full_join(df,d)

d <- rnorm(2967, mean= 52.54,sd=5.97)#Canta Galo
e <- rnorm(2967, mean= 78.62,sd=5.21)
f <- rnorm(2967, mean = 2929.20, sd=704.414)
g <- rnorm(2967, mean=49.25, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Canta Galo"
df <- full_join(df,d)

d <- rnorm(6454, mean= 49.50,sd=5.97)#Centro
e <- rnorm(6454, mean= 85.79,sd=5.21)
f <- rnorm(6454, mean = 4244.76, sd=704.414)
g <- rnorm(6454, mean=41.49, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Centro"
df <- full_join(df,d)

d <- rnorm(1794, mean= 46.15, sd=5.97)#Fundo Canoas
e <- rnorm(1794, mean= 85.38,sd=5.21)
f <- rnorm(1794, mean = 2759.02, sd=704.414)
g <- rnorm(1794, mean=46.68, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Fundo Canoas"
df <- full_join(df,d)


d <- rnorm(3151, mean= 56.68, sd=5.97)#Laranjeiras
e <- rnorm(3151, mean= 80.67,sd=5.21)
f <- rnorm(3151, mean = 2142.43, sd=704.414)
g <- rnorm(3151, mean=54.29, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Laranjeiras"
df <- full_join(df,d)

d <- rnorm(2314, mean= 48.87, sd=5.97)#Progresso
e <- rnorm(2314, mean= 82.67,sd=5.21)
f <- rnorm(2314, mean = 2594.47, sd=704.414)
g <- rnorm(2314, mean=46.31, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Progresso"
df <- full_join(df,d)

d <- rnorm(1012, mean= 52.96, sd=5.97)#Santa Rita
e <- rnorm(1012, mean= 78.42,sd=5.21)
f <- rnorm(1012, mean = 1847.79, sd=704.414)
g <- rnorm(1012, mean=56.29, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Santa Rita"
df <- full_join(df,d)

d <- rnorm(2494, mean= 58.34, sd=5.97)#Santana
e <- rnorm(2494, mean= 80.00,sd=5.21)
f <- rnorm(2494, mean = 3237.88, sd=704.414)
g <- rnorm(2494, mean=52.84, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Santana"
df <- full_join(df,d)

d <- rnorm(2072, mean= 53.18, sd=5.97)#Sumaré
e <- rnorm(2072, mean= 84.35,sd=5.21)
f <- rnorm(2072, mean = 4485.69, sd=704.414)
g <- rnorm(2072, mean=49.47, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 0
d$Bairro <- "Sumaré"
df <- full_join(df,d)

d <- rnorm(628, mean= 56.21, sd=5.97)#Taboão
e <- rnorm(628, mean= 79.56,sd=5.21)
f <- rnorm(628, mean = 2688.64, sd=704.414)
g <- rnorm(628, mean=52.14, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Taboão"
df <- full_join(df,d)


d <- rnorm(885, mean= 50.39, sd=5.97)#Valada São Paulo
e <- rnorm(885, mean= 78.97,sd=5.21)
f <- rnorm(885, mean = 1781.26, sd=704.414)
g <- rnorm(885, mean=53.53, sd=5.823166)#Jailson
d<- data.frame(d,e,f,g)
d$enchente <- 1
d$Bairro <- "Valada São Paulo"
df <- full_join(df,d)


#verificar se deu certo:
summary(as.factor(df$Bairro))
summary(as.factor(df$enchente))

#medias comparar
mean(dados$Gariba2012)
mean(df$d)


mean(dados$Hobus2008)
mean(df$e)

mean(dados$RENDA)
mean(df$f)

mean(dados$Jailson2004)
mean(df$g)

#recods
df$Gariba2012 <- df$d
df$Hobus2008 <- df$e
df$renda_simulada <- df$f
df$Jailson2004 <- df$g
df$Bairro <- as.factor(df$Bairro)
#select
df <- subset(df, select=c(Bairro, Gariba2012, Hobus2008, renda_simulada,
                          enchente, Jailson2004))
summary(df)#simulado
summary(dados)#oficiais

df$QuartisJailson2004 <- ntile(df$Jailson2004, 4)
df$QuartisJailson2004<- as.factor(df$QuartisJailson2004)
levels(df$QuartisJailson2004)
levels(df$QuartisJailson2004) <- c('1','2',
                                 '3','4')

df$enchente <- as.factor(df$enchente)
levels(df$enchente)
levels(df$enchente) <- c('Não','Sim')

#### regressao #######

simulado13 <- lm(Gariba2012 ~ QuartisJailson2004 + enchente +
                   QuartisJailson2004*enchente, data=df)
summary(simulado13)

