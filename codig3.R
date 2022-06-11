#Em codig3 - nos restringimos a 15 bairros que representam parte sigficativa do eleitorado de 2012, mas com isso conseguimos auferir se a votação do ex-prefeito Jaílson Lima em sua derrota por pequena margem em 2004, apoiador de Gariba, foi positivamente correlacionada com o pleito de 2012. Jaílson foi o grande adversário de Hobus em 2004 naquela que é até hoje (10/06/2022) a eleição mais disuptada da história da cidade

library(readxl)
dados_gerais <- read_excel("dados_gerais.xlsx")
sum(dados_gerais$eleitorado_2012)
#38063 eleitores votantes em 2012 (votos válidos)
library(tidyverse)

dados<- read_excel("dados_para_rodar_em_codig3.xlsx")
sum(dados$eleitorado_2012)
# 33972

(33972*100)/38063 # 89,25% do eleitorado representado em codig3
rm(dados_gerais)

dados <- subset(dados, select=c(Jailson2004, Hobus2008,Gariba2012,
                                Enchente,RENDA, BAIRRO, eleitorado_2012))

cor(dados$Jailson2004, dados$Gariba2012)#0.7 boa
plot(dados$Gariba2012, dados$Jailson2004)# são 15 casos, mas parece ok
cor(dados$Enchente, dados$Gariba2012)# segue sendo negativa e não sig (ok, era esperado)
cor(dados$Jailson2004, dados$Hobus2008)#nossaaaaaaaa 0.91
plot(dados$Jailson2004, dados$Hobus2008)
#inserir Hobus e Jailson na mesmo modelo é problemático pq são correlacionados em demasia, vamos ver se isso persiste após a simulação com rnorm


sd(dados$Jailson2004)
by(dados$Jailson2004, dados$BAIRRO, mean)

set.seed(666333)



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

#correlaçoes, comparaççoes e gráficos plots
hist(dados$Gariba2012)
hist(df$Gariba2012)

cor.test(df$Gariba2012, df$Jailson2004)
plot(df$Gariba2012, df$Jailson2004)


H3 <- ggplot(df, aes(Jailson2004, Gariba2012, color=enchente))
H3 + geom_point() + ggtitle("Gariba2012 por Jailson2004 + enchente,
                            dados simulados")


# modelos de regressão - dos oficiais e dos simulados

oficial <- lm(Gariba2012 ~ Jailson2004, data=dados)
summary(oficial)
simulada <- lm(Gariba2012 ~ Jailson2004, data=df)
summary(simulada)
#jailson 2004 segue logica oposta Hobus 2008
