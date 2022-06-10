library(readxl)

hob <- read_excel("novos_dados_analise_2.xlsx")
cor(hob$J.Teixeira2012, hob$Hobus2008)# apoio Teixeira e tem correlação os bairros mais votados de um, foram os mais votados de outro (2008-2012)
cor(hob$Gariba2012, hob$Hobus2008)# se relaciona negativamente com o voto em Gariba
# aqui q tá a jogada


# vou repetir a operação do arquivo codig.R
# por isso nao demanda explicar a metodologia
# vou acrescer a variável Hobus2004 para a técnica rnorm que fiz em codig.R

# antes, porém cabem algumas palavras sobre o pleito
# em 2008, Milton foi oficialmente eleito com 100% dos votos, seu opositor foi impugnado e os votos para o mesmo foram considerados nulos, aqui partimos do pressuposto que a distrbuição dos votos de Milton seria dada sem essa impugnação. Lembremos que acima viu-se que essa estatística correlaciona positivamente com Teixeira e negativamente com Gariba.

mod23 <- lm(Gariba2012 ~ Hobus2008 + enchente + eleitorado_2012, data=hob)
summary(mod23)# essa é a expectativa, mas pode ser que controlando..

#com a mudança de bairros no TSE, perdemos o bairro de navegantes em relação à analise efetuada em codig.R

by(hob$Hobus2008, hob$Bairro, mean)
sd(hob$Hobus2008)

#d vai virar gariba2012
#e vai virar Hobus2008
#f vai virar renda2010
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
summary(d)
by(df$d, df$Bairro, mean)
mean(df$d)
mean(hob$Gariba2012)
summary(e)
by(df$e, df$Bairro, mean)
mean(df$e)
mean(hob$Hobus2008)
df$Gariba2012 <- df$d
df$Hobus2008 <- df$e
df$Bairro <- as.factor(df$Bairro)
df <- subset(df, select=c(Bairro, Gariba2012, Hobus2008, enchente))


# correlações
cor.test(hob$Hobus2008, hob$Gariba2012)
plot(hob$Hobus2008, hob$Gariba2012)
cor.test(df$Hobus2008, df$Gariba2012)
plot(df$Hobus2008, df$Gariba2012)


# regressões
options(scipen = 999)
summary(mod23)
mod4 <- lm(Gariba2012 ~ Hobus2008 + enchente, data=df)
summary(mod4)
# HOBUS REDUZ 0,32 E ENCEHNTE SEGUE REDUZINDO AGORA 2,95


# MODELOS ADICIONAIS
df$Bairro <- relevel(df$Bairro, "Valada Itoupava")
mod5 <- lm(Gariba2012 ~ Hobus2008 + Bairro, data=df)# hobus perde significância, mostrando que a vitporia deve ter outras explicaçoes
summary(mod5)

mod66 <- lm(Gariba2012 ~ Hobus2008 + Bairro + Bairro*Hobus2008, data=df)#
summary(mod66)


cor.test(df$Gariba2012, df$enchente)# fraco
cor.test(df$Hobus2008, df$enchente)# fraco

mod666 <- lm(Gariba2012 ~ Hobus2008, data=df)#
summary(mod666)


# renda vamos acrescer renda à analise 
# perdemos mais um bairro, mas seguimos com dezenove(o motivo é que no censo nao consta a mesma classificao do TSE)
rm(list = ls())
hob <- read_excel("novos_dados_analise_2.xlsx")
rend <- read_excel("novos_dados_analise_3_19 bairros.xlsx")
hob$Bairro
rend$Bairro
# perdemos Barra Taboão e Navegantes, bairros pouco populosos. seguimos com os dezenove
# vamos fazer uma imputação de renda
# por exemplo se a renda média é x, vamos supor que o cara ganhe x simulado pelo rnorm pelo desvio padrão de renda no banco novo (rend)


cor(rend$Gariba2012, rend$renda2010)# sem relação esperada, mas talvez controlando com as outras, dê alguma coisa
mod24 <- lm(Gariba2012 ~ renda2010 + Hobus2008 + enchente, data=rend)
summary(mod24)# guardar pra depois  # renda sem efeito, enchente idem, hobus segue dando efeito

sd(rend$renda2010)# desvio padrão renda dos bairros de 2010(censo IBGE)
by(rend$renda2010, rend$Bairro, mean)
#d vai virar gariba2012
#e vai virar Hobus2008
#f vai virar renda2010
set.seed(666333)
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


#verificar se deu certo:
summary(as.factor(df$Bairro))
summary(as.factor(df$enchente))
mean(df$d)
mean(df$e)
mean(hob$Hobus2008)
mean(rend$renda2010)
mean(df$f)
df$Gariba2012 <- df$d
df$Hobus2008 <- df$e
df$renda_simulada <- df$f
df$Bairro <- as.factor(df$Bairro)
df <- subset(df, select=c(Bairro, Gariba2012, Hobus2008, renda_simulada, enchente))

library(ggplot2)

H1 <- ggplot(df, aes(Hobus2008, Gariba2012, color=enchente))
H1 + geom_point() + ggtitle("Gariba2012 por Hobus2008 + enchente,
                            dados simulados")

# não há uma relação visível

H2 <- ggplot(df, aes(renda_simulada, Gariba2012, color=enchente))
H2 + geom_point() + ggtitle("Gariba2012 por renda familiar 2010 + enchente,
                            dados simulados")

# a enchente binária parece ter relação com renda, mas não com a votação em Gariba

# 

# correlações
require(corrplot)
num_oficial <- subset(rend, select=c(Gariba2012, renda2010,Hobus2008,enchente))
num_simulada <- subset(df, select=c(Gariba2012, Hobus2008, renda_simulada, enchente)) 


matcor1 <- cor(num_oficial)
corrplot(matcor1, method="circle")# oficial

matcor2 <- cor(num_simulada)
corrplot(matcor2, method="circle")#simualdo

num_oficial2 <- subset(num_oficial, select=c(Gariba2012, renda2010,Hobus2008,
                                    enchente))
num_simulada2 <- subset(num_simulada, select=c(Gariba2012, Hobus2008, renda_simulada, enchente))
num_simulada2$enchente <- as.factor(num_simulada2$enchente)
num_oficial2$enchente <- as.factor(num_oficial2$enchente)
library(GGally)
ggpairs(num_oficial2, title="Comparando dados oficiais")
ggpairs(num_simulada2, title="Comparando dados simulados")


# modelos
summary(mod24)#dados oficiais

mod44 <- lm(Gariba2012 ~ Hobus2008 + enchente + renda_simulada, data=df)
summary(mod44)
# HOBUS REDUZ e ENCEHNTE SEGUE REDUZINDO AGORA 2,95_ renda e encehnte só são sigs pelo aumento de N

# MODELOS ADICIONAIS
df$Bairro <- relevel(df$Bairro, "Valada Itoupava")
mod54 <- lm(Gariba2012 ~ Hobus2008 + Bairro + renda_simulada, data=df)# hobus perde significância, mostrando que a vitporia deve ter outras explicaçoes
summary(mod54)# Hobus perde significância

# conclusão suspeitamos que Hobus seja a melhro variável mas o modelo 54 nos faz suspeitar que ainda há varios fatos a serem descobertos sobre a real causa da eleição de Gariba, a enchente (ao menos na sua forma binarizada) ao que parece não consegue ser suficientemente convincente quanto a sua carga explicativa. 
