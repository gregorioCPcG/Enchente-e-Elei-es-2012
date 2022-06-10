# enchendo rnorm _ colocar bairro por pop

#Gariba vice prefeito eleito em 2008 , rompeu com o então prefeito Hobus durante o mandato
# A enchente de 2011 foi devastadora e Gariba foi apontado no senso comum por ter se destacado
# já Hobus foi alvo de críticas por sua ação à epóca, o que o motivou a mudar sua imagem se inserindo na defesa civil estadual poucos anos depois.

# dizia-se à epóca que Gariba foi beneficiado pela destacada atuaçção na enchente de 2011
# Do concorrrente(jorge teixeira), apoiado pelo prefeito Hobus, esperava-se o oposto.

# é isso que vamos veriricar

# dois são os limitadores :
# os dados (apenas TSE para votos) e enchente - binário - sim(1) ou não(2)

# logo, o uso de dados por rua, ou uma medida mais exata da enchente 

# outro limitador é o tempo (meu e a distância no tempo quando escrevo em 2022, o que 
#inviabiliza uma pesquisa nova de amostragem)


library(readxl)
dados_gerais <- read_excel("dados_gerais.xlsx")
summary(dados_gerais)
library(tidyverse)
library(knitr)
library(kableExtra)
b5 <- dados_gerais %>% 
  dplyr::select(Bairro, eleitorado_2012, Gariba2012,enchente) %>% 
  arrange(Bairro)
b5 %>%
  kbl(caption = "") %>%
  kable_classic(full_width = F, html_font = "Garamond")

sd(dados_gerais$Gariba2012)#sd da formula, o desvio padrão dos 21 bairros votação de Gariba

# rrnom é pra criar bases de dados com populacao dos bairros
# o valor que gerará será a prob de votar em Gariba
#formula:
# rnomr(votantes em 2012 daquele bairro, média = votação daquele bairro, sd = 5.97 [o desvio padrão da amostra com dados dos bairros])

d <- rnorm(560, mean= 56.07,sd=5.97)#Albertina
Albertina <- data.frame(d)
Albertina$enchente <- 1
Albertina$Bairro <- "Albertina"

summary(Albertina)# na allbertina , Gariba recebeu 56,07 - simula-se que a chance de:
# um albertinense aleatório de votar em Gariba seja entre 38,04 e 74,50
# é um exercício para amplificar a base
# enchente(1) é se o Bairro foi afetado('1') ou não(valor '0') pelas cheias de 2011

d <- rnorm(2518, mean=60.54, sd=5.97)#boavista
#o mesmo será feito com todos os bairros_ boa vista não foi afetada por isso enchente é 0
BoaVista<- data.frame(d)
BoaVista$enchente <- 0
BoaVista$Bairro <- "Boa Vista"

summary(BoaVista)
summary(Albertina)
df <- full_join(Albertina, BoaVista)
summary(df)
table(df$Bairro)# deu certo

d <- rnorm(564, mean= 46.80,sd=5.97)#BarraItoup
Barraitoup<- data.frame(d)
Barraitoup$enchente <- 1
Barraitoup$Bairro <- "Barra Itoupava"
df <- full_join(df,Barraitoup)
table(df$Bairro)#deu certo

#agora por numero consultar objeto b5(o 1(Albertina),
#o 2(Barra Itoup), e o 7(Boa vissta) já foram) 
#e faltam 3,4,5,6,8,9,10,11,..... até 21



d <- rnorm(1783, mean= 57.76,sd=5.97)#Barra do Trombudo
d<- data.frame(d)#cria aq base do bairro
d$enchente <- 1 # 1 se foi afetado, 0 se não foi
d$Bairro <- "Barra do Trombudo"#nome do bairro - cria a categórica
df<- full_join(df,d) #juntando dado para ir completando até ter todos

table(df$Bairro)

d <- rnorm(1840, mean= 58.31,sd=5.97)#Barra Taboão
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Barra Taboão"
df <- full_join(df,d)


d <- rnorm(1777, mean= 62.97,sd=5.97)#Barragem
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Barragem"
df <- full_join(df,d)

d <- rnorm(2220, mean= 51.35,sd=5.97)#Bela Aliança
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Bela Aliança"
df <- full_join(df,d)

d <- rnorm(1903, mean= 57.59,sd=5.97)#Budag
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Budag"
df <- full_join(df,d)

d <- rnorm(2967, mean= 52.54,sd=5.97)#Canta Galo
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Canta Galo"
df <- full_join(df,d)

d <- rnorm(6454, mean= 49.50,sd=5.97)#Centro
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Centro"
df <- full_join(df,d)

d <- rnorm(1794, mean= 46.15, sd=5.97)#Fundo Canoas
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Fundo Canoas"
df <- full_join(df,d)

d <- rnorm(454, mean= 61.89, sd=5.97)#Jardim Alexandro
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Jardim Alexandro"
df <- full_join(df,d)

d <- rnorm(3151, mean= 56.68, sd=5.97)#Laranjeiras
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Laranjeiras"
df <- full_join(df,d)

d <- rnorm(263, mean= 52.47, sd=5.97)#Navegantes
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Navegantes"
df <- full_join(df,d)

d <- rnorm(2314, mean= 48.87, sd=5.97)#Progresso
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Progresso"
df <- full_join(df,d)

d <- rnorm(1012, mean= 52.96, sd=5.97)#Santa Rita
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Santa Rita"
df <- full_join(df,d)

d <- rnorm(2494, mean= 58.34, sd=5.97)#Santana
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Santana"
df <- full_join(df,d)

d <- rnorm(2072, mean= 53.18, sd=5.97)#Sumaré
d<- data.frame(d)
d$enchente <- 0
d$Bairro <- "Sumaré"
df <- full_join(df,d)

d <- rnorm(628, mean= 56.21, sd=5.97)#Taboão
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Taboão"
df <- full_join(df,d)

d <- rnorm(410, mean= 37.80, sd=5.97)#Valada Itoupava
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Valada Itoupava"
df <- full_join(df,d)

d <- rnorm(885, mean= 50.39, sd=5.97)#Valada São Paulo
d<- data.frame(d)
d$enchente <- 1
d$Bairro <- "Valada São Paulo"
df <- full_join(df,d)


#verificar se deu certo:
summary(as.factor(df$Bairro))
summary(as.factor(df$enchente))
summary(d)
by(df$d, df$Bairro, mean)
mean(df$d)
mean(dados_gerais$Gariba2012)
df$Gariba2012 <- df$d
df$Bairro <- as.factor(df$Bairro)
df <- subset(df, select=c(Bairro, Gariba2012,enchente))
#obs o ideal seria fazer uma pesquisa, mas já passou muito tempo.
# isso daqui é só um exercício, nao é possível afirmar nada. Mas pode-se supor que esse fato tenha ocorrido
# um dado da epoca de pesquisas internas poderiam aclarar essa dimensão
# outra classificação de enchente, mensurando pelo nível de impacto de cada bairro(não 0 ou 1)
# ou até gerar dados por local de votação (zona eleitoral)
# são possibilidades

# feito essas ressalvas, abaixo segue análises de correlação entre o bairro ter ou nao enchente (0 ou 1) e a votação em Gariba.

mod <- lm(Gariba2012 ~ enchente, data=df)#regressão simples dados simulados
summary(mod)#
df$Bairro <- relevel(df$Bairro, "Valada Itoupava")
library(sjPlot)
tab_model(mod)
# aqui nota-se como teve o efeito contrário, ter sido de um bairro que sofreu reduz (o modelo estima) cerca de 3.84 % (na verdade entre 4.08 e 3.61)
# é legal usar o intercept para fica mais claro ainda essa estimação
# O bairro sem enchente tem valores estimados de 57.12 e 57.56 %
# os com enchente entre 53.26 e 53.73%


mod2 <- lm(Gariba2012 ~ Bairro, data =df)# referencia valada Itoupava
summary(mod2) # aqui só pra conferencia

mod_21 <- lm(Gariba2012 ~ enchente, data=dados_gerais)#r
summary(mod_21)#
mod_22 <- lm(Gariba2012 ~ enchente + eleitorado_2012, data=dados_gerais)#
summary(mod_22)#

# O EFEITO É SIMILAR AO COM A BASE SÓ DE BAIRROS (21 CASOS), NOTA-SE QUE A SIGNIFICÂNCIA É OBTIDA AUMENTANDO O N DE 21 PARA 38 MIL. MAS ISSO NÃO INDICA CAUSALIDADE É APENAS UM EXERCÍCIO PARA MOSTRAR QUE AS COMPLEXIDADES ELEITORAIS VÃO ALÉM DO SENSO COMUM, ALÉM É CLARO DE TRAZER À TONA AS DIFICULDADES DE LIDAR COM DADOS FRÁGEIS DO PASSADO. 
# UM LEVANTAMENTO FEITO A EPÓCA PERGUNTANDO À UMA AMOSTRA REPRESENTATIVA SE FOI AFETADO PELA ENCHENTE E EM QUEM VOTOU SERIA ÚTIL(ALÉM DE OUTRAS QUESTÕES ELUCIDATIVAS QUANTO AOS MOTIVOS DO VOTO, CARISMA, APROVAÇÃO DO GOVERNO DE MILTON, VISÃO SOBRE JORGE TEIXEIRA, CONDIÇÃO DA ESTRADA, ETC...) PARA ELUCIDAR. COMO NÃO DISPOMOS DESSE DADOS ESTAMOS DIANTE DA FRAGILIGADE DOS MESMO, TENTAMOS LIDAR COM ELES DE FORMA COM O RNORM. 
# SE ALGUÉM LER ISSO E TIVER DADOS DA EPOCA, ENTRE EM CONTATO.

tab_model(mod, mod_22)

#por fim olhe a comparação dos modelos e veja como a tática rnorm pode ser útil para auferir com mais exatidão dados de bairros, pode ser útil para outras análises que tem mais dados.
