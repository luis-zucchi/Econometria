### econometria 3
### Isadora Russo e Luis Zucchi

# LISTA 3: Exerc°cios sobre Decomposiá‰es de sÇries de tempo

# SÇrie : Expectativa mÇdia de Inflaá∆o - IPCA - taxa acumulada para os pr¢ximos doze meses
# http://www.ipeadata.gov.br/Default.aspx
# fràquencia da sÇrie: mensal

#base de dados:
expectativa_infl <- read_excel("C:\\Users\\isadora\\Downloads\\ipeadata[02-04-2024-12-52]")

expectativa_infl <- ipeadata_02_04_2024_12_52_

# Remover a terceira coluna ( j† que Ç vazia)
expectativa_infl <- expectativa_infl[, -3]

# Renomear as colunas para simplificar

names(expectativa_infl)[1] <- "data"
names(expectativa_infl)[2] <- "valor"


# QUEST«O 1
#ajuste sazonal 

#carregamento de pacotes

install.packages("seasonal")
install.packages("mFilter")

library("seasonal") #Arima X13-Seats
library("mFilter") #Filtro HP


# 1) mÇtodo das mÇdias m¢veis centradas

# Criar a sÇrie temporal
# onde ts Ç sÇrie temporal e frequency = 12 dado que a sÇrie Ç mensal

expectativa_infl_ts <- ts(data = expectativa_infl$valor, start = c(2001, 7), frequency = 12)

#graficamente
#vamos vizualizar a sÇrie em n°vel e em primeira diferenáa para assim descobrir se utilizaremos o 
#modelo multiplicativo ou o aditivo

plot(expectativa_infl_ts) #sÇrie em n°vel
plot(diff(expectativa_infl_ts)) # gr†fico da primeira diferenáa

# conclus∆o: modelo aditivo parece razo√°vel


#Ajuste de mÇdia m¢vel 

decomposicao = decompose(expectativa_infl_ts, type = "additive")
expectativa_infl_ajustada = expectativa_infl_ts - decomposicao$seasonal

plot(expectativa_infl_ts)
lines(expectativa_infl_ajustada, col = 'blue')


# 2) metodologia Arima X13/Seats

modelo_sazonal = seas(expectativa_infl_ts)
#A funá∆o seas() Ç uma funá∆o do pacote "seasonal" em R, que Ç usada para ajustar modelos sazonais a uma sÇrie temporal.
lines(expectativa_infl_x13, col = 'red')
plot(modelo_sazonal)

expectativa_infl_x13 = predict(modelo_sazonal)



# QUEST«O 2


#Filtro HP
filtrado = hpfilter(expectativa_infl_x13,129600,type = "lambda")
#Regra de bolso Ç usar ?? = 1600 para dados trimestrais, ?? = 6, 25 para
#dados anuais e ?? = 129600 para dados mensais (Ravn e Uhlig, 2002).

plot(expectativa_infl_x13, col='red')
lines(filtrado$trend,col='blue')

#o gr†fico resultante exibe a sÇrie temporal original em vermelho e sua tendància estimada pelo filtro HP em azul, permitindo comparar visualmente a sÇrie original com a tendància extra°da.



# QUEST«O 3

#Proposta de Hamilton

base = cbind(expectativa_infl_x13,do.call(cbind, lapply(-24-0:3, function(x) lag(expectativa_infl_x13, x))))
colnames(base) = c('valor', paste('L',0:3,sep=''))

model = lm(valor ~., data = base)

base = cbind(base, 'hamilton_cycle' =base[,1]- predict(model, base))

plot(base[,"hamilton_cycle"], col = 'blue')
abline(h=0)




############### base de dados 2 ################################



# Lista 3 - DecomposiÁıes de sÈries de tempo
install.packages("CADFtest")
library(ggplot2)
library(readxl)

#Carregando pacotes necess√°rios. Instale-os se n√£o os possuir
library("CADFtest") # teste_fats de raiz unit√°ria
library("sandwich") #Erros padr√£o HAC
library("lmtest") #teste_fat robusto
library("car") #teste_fat de restri√ß√µes lineares

# SÈrie 1: ComÈrcio - bens n„o dur·veis - faturamento real 
# h = 12

# 1)
# MÈida mÛvel centrada em t:
faturamento_comercio_bens_nao_duraveis <- read_excel("faturamento comercio bens nao duraveis.xls")

fat_bens_nao_duraveis = ts(data = faturamento_comercio_bens_nao_duraveis$`ComÈrcio - bens n„o dur·veis - faturamento real - Ìndice (mÈdia 1998 = 100) - RMSP - INATIVA - - - FederaÁ„o do ComÈrcio do Estado de S„o Paulo, Pesquisa Conjuntural do ComÈrcio Varejista  da Regi„o Metropolitana de S„o Paulo (Fecomercio SP) - FCESP12_NAODURR12 -`, start = c(1990,01), frequency = 12)

plot(fat_bens_nao_duraveis)
plot(diff(fat_bens_nao_duraveis)) #Gr·fico da primeira diferenÁa: modelo aditivo parece razo·vel

#1. Ajuste de mÈdia mÛvel 
decomposicao = decompose(fat_bens_nao_duraveis, type = "additive")
fat_ajustado_cma = fat_bens_nao_duraveis - decomposicao$seasonal

plot(fat_bens_nao_duraveis)
lines(fat_ajustado_cma, col = 'blue')

#2. Ajuste via ARIMA X13-Seats
modelo_sazonal = seas(fat_bens_nao_duraveis)

plot(modelo_sazonal)

fat_x13 = predict(modelo_sazonal)


# A sÈrie aparenta apresentar movimento sazonal


# 2)
#Filtro HP
filtrado = hpfilter(fat_x13,129600,type = "lambda")

plot(fat_x13, col='red')
lines(filtrado$trend,col='blue')

# … possÌvel identificar ciclos na sÈrie

# 3)
#Proposta de Hamilton
base = cbind(fat_x13,do.call(cbind, lapply(-24-0:3, function(x) lag(fat_x13, x))))
colnames(base) = c('fat', paste('L',0:3,sep=''))

model = lm(fat~., data = base)

base = cbind(base, 'hamilton_cycle' =base[,1]- predict(model, base))

plot(base[,"hamilton_cycle"], col = 'blue')
abline(h=0)

# Agora n„o h· a tendÍncia, isto È, n„o apresenta o movimento de longo prazo. Agora podemos ver os ciclos da sÈrie.











# LISTA 4 -  teste_fat Raiz Unitaria

# 1)
#Testando a presen√ßa de ra√≠zes unit√°rias
#Vamos conduzir os teste_fats a 10% de signific√¢ncia

#Come√ßamos pelo modelo mais completo:

#Ho: ?? = 0
#h1: ?? < 0

teste_fat = CADFtest(fat_bens_nao_duraveis, type = "trend", max.lag.y = ceiling(12*(length(fat_bens_nao_duraveis)/100)^(1/4)),
                 criterion = "MAIC")

print(teste_fat)
# pvalor = 0.3173
#O teste_fat t N√ÉO rejeita a hip√≥tese nula de uma raiz unit√°ria, a 10% (p-valor √© de 0.3173 > 0.1)



#Vamos conduzir o teste_fat F para detectar se o modelo de fato apresenta ***** tend√™ncia linear *****

print(teste_fat$est.model)
linearHypothesis(teste_fat$est.model,c("trnd = 0", "L(y, 1) = 0" ))

# n = 169 -> 250
# F = 3.484
# Fc(10%) = 5.39
# Fc(5%) = 6.34
#A estat√≠stica F √© de 3.484. Usando o Caso 4 da tabela na p√°gina 11 dos slides, vemos que, a 10% de signific√¢ncia,
#o valor cr√≠tico para 169 observa√ß√µes È de 5.39. Como F < 5.39, n√£o rejeitamos a hip√≥tese nula de que 
#a tend√™ncia e o coeficiente associado a y_{t-1} s√£o ambos zero. Nesse caso, vamos para o modelo **** somente com
#intercepto ****

teste_fat = CADFtest(fat_bens_nao_duraveis, type = "drift", max.lag.y = ceiling(12*(length(fat_bens_nao_duraveis)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste_fat)
# pvalor = 0.8806
#O teste_fat t N√ÉO rejeita a hip√≥tese nula de uma raiz unit√°ria, a 10% (p-valor √© de 0.8806 > 0.1)


#Vamos conduzir o teste_fat F para detectar se o modelo de fato apresenta intercepto.

print(teste_fat$est.model)
linearHypothesis(teste_fat$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

# n = 169 -> 250
# F = 2.049
# Fc(10%) = 3.81
# Fc(5%) = 4.63
#A estat√≠stica F √© de 2.049. Usando o Caso 2 da tabela na p√°gina 11 dos slides, vemos que, a 10% de signific√¢ncia,
#o valor cr√≠tico para 169 observa√ß√µes È de 3.81. Como F < 3.81, n√£o rejeitamos a hip√≥tese nula de que 
#o intercepto e o coeficiente associado a y_{t-1} s√£o ambos zero. Nesse caso, vamos para o modelo sem componentes
#determin√≠sticos

teste_fat = CADFtest(fat_bens_nao_duraveis, type = "none", max.lag.y = ceiling(12*(length(fat_bens_nao_duraveis)/100)^(1/4)), 
                 criterion = "MAIC")
print(teste_fat)

# pvalue = 0.9731 > 0.1
#Aqui, tamb√©m n√£o rejeitamos a hip√≥tese nula. Conclu√≠mos que a s√©rie apresenta UMA raiz unit√°ria


# 2)

#Vamos testar a presen√ßa de componentes determin√≠sticos.
#Como os dados apresentam tend√™ncia estoc√°stica, trabalhamos com a s√©rie diferenciadas
trend_fat = 1:(length(fat_bens_nao_duraveis)-1)
modelo_fat = lm(diff(fat_bens_nao_duraveis)~trend_fat)

coeftest(modelo_fat, vcov. = vcovHAC)

#N„o rejeitamos a hipÛtese nula de que N√O H¡ tendÍncia linear determinÌstica na sÈrie em primeira diferenÁa


# 3)
# n = 169 -> 250
# F = 3.484
# Fc(10%) = 5.39
# Fc(5%) = 6.34 > F


# n = 169 -> 250
# F = 2.049
# Fc(10%) = 3.81
# Fc(5%) = 4.63 > F


# a 10% de significancia, nao mudamos nossas conclusoes
