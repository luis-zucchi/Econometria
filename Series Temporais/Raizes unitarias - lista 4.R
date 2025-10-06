### econometria 3
### Isadora Russo e Luis Zucchi

# LISTA 4: Exerc°cios sobre Ra°zes Unit†rias

# SÇrie : Expectativa mÇdia de Inflaá∆o - IPCA - taxa acumulada para os pr¢ximos doze meses
# http://www.ipeadata.gov.br/Default.aspx
# fràquencia da sÇrie: mensal

#base de dados:
expectativa_infl <- read.csv("C:/Users/isado/Downloads/ipeadata[01-04-2024-06-30].csv")
expectativa_infl <- read.csv("C:/Users/luisz/Downloads/ipeadata[01-04-2024-06-30].csv")

# Remover a terceira coluna ( j† que Ç vazia)
expectativa_infl <- expectativa_infl[, -3]

# Renomear as colunas para simplificar

names(expectativa_infl)[1] <- "data"
names(expectativa_infl)[2] <- "valor"

#transformando em serie temporal mensal com inicio em julho de 2001

expectativa_infl_ts = ts(data = expectativa_infl$valor, start = c(2001,07), frequency = 12)



##### testando a presenáa de raizes unitarias


#Vamos conduzir os testes a 10% de significÉncia:

# inciaremos pelo modelo mais completo:

teste = CADFtest(expectativa_infl_ts, type = "trend", max.lag.y = ceiling(12*(length(expectativa_infl_ts)/250)^(1/4)),
                 criterion = "MAIC")

print(teste)

#temos que o p-valor Ç de 0.3736
# definimos 250 pela tabela dado que o numero de observaá‰es Ç 272 

#CONCLUS«O: O teste t n∆o rejeita a hip¢tese nula de uma raiz unit†ria, a 10% (p-valor de 0.3736 > 0.1)


#Agora, Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendància linear

print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estat°stica F Ç de 2.9344

#Usando o Caso 4 da tabela na p†gina 11 dos slides, vemos que, a 10% de significÉncia:

#o valor cr°tico para 272 observaá‰es Ç de 5.39. 

#Como F (2.9344) < 5.39, n∆o rejeitamos a hip¢tese nula de que 
#a tendància e o coeficiente associado a y_{t-1} s∆o ambos zero.


#Nesse caso, vamos para o modelo somente com intercepto

teste = CADFtest(expectativa_infl_ts, type = "drift", max.lag.y = ceiling(12*(length(expectativa_infl_ts)/250)^(1/4)), 
                 criterion = "MAIC")

print(teste)

# Encontramos o p-value de 0.2507

# O teste t n∆o rejeita a hip¢tese nula de uma raiz unit†ria, a 10% (p-valor Ç de 0.2507 > 0.1)


#Vamos conduzir o teste F para detectar se o modelo de fato apresenta intercepto.

print(teste$est.model)
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#A estat°stica F Ç de 2.2081.

#Usando o Caso 2 da tabela na p†gina 11 dos slides, vemos que, a 10% de significÉncia:

# o valor cr°tico para 272 observaá‰es Ç 3.81.
# Como F (2.2081) < 3.81, n∆o rejeitamos a hip¢tese nula de que o intercepto e o coeficiente associado a y_{t-1} s√£o ambos zero. 





#Nesse caso, vamos para o modelo sem componentes determin°sticos:

teste = CADFtest(expectativa_infl_ts, type = "none", max.lag.y = ceiling(12*(length(expectativa_infl_ts)/250)^(1/4)), 
                 criterion = "MAIC")
print(teste)

# P-value de 0.2625

# O teste t n∆o rejeita a hip¢tese nula de uma raiz unit†ria, a 10% (p-valor Ç de 0.2625 > 0.1)

#Aqui, tambÇm n∆o rejeitamos a hip¢tese nula.

#Conclu°mos que a sÇrie apresenta UMA raiz unit†ria



#1. Para cada uma das sÇries, realize o procedimento sequencial descrito nos
#slides para testar a presenáa de raiz unit†ria. Conduza os testes ao n°vel
#de significÉncia de 10%. Qual a conclus∆o dos procedimentos?

#Resposta: a conclus∆o Ç que a sÇrie apresenta apenas uma raiz unit†ria a n°vel de significancia de 10%



#2. Para cada uma das sÇries, realize o teste de tendància determin°stica descrito nos slides, j† usando a sÇrie transformada obtida com base na conclus∆o dos testes no item anterior. Conduza os testes ao n°vel de significÉncia de 10%. Qual a conclus∆o de cada teste?

# resposta: 

#Vamos testar a presenáa de componentes determin°sticos.

#Como os dados apresentam tendància estoc†stica, trabalhamos com a serie diferenciadas

trend = 1:(length(expectativa_infl_ts)-1)
modelo = lm(diff(expectativa_infl_ts)~trend)

coeftest(modelo, vcov. = vcovHAC)

#Nao rejeitamos a hipotese nula de que nao ha tendencia linear determin°stica na serie em primeira diferenáa


#3. Repita os dois itens anteriores a 5% de significÉncia. As conclus‰es mudaram?


#Agora vamos conduzir os testes a 5% de significÉncia:

# inciaremos pelo modelo mais completo:

teste = CADFtest(expectativa_infl_ts, type = "trend", max.lag.y = ceiling(12*(length(expectativa_infl_ts)/250)^(1/4)),
                 criterion = "MAIC")

print(teste)

#temos que o p-valor Ç de 0.3736
# definimos 250 pela tabela dado que o numero de observaá‰es Ç 272 

#CONCLUS«O: O teste t n∆o rejeita a hip¢tese nula de uma raiz unit†ria, a 5% (p-valor de 0.3736 > 0.05)


#Agora, Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendància linear

print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estat°stica F Ç de 2.9344

#Usando o Caso 4 da tabela na p†gina 11 dos slides, vemos que, a 5% de significÉncia:

#o valor cr°tico para 272 observaá‰es Ç de 6.34

#Como F (2.9344) < 6.34, n∆o rejeitamos a hip¢tese nula de que 
#a tendància e o coeficiente associado a y_{t-1} s∆o ambos zero.


#Nesse caso, vamos para o modelo somente com intercepto

teste = CADFtest(expectativa_infl_ts, type = "drift", max.lag.y = ceiling(12*(length(expectativa_infl_ts)/250)^(1/4)), 
                 criterion = "MAIC")

print(teste)

# Encontramos o p-value de 0.2507

# O teste t n∆o rejeita a hip¢tese nula de uma raiz unit†ria, a 5% (p-valor Ç de 0.2507 > 0.05)


#Vamos conduzir o teste F para detectar se o modelo de fato apresenta intercepto.

print(teste$est.model)
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#A estat°stica F Ç de 2.2081.

#Usando o Caso 2 da tabela na p†gina 11 dos slides, vemos que, a 10% de significÉncia:

# o valor cr°tico para 272 observaá‰es Ç 4.63
# Como F (2.2081) < 4.63 n∆o rejeitamos a hip¢tese nula de que o intercepto e o coeficiente associado a y_{t-1} s√£o ambos zero. 

#Nesse caso, vamos para o modelo sem componentes determin°sticos:

teste = CADFtest(expectativa_infl_ts, type = "none", max.lag.y = ceiling(12*(length(expectativa_infl_ts)/250)^(1/4)), 
                 criterion = "MAIC")
print(teste)

# P-value de 0.2625

# O teste t n∆o rejeita a hip¢tese nula de uma raiz unit†ria, a 5% (p-valor Ç de 0.2625 > 0.05)

#Aqui, tambÇm n∆o rejeitamos a hip¢tese nula.

#Conclu°mos que a sÇrie apresenta UMA raiz unit†ria


# Agora vamos testar a presenáa de componentes determin°sticos:

#Como os dados apresentam tendància estoc†stica, trabalhamos com a serie diferenciadas

trend = 1:(length(expectativa_infl_ts)-1)
modelo = lm(diff(expectativa_infl_ts)~trend)

coeftest(modelo, vcov. = vcovHAC)

#Nao rejeitamos a hipotese nula de que nao ha tendencia linear determin°stica na serie em primeira diferenáa

# Sendo assim, as conclus‰es n∆o mudaram de um nivel de significancia para o outro. 






# LISTA 4 -  teste_fat Raiz Unitaria
library("readxl")
faturamento_comercio_bens_nao_duraveis <- readxl("faturamento comercio bens nao duraveis.xls")

fat_bens_nao_duraveis = ts(data = faturamento_comercio_bens_nao_duraveis$`ComÈrcio - bens n„o dur·veis - faturamento real - Ìndice (mÈdia 1998 = 100) - RMSP - INATIVA - - - FederaÁ„o do ComÈrcio do Estado de S„o Paulo, Pesquisa Conjuntural do ComÈrcio Varejista  da Regi„o Metropolitana de S„o Paulo (Fecomercio SP) - FCESP12_NAODURR12 -`, start = c(1990,01), frequency = 12)

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

#N√£o rejeitamos a hip√≥tese nula de que N√ÉO H√Å tend√™ncia linear determin√≠stica
#na s√©rie em primeira diferen√ßa



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


