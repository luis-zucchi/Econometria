### econometria 3
### Isadora Russo e Luis Zucchi

#### TRABALHO 1 ####

#Fa‡a o download da s‚rie de vagas nÆo preenchidas no mercado de trabalho alemÆo (link), 
#al‚m dos dados de desemprego e popula‡Æo economicamente ativa (link), de janeiro de 1991 a dezembro de 2023,
#com ajuste sazonal.


library(sandwich)
library(CADFtest)
library(lmtest)
library(car)
library(readxl)


#base de dados:

# vagas nÆo preenchidas no mercado de trabalho alemÆo

vagas <- read.csv("C:\\Users\\luisz\\Downloads\\OECD.SDD.TPS,DSD_OLAB@DF_OIALAB_INDIC,1.1+DEU.VAC_U.._Z.Y..M.csv")
vagas 


# dados de desemprego e popula‡Æo economicamente ativa com ajuste sazonal

dados <- read.csv2("C:\\Users\\luisz\\Downloads\\13231-0001_$F (1).csv")
dados

# 2. Construa a s‚rie de taxa de vacƒncia (postos nÆo preenchidos como percentual da popula‡Æo economicamente ativa). 
# H  evidˆncia de nÆo estacionariedade na s‚rie? De quais tipos? Justifique.

#A taxa de vacƒncia ‚ definida como o n£mero de postos nÆo preenchidos como percentual da popula‡Æo economicamente ativa.

# mudando nome das colunas

colnames(dados)[1] <- "ano"
colnames(dados)[2] <- "mes"
colnames(dados)[3] <- "pop_econ_ativa"
colnames(dados)[4] <- "pessoas_empregadas"
colnames(dados)[5] <- "pessoas_desempregadas"
colnames(dados)[6] <- "taxa_desemprego"


vagas <- vagas[, -c(1, 2, 3,4, 5, 6, 7, 8, 9 , 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 22, 23, 24, 25, 26, 27, 28, 29, 30)]


colnames(vagas)[2] <- "obs"
colnames(vagas)[1] <- "periodo"


# A taxa de vacƒncia ‚ definida como o n£mero de postos nÆo preenchidos (vagas) dividido pela popula‡Æo economicamente ativa (pop_econ_ativa).


# Juntar os dados de vagas e dados econ“micos

data_inicial <- "1991-01"
data_final <- "2023-12"

# linhas dentro do intervalo desejado
vagas <- vagas[vagas$periodo >= data_inicial & vagas$periodo <= data_final, ]


# Adicionar a coluna de observa‡äes ao dataframe 'dados'
dados$obs <- vagas$obs

# Calculando a taxa de vacƒncia: 

#formato num‚rico
dados$obs <- as.numeric(dados$obs)
dados$pop_econ_ativa <- as.numeric(dados$pop_econ_ativa)

#taxa de vacƒncia: 

dados$taxa_vacancia <- (dados$obs / (dados$pop_econ_ativa*1000)  )

#H  evidˆncia de nÆo estacionariedade na s‚rie?

# vamos testar a presen‡a de raiz unit ria para responder,conduzindo os testes a 10% de significƒncia

#transformando em serie temporal mensal com inicio em julho de 2001

dados_ts = ts(data = dados$taxa_vacancia, frequency = 12)


# inciaremos pelo modelo mais completo:

teste = CADFtest(dados_ts, type = "trend", max.lag.y = ceiling(12*(length(dados_ts)/500)^(1/4)),
                 criterion = "MAIC")

print(teste)


#temos que o p-valor ‚ de 0.1855

#CONCLUSÇO: O teste t nÆo rejeita a hip¢tese nula de uma raiz unit ria, a 10% (p-valor de 0.1855 > 0.1)


#Agora, Vamos conduzir o teste F para detectar se o modelo de fato apresenta tendˆncia linear

print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))

#A estat¡stica F ‚ de 4.1906

#Usando o Caso 4 da tabela na p gina 11 dos slides, vemos que, a 10% de significƒncia:

# o valor cr¡tico para 500 observa‡äes ‚ de 5.36 

# Como F (4.1906) < 5.36, nÆo rejeitamos a hip¢tese nula de que 

#a tendˆncia e o coeficiente associado a y_{t-1} sÆo ambos zero.


#####   Nesse caso, vamos para o modelo somente com intercepto   #######


teste = CADFtest(dados_ts, type = "drift", max.lag.y = ceiling(12*(length(dados_ts)/500)^(1/4)), 
                 criterion = "MAIC")

print(teste)

# Encontramos o p-value de 0.7693

# O teste t nÆo rejeita a hip¢tese nula de uma raiz unit ria, a 10% (p-valor ‚ de 0.7693 > 0.1)



#Vamos conduzir o teste F para detectar se o modelo de fato apresenta intercepto.

print(teste$est.model)
linearHypothesis(teste$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

#A estat¡stica F ‚ de  0.7968

#Usando o Caso 2 da tabela na p gina 11 dos slides, vemos que, a 10% de significƒncia:

# o valor cr¡tico para 500 observa‡äes ‚ 3.79

# Como F ( 0.7968) < 3.79, nÆo rejeitamos a hip¢tese nula de que o intercepto e o coeficiente associado a y_{t-1} sÃ£o ambos zero. 



######  Nesse caso, vamos para o modelo sem componentes determin¡sticos:   ########

teste = CADFtest(dados_ts, type = "none", max.lag.y = ceiling(12*(length(dados_ts)/500)^(1/4)), 
                 criterion = "MAIC")
print(teste)

# P-value de 0.7838

# O teste t nÆo rejeita a hip¢tese nula de uma raiz unit ria, a 10% (p-valor ‚ de  0.7838 > 0.1)

#Aqui, tamb‚m nÆo rejeitamos a hip¢tese nula.

#Conclu¡mos que a s‚rie apresenta UMA raiz unit ria


##### Agora vamos testar a presen‡a de componentes determin¡sticos ###########

#Como os dados apresentam tendˆncia estoc stica, trabalhamos com a serie diferenciadas

trend = 1:(length(dados_ts)-1)
modelo = lm(diff(dados_ts)~trend)

coeftest(modelo, vcov. = vcovHAC)

# p valor =  0.9298

# O teste t nÆo rejeita a hip¢tese de que nao ha tendencia linear determin¡stica na serie em primeira diferen‡a,
# (p-valor ‚ de 0.9298 > 0.1)


#Nao rejeitamos a hipotese nula de que nao ha tendencia linear determin¡stica na serie em primeira diferen‡a




#3



#taxa_eficiente <- read.csv2("d:\\Users\\luis.souza\\Downloads\\taxa eficiente.csv")
taxa_eficiente <- read_excel("C:\\Users\\luisz\\Downloads\\taxa eficiente.xlsx")

tx_eficiente <- ts(data = taxa_eficiente$`Tx eficiente` , start = c(1991, 1), frequency = 12)
tx_eficiente


plot(tx_eficiente)
plot(diff(tx_eficiente))

teste_tx = CADFtest(tx_eficiente, type = "trend", max.lag.y = ceiling(12*(length(tx_eficiente)/100)^(1/4)),
                     criterion = "MAIC")
print(teste_tx)

#pvalor = 0.2265
#nao rejeitamos a hipotese nula de haver raiz unitaria

# teste de tendencia linear
print(teste_tx$est.model)
linearHypothesis(teste_tx$est.model,c("trnd = 0", "L(y, 1) = 0" ))

# Fc (5%) = 5.36
# Fc (10%) = 6.3
# F^ = 4.1058
# F^ < Fc, nao rejeitamos que nao ha tendencia linear
# vamos ao modelo com intercepto

teste_tx = CADFtest(tx_eficiente, type = "drift", max.lag.y = ceiling(12*(length(tx_eficiente)/100)^(1/4)),
                    criterion = "MAIC")
print(teste_tx)

#pvalor = 0.1755
#nao rejeitamos a hipotese nula de haver raiz unitaria

#teste de intercepto

print(teste_tx$est.model)
linearHypothesis(teste_tx$est.model,c("(Intercept) = 0", "L(y, 1) = 0" ))

# Fc (5%) = 5.36
# Fc (10%) = 6.3
# F^ = 2.6244
# F^ < Fc, nao rejeitamos que nao ha intercepto
# vamos ao modelo com intercepto

teste_tx = CADFtest(tx_eficiente, type = "none", max.lag.y = ceiling(12*(length(tx_eficiente)/100)^(1/4)),
                    criterion = "MAIC")
print(teste_tx)

#pvalor = 0.6124
#nao rejeitamos a hipotese nula de haver raiz unitaria


trend_ef = 1:(length(tx_eficiente)-1)
modelo_ef = lm(diff(tx_eficiente)~trend_ef)

coeftest(modelo_ef, vcov. = vcovHAC)







# 4
tx_reduz <- ts(data = taxa_eficiente$`Tx eficiente` , start = c(1991, 1), end = c(2002,12), frequency = 12)
tx_reduz

# Ajustar modelo de regressão linear para a tendência
time <- time(tx_reduz)
fit_trend <- lm(tx_reduz ~ time)
summary(fit_trend)

# Obter resíduos (série sem tendência)
residuals_trend <- residuals(fit_trend)

# Plotar série sem tendência
plot(residuals_trend, main="Série sem Tendência", ylab="Resíduos", xlab="Tempo")



# Decomposição aditiva (ou multiplicativa) para remover sazonalidade
decomposed <- decompose(tx_reduz)
seasonally_adjusted <- residuals_trend - decomposed$seasonal

# Plotar série sem sazonalidade
#plot(seasonally_adjusted, main="Série sem Tendência e Sazonalidade", ylab="Resíduos Ajustados", xlab="Tempo")



# Análise ACF e PACF da série ajustada
acf(residuals_trend, main="FAC dos Resíduos Ajustados")
pacf(residuals_trend, main="FACP dos Resíduos Ajustados")

#notamos que há decaimento na FAC e truncamento de ordem 1 na FACP



#Sendo conservador, da inspeÃ§Ã£o visual da FAC e FACP, incluÃ­mos, na parte nÃ£o sazonal:
pmax = 3
qmax = 3

#A parte sazonal parece compatÃ­vel com, no mÃ¡ximo, um SAR(2). Vamos considerar:
spmax = 2

#Vamos usar uma funÃ§Ã£o auxiliar, que incluÃ­ no arquivo box_jenkins_parallel.R
source("box_jenkins_parallel.R")
tabela = arima.est.parallel(tx_reduz, pmax, qmax, d=1, pseas_max = spmax, include.constant = F, include.trend = T,
                            signif = 0.1, lags.lbox = c(20,30))

#best_model <- auto.arima(residuals_trend, seasonal=FALSE, stepwise=FALSE, approximation=FALSE)
#summary(best_model)


#Removendo modelos que nÃ£o convergiram
tabela = tabela[tabela$Converged,]

#ARIMA(0,1,3) passa em nosso diagnÃ³stico Vamos estimÃ¡-lo:
modelo = Arima(ts(tx_reduz, start = c(1991, 1), frequency = 12), order =c(0,1,3), include.constant = F, include.drift = F)
summary(modelo)

checkresiduals(modelo)

tx_reduz

# Calculando o teste de Jarque-Bera para os resíduos do modelo
residuos <- resid(modelo)  # Supondo que 'modelo' seja o seu modelo ARIMA
resultado_teste <- jarque.bera.test(residuos)
resultado_teste


forecast_values <- forecast(modelo, h=48)
forecast_values
plot(forecast(modelo, h=48))
modelo

forecast_values$mean
forecast_values$lower
forecast_values$upper







# Previsão para 48 meses (jan/2003 a dez/2006)
forecast_arima <- forecast(modelo, h = 48)
forecast_arima_values <- forecast_arima$mean



forecast_uestrela <- forecast_arima
janela2 <- tx_reduz <- ts(data = taxa_eficiente$`Tx eficiente` , start = c(2003,1), frequency = 12)
janela2


 #A estimativa pontual será:
  alpha_pontual <- janela2 - forecast_uestrela$mean
 #A estimativa inferior é:
   alpha_inf <- janela2 - forecast_uestrela$lower
alpha_inf_95 <- alpha_inf[, 2] #Selecionando as previsões a 95%

 #A estimativa superior é:
   alpha_sup <- janela2 - forecast_uestrela$upper
 alpha_sup_95 <- alpha_sup[, 2] #Selecionando as previsões a 95%
 #Visualização gráfico dos efeitos estimados
   plot(alpha_inf_95, type = "n", col = "lightblue", ylim =range(alpha_inf_95, alpha_sup_95), xlab = "Time", ylab = "Values")
 polygon(c(time(alpha_inf_95), rev(time(alpha_sup_95))),
          + c(alpha_inf_95, rev(alpha_sup_95)), col = "lightblue", border = NA)
 lines(alpha_pontual, col = "red")
 abline(h = 0, col = "black", lty = 2)

