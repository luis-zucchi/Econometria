### econometria 3
### Isadora Russo e Luis Zucchi

# LISTA 5: Metodologia de Box-Jenkins

# S‚rie 1: Expectativa m‚dia de Infla‡Æo - IPCA - taxa acumulada para os pr¢ximos doze meses
# http://www.ipeadata.gov.br/Default.aspx
# frˆquencia da s‚rie: mensal

#base de dados:
expectativa_infl <- read.csv("C:/Users/isado/Downloads/ipeadata[01-04-2024-06-30].csv")

# Remover a terceira coluna ( j  que ‚ vazia)
expectativa_infl <- expectativa_infl[, -3]

# Renomear as colunas para simplificar

names(expectativa_infl)[1] <- "data"
names(expectativa_infl)[2] <- "valor"


#(a) Divida o conjunto de dados em duas janelas, primeira em segunda,
#em que a primeira cont‚m 4/5 dos per¡odos de tempo, e a segunda
#cont‚m a quinta parte final. Dica: use o comando window visto em aula.


#transformando em serie temporal mensal com inicio em julho de 2001

expectativa_infl_ts = ts(data = expectativa_infl$valor, start = c(2001,07), frequency = 12)


# Dividir a s‚rie em duas janelas (4/5 e 1/5)

janela1 <- window(expectativa_infl_ts, end = c(2019, 6))
janela2 <- window(expectativa_infl_ts, start = c(2019, 7))

#visualizando:
janela1
janela2


length(janela1)
length(janela2)


#(b) Com base na primeira janela, realize a etapa de identifica‡Æo da metodologia de Box-Jenkins.
#Quais sÆo os modelos candidatos? Por quˆ? 
  

# Transformar a primeira janela em uma s‚rie temporal

janela1_ts <- ts(janela1, start = c(2001, 7), frequency = 12)

# Calcular a primeira diferen‡a da primeira janela

diff_janela1 <- diff(janela1_ts)

# Estimar uma regressÆo linear das diferen‡as da s‚rie temporal

trend <- 1:length(diff_janela1)
reg <- lm(diff_janela1 ~ trend)
coeftest(reg, vcov. = vcovHAC) 

# Gr ficos ACF e PACF da primeira diferen‡a da primeira janela

par(mfrow=c(2,1))
acf(diff_janela1, lag.max = 60, main = "ACF da primeira diferen‡a (Primeira janela)")
pacf(diff_janela1, lag.max = 60, main = "PACF da primeira diferen‡a (Primeira janela)")


#aparenta ser um ARMA (12, 9)
pmax= 12
qmax=9
spmax=2
  

#Vamos usar uma funcao auxiliar, que inclui no arquivo box_jenkins_parallel.R

source("C:/Users/isado/Downloads/box_jenkins_parallel.R")

library(forecast)

tabela = arima.est.parallel(diff_janela1, pmax, qmax, d=1, pseas_max = spmax, include.constant = F, include.trend = F,
                            signif = 0.1, lags.lbox = c(20,30))

#Removendo modelos que nÃ£o convergiram
tabela = tabela[tabela$Converged,]

# ARIMA ( 0, 1, 1), ARIMA( 0,1, 2) e ARIMA( 1,1,1) passam no nosso diagn¢stico.



#(c) Com base na primeira janela, estime os modelos candidatos. 
#Com base nos crit‚rios de diang¢stico, selecione um ou mais modelos com boas m‚tricas.
#Justifique suas escolhas. 


#(d) Compute as previsäes para at‚ um ano fora da primeira janela.
#Reporte os intervalos de predi‡Æo associados.
#Como as predi‡äes se compararam ao que ocorreu na segunda janela?
#Qual ‚ a interpreta‡Æo do erro de previsÆo, nesse caso?


# Estimar o modelo ARIMA(0,1,1)
modelo_011 <- arima(janela1_ts, order = c(0, 1, 1))

# Exibir resumo do modelo ARIMA(0,1,1)
summary(modelo_011)
forecast_011 <- forecast(modelo_011, h = 12)
print(forecast_011)

# Estimar o modelo ARIMA(0,1,2)
modelo_012 <- arima(janela1_ts, order = c(0, 1, 2))

# Exibir resumo do modelo ARIMA(0,1,2)
summary(modelo_012)
forecast_012 <- forecast(modelo_012, h = 12)
print(forecast_012)

# Estimar o modelo ARIMA(1,1,1)
modelo_111 <- arima(janela1_ts, order = c(1, 1, 1))

# Exibir resumo do modelo ARIMA(1,1,1)
summary(modelo_111)
forecast_111 <- forecast(modelo_111, h = 12)
print(forecast_111)



# Extrair os dados da segunda janela
realidade <- window(expectativa_infl_ts, start = start(janela2), end = end(janela2))

# Criar dataframes para armazenar as expectativas de cada modelo
expectativa_011 <- data.frame(Expectativa = fitted(modelo_011))
expectativa_012 <- data.frame(Expectativa = fitted(modelo_012))
expectativa_111 <- data.frame(Expectativa = fitted(modelo_111))

# Ajustar o comprimento das expectativas para corresponder ao comprimento dos dados reais
expectativa_011 <- head(expectativa_011, length(realidade))
expectativa_012 <- head(expectativa_012, length(realidade))
expectativa_111 <- head(expectativa_111, length(realidade))

# Criar um dataframe para armazenar as expectativas e a realidade para cada modelo
comparacao <- data.frame(Modelo = c("ARIMA(0,1,1)", "ARIMA(0,1,2)", "ARIMA(1,1,1)"),
                         Expectativa_011 = expectativa_011$Expectativa,
                         Expectativa_012 = expectativa_012$Expectativa,
                         Expectativa_111 = expectativa_111$Expectativa,
                         Realidade = realidade)

print(comparacao)



##(e) Agora, para cada per¡odo na segunda janela, compute a previsÆo um passo … frente, 
#com base nos dados at‚ o per¡odo imediatamente anterior,
#para cada modelo Arima por vocˆ selecionado na primeira janela. 
#Calcule o erro quadr tico m‚dio, um passo … frente, com base nos erros dessas previsäes.
#Dentre os modelos por vocˆ estimados, qual se saiu melhor?
#Suas conclusäes batem com o desempenho calculado no item anterior? Qual a diferen‡a entre as m‚tricas?


# Fun‡Æo para calcular o erro quadr tico m‚dio (MSE)
mse <- function(observed, predicted) {
  mean((observed - predicted)^2)
}

# Calcular as previsäes um passo … frente para cada modelo
forecast_011 <- forecast(modelo_011, h = length(janela2))
forecast_012 <- forecast(modelo_012, h = length(janela2))
forecast_111 <- forecast(modelo_111, h = length(janela2))

# Calcular o erro quadr tico m‚dio para cada modelo
mse_011 <- mse(forecast_011$mean, janela2)
mse_012 <- mse(forecast_012$mean, janela2)
mse_111 <- mse(forecast_111$mean, janela2)

# Exibir o MSE para cada modelo
print(paste("MSE para ARIMA(0,1,1):", mse_011))
print(paste("MSE para ARIMA(0,1,2):", mse_012))
print(paste("MSE para ARIMA(1,1,1):", mse_111))

# Verificar qual modelo teve o menor MSE
melhor_modelo <- which.min(c(mse_011, mse_012, mse_111))
print(paste("O melhor modelo ‚ o ARIMA(0,1,1), com MSE =", min(c(mse_011, mse_012, mse_111))))

#"O melhor modelo ‚ o ARIMA(0,1,1), com MSE = 1.59410362436346"

# Série 2: Comércio - bens não duráveis - faturamento real 
# h = 12

# 1)
# Méida móvel centrada em t:
faturamento_comercio_bens_nao_duraveis <- read_excel("faturamento comercio bens nao duraveis.xls")

fat_bens_nao_duraveis = ts(data = faturamento_comercio_bens_nao_duraveis$`Comércio - bens não duráveis - faturamento real - índice (média 1998 = 100) - RMSP - INATIVA - - - Federação do Comércio do Estado de São Paulo, Pesquisa Conjuntural do Comércio Varejista  da Região Metropolitana de São Paulo (Fecomercio SP) - FCESP12_NAODURR12 -`, start = c(1990,01), frequency = 12)

plot(fat_bens_nao_duraveis)

#Da aula anterior, sabemos que a sÃ©rie apresenta raiz unitÃ¡ria
#SÃ©rie nÃ£o apresenta componentes determinÃ­sticos na primeira diferenÃ§a.
#Dessa forma, estimaremos o ARMA em primeiras diferenÃ§as (ARIMA(p,1,q)) sem intercepto ou tendÃªncia.

#a) dividindo a serie
fat_1 <- window(fat_bens_nao_duraveis,start=1990,end=c(2001,3))
fat_1

fat_2 <-window(fat_bens_nao_duraveis,start=c(2001,4))
fat_2

length(fat_1)
length(fat_2)

#FAC e FACP do processo estacionarizado (no nosso caso, basta tirar as diferenÃ§as, 
#pois n hÃ¡ nÃ£o estacionariedade determinÃ­stica uma vez que o processo Ã© diferenciado)
acf((diff(fat_1)), lag.max = 60)
pacf((diff(fat_1)), lag.max = 60)

#aparenta ser um ar(25)
pmax= 5
qmax=5
spmax=2



#Vamos usar uma funÃ§Ã£o auxiliar, que incluÃ­ no arquivo box_jenkins_parallel.R
source("box_jenkins_parallel.R")

tabela = arima.est.parallel(fat_1, pmax, qmax, d=1, pseas_max = spmax, include.constant = F, include.trend = F,signif = 0.1, lags.lbox = c(20,30))

#Removendo modelos que nÃ£o convergiram
tabela = tabela[tabela$Converged,]

#usando o criterio de informacao, aparenta ser arima(1,1,0)v

#c) estimacao

#ARIMA(1,1,0)(1,0,0)_12 passa em nosso diagnÃ³stico Vamos estimÃ¡-lo:
modelo1 = Arima(fat_1, order =c(1,1,0), seasonal = c(1,0,0), include.constant = F, include.drift = F)
modelo2 = Arima(fat_1, order =c(0,1,1), seasonal = c(1,0,0), include.constant = F, include.drift = F)

summary(modelo1)

checkresiduals(modelo1)


# Carregando o pacote
library(tseries)

# Calculando o teste de Jarque-Bera para os resíduos do modelo
residuos <- resid(modelo)  # Supondo que 'modelo' seja o seu modelo ARIMA
resultado_teste <- jarque.bera.test(residuos)

# Exibindo o resultado do teste
print(resultado_teste)



summary(modelo2)

checkresiduals(modelo2)

# Calculando o teste de Jarque-Bera para os resíduos do modelo
residuos2 <- resid(modelo2)  # Supondo que 'modelo' seja o seu modelo ARIMA
resultado_teste2 <- jarque.bera.test(residuos2)

# Exibindo o resultado do teste
print(resultado_teste2)


y <-forecast(modelo2, h=12)


fat_2

x <-forecast(modelo1)
plot(forecast(modelo2))




mse <- function(observed, predicted) {
  mean((observed - predicted)^2)
}

# Calcular o erro quadr tico m‚dio para cada modelo
mse_1 <- mse(x$mean, fat_2)
mse_2 <- mse(y$mean, fat_2)

# Exibir o MSE para cada modelo
print(paste("MSE:", mse_2))




modelos <- list(modelo1, modelo2)

# Inicialize um vetor para armazenar os MSEs de cada modelo
MSEs <- numeric(length(modelos))

# Iterar sobre cada modelo ARIMA na lista 'modelos'
for (i in 1:length(modelos)) {
  # Fazer previsões de um passo à frente para cada período na segunda janela
  previsoes <- forecast(modelos[[i]], h = length(fat_2))
  
  # Calcular os erros quadráticos entre as previsões e os valores observados
  erros_quadraticos <- (previsoes$mean - fat_2)^2
  
  # Calcular o MSE usando os erros quadráticos
  MSEs[i] <- mean(erros_quadraticos)
}

# Identificar o modelo com o menor MSE
melhor_modelo <- which.min(MSEs)
which.min(MSEs)
# Exibir os MSEs de todos os modelos
print(MSEs)

# Exibir o modelo que se saiu melhor
print(paste("O melhor modelo é o modelo", melhor_modelo))
