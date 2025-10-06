### econometria 3
### Isadora Russo e Luis Zucchi


#### Lista de Exercicios VAR ####

library("CADFtest")
library("car")
library("lmtest")
library("sandwich")

#Pacote para VAR
install.packages("vars")
library("vars")

# 1. Escolha uma das sÇries por vocà analisadas na metodologia de Box-Jenkins.

#  a) Considere mais uma vari†vel que vocà acredita que possa ajudar na previs∆o desta sÇrie.
#  Ajuste um modelo VAR bivariado para as duas sÇries, devidamente estacionarizadas. 
#  Conduza todas as etapas de especificaá∆o e diagn¢stico do modelo, e calcule as previs‰es fora da amostra.
#  Como elas se comparam Ös obtidas na metodologia BoxJenkins?
  

# SÇrie : Expectativa mÇdia de Inflaá∆o - IPCA - taxa acumulada para os pr¢ximos doze meses
# http://www.ipeadata.gov.br/Default.aspx
# fràquencia da sÇrie: mensal

#base de dados:
expectativa_infl <- read.csv("ipeadata[01-04-2024-06-30].csv")

# Remover a terceira coluna ( j† que Ç vazia)
expectativa_infl <- expectativa_infl[, -3]

# Renomear as colunas para simplificar

names(expectativa_infl)[1] <- "data"
names(expectativa_infl)[2] <- "valor"


# nova base de dados que adicionaremos para ajudar na previs∆o da sÇrie de expetativa mÇdia de inflaá∆o

# SÇrie :Taxa de juros - Over / Selic - acumulada no màs
# http://www.ipeadata.gov.br/Default.aspx
# fràquencia da sÇrie: mensal


selic <- read.csv("selic - ipeadata[21-05-2024-04-59].csv")
selic

# Renomear as colunas para simplificar

names(selic)[1] <- "data"
names(selic)[2] <- "valor"

# Remover a primeira linha ( j† que Ç vazia)
selic <- selic[-1,  ]
selic <- selic[-(1:6), ]


# Remover a terceira coluna ( j† que Ç vazia)
selic <- selic[, -3]


#transformando em serie temporal mensal com inicio em julho de 2001

expectativa_infl_ts = ts(data = expectativa_infl$valor, start = c(2001,07), frequency = 12)

selic_ts <- ts(data = selic$valor, start = c(2001, 7), frequency = 12)


dados = cbind(selic_ts, expectativa_infl_ts)
dados = window(dados, start = c(2001,07), end = c(2024,03))


plot(dados)


#Vamos verificar a fonte de n∆o estacionariedade dos dados 

##########################
#Expectativas de inflaá∆o 
##########################

#TESTE DE RAIZ UNIT√ÅRIA

teste = CADFtest(dados[,1], type = "trend", max.lag.y = ceiling(12*(length(dados[,1])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)

#Rejeitamos a nula de raiz unit√°ria, aos n√≠veis de signific√¢ncia convencionais. 

#TESTE DE COMPONENTE DETERMIN√çSTICO
trnd = 1:nrow(dados)
coeftest(lm(dados[,1]~ trnd), vcov = vcovHAC)

#Evid√™ncia de tend√™ncia determin√≠stica na s√©rie em n√≠vel, a 5%


#######
#SELIC#
#######

#TESTE DE RAIZ UNIT√ÅRIA
teste = CADFtest(dados[,2], type = "trend", max.lag.y = ceiling(12*(length(dados[,2])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
#N√£o rejeitamos a nula, nesta primeira etapa, nos n√≠veis convencionais.

#Vamos fazer o teste F
print(teste$est.model)
linearHypothesis(teste$est.model,c("trnd = 0", "L(y, 1) = 0" ))
#N√£o rejeitamos H0, do teste F, a 5%, olhando na tabela dos slides

#Rodando agora no modelo com drift somente
teste = CADFtest(dados[,2], type = "drift", max.lag.y = ceiling(12*(length(dados[,2])/100)^(1/4)),
                 criterion = "MAIC")
summary(teste)
#Conclu√≠mos que n√£o h√° raiz unit√°ria


#TESTE DE COMPONENTE DETERMIN√çSTICO
trnd = 1:nrow(dados)
coeftest(lm(dados[,2]~ trnd), vcov = vcovHAC)
#Evid√™ncia de tend√™ncia determin√≠stica na s√©rie em n√≠vel aos n√≠veis de signific√¢ncia convencionais


#Vamos analisar as FACs dos processos, para detectar a presen√ßa de sazonalidade.
#Note que os dois primeiros processos devem ser detrended

resid = residuals(lm(dados[,1:2]~ trnd))
dados = cbind(dados,resid)
colnames(dados)[4:5] = paste("r", c("expectativa_infl_ts","selic_ts"), sep="_")


acf(dados[,3:5],lag.max=40)

#N√£o parece haver evid√™ncia de componentes sazonais no processo

#Vamos estimar um modelo VAR para as s√©ries.

#H√° duas alternativas aqui: ou fazemos com infla√ß√£o + juros e selic detrended,
#ou usamos as s√©ries originais e inclu√≠mos uma tend√™ncia linear diretamente no modelo vetorial.

#Desvantagem do segundo m√©todo √© que sabemos que tend√™ncia linear na infla√ß√£o √© desnecess√°ria
#No entanto, estima√ß√£o conjunta dos par√¢metros pode ser vantajosa para as duas outras s√©ries.
#Logo, vantagens relativas n√£o s√£o claras.

#Vamos incluir a tend√™ncia linear, pois facilita o c√°lculo de previs√µes via R (vale testar a outra alternativa).

dados <- na.omit(dados)


# Selecionando ordem do VAR inicial
VARselect(dados[, 1:2], type = "both", lag.max = ceiling(12 * (nrow(dados) / 100)^(1 / 4)))

#Vamos verificar o modelo do VAR com SC
modelo = VAR(dados[,1:2],type = 'both', p = 2)

summary(modelo)
help(roots)
any(roots(modelo)>1)

#Processo estimado nao apresenta raiz unit√°ria

#Vamos fazer o teste de raz√£o de verossimilhan√ßa

stat = 2*(logLik(modelo) - logLik(VAR(dados[,1:2],type = 'both', p = 1)))
print(stat)

#Valor cr√≠tico √©, a 5%

qchisq(0.95, 9)

#Rejeitamos nula de que segunda defasagem √© in√≥cua.

#Vamos verificar o teste de Portmanteau para autocorrela√ß√£o dos res√≠duos

serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))

#Rejeitamos a hip√≥tese nula: VAR(2) joga muita informa√ß√£o fora!!!

#Vamos considerar o VAR com base no AIC

modelo = VAR(dados[,1:2],type = 'both', p = 14)

summary(modelo)
help(roots)
any(roots(modelo)>1)

#Processo estimado  apresenta raiz unit√°ria

#Vamos fazer o teste de raz√£o de verossimilhan√ßa

stat = 2*(logLik(modelo) - logLik(VAR(dados[,1:2],type = 'both', p = 13)))
print(stat)
#Valor cr√≠tico √©, a 5%
qchisq(0.95, 9)
#Evid√™ncia de que precisamos desta defasagem

serial.test(modelo, lags.pt = 20, type = c("PT.adjusted"))
#Parecemos melhorar algo em rela√ß√£o ao modelo menor

normality.test(modelo)
#Rejeitamos claramente normalidade dos erros

#Podemos procurar especifica√ß√µes com ordem maiores, mas a dimensionalidade vai tender a piorar
#Vamos ficar com o modelo do AIC (vale comparar os resultados com o do BIC e HQ tamb√©m)

predicoes = predict(modelo, ci = 0.95)

fanchart(predict(modelo, ci = 0.95), plot.type = 'single')



### VAMOS INCORPORAR OS DADOS DE ABRIL PARA A SELIC
dado_up = read.csv("selicatualizado.csv")
at= dado_up[nrow(dado_up),2]
eup = summary(modelo)$covres[2,]%*%solve(summary(modelo)$covres[2,2])%*%(at - predicoes$fcst$selic_ts[1,1])
at

#Proje√ß√µes atualizadas em abril
vlh =t(do.call(c,lapply(predicoes$fcst, function(x) x[1,1])) + eup)

#Proje√ß√µes de maio para frente
mod_up = predict(VAR(ts(rbind(dados[,1:2],vlh),end=c(2024,04),frequency=12) ,type = 'both', p = 14), ci = 0.95)

# Valores previstos
valores_previstos <- mod_up$fcst
print(valores_previstos)

#Testes de causalidade de Granger e contempor√¢nea
causality(modelo,cause = 'selic_ts')


#  b) Realize o teste da hip?tese nula de que a inclus?o das defasagens da nova vari?vel n?o afeta as previs?es da s?rie original, 
# relativamente a um modelo AR de mesma ordem que o VAR(p) por voc? escolhido. Qual ? o nome desta hip?tese nula? 
# Qual a conclus?o do teste, a 5%?


  
  
#  b) Realize o teste da hip¢tese nula de que a inclus∆o das defasagens da nova vari†vel n∆o afeta as previs‰es da sÇrie original, 
# relativamente a um modelo AR de mesma ordem que o VAR(p) por vocà escolhido. Qual Ç o nome desta hip¢tese nula? 
# Qual a conclus∆o do teste, a 5%?
  
  
  