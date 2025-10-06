install.packages("readxl")
library(readxl)
install.packages("stats")
install.packages("lmtest")
library(stats)
library(lmtest)

#IMPORTACAO E TRATAMENTO DE BASES DE DADOS

#IPCA
#importando base de dados do ipca
ipca <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/ipca tx variacao.xls")

#mudando o nome da coluna
colnames(ipca)[2] <- "IPCA Tx Variacao"

#selecionando o intervalo desejado (1999-2014)
ipca <- ipca[c(19:35),]

#como esta em pontos percentuais, dividimos por 100
ipca$`IPCA Tx Variacao` <- ipca$`IPCA Tx Variacao`/100

#indice de precos
ipca$indice <- 100 

#deflacionando, tomando o ano de 2014 como base
for (i in 1:16) {
  ipca[i+1,"indice"] <- (ipca[i+1,"IPCA Tx Variacao"]/100+1)*
    ipca[i,"indice"]
}

ipca$deflator_base2014 <- c(NA,rep(1,16))

for (i in 0:15) {
  ipca[17-i,"deflator_base2014"] <- ipca[17,"indice"]/
    ipca[17-i-1,"indice"]
}

#vamos tirar os anos de 1998, 2000 e 2010
ipca <- ipca[-13,]
ipca <- ipca[-3,]
ipca <- ipca[-1,]


#SALARIO MINIMO NOMINAL
#importando base de dados do salario minimo
salario_nominal <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/salario minimo nominal.xls")


#como o salario minimo estao por mes, selecionamos apenas o ultimo mes de cada ano
salario_nominal_anual <- salario_nominal[substr(salario_nominal$Data, nchar(salario_nominal$Data)-1, nchar(salario_nominal$Data)) == "12", ]

#selecionando o intervalo desejado (1999-2014)
salario_nominal_anual <- salario_nominal_anual[c(60:75),]

#mudando o nome da coluna
colnames(salario_nominal_anual)[2] <- "Salarionominal"

#vamos tirar os anos de 2000 e 2010
salario_nominal_anual <- salario_nominal_anual[-12,]
salario_nominal_anual <- salario_nominal_anual[-2,]



#SALARIO MINIMO SP
#importando base de dados do salario minimo do Estado de SP
salario_sp <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/salario minimo nom sp.xlsx")

#deletando colunas inuteis (selecionamos apenas a faixa 1)
salario_sp <- salario_sp[,c(1,3)]

#deletando o ano de 2010
salario_sp <- salario_sp[-4,]


#UNINDO SALARIO NOMINAL E SALARIO SP
salario_nominal_anual$Salarionominal[8:14] <- salario_sp$`Faixa 1`[1:8]

#DEFLACIONANDO SALARIO NOMINAL (SP)
#adicionando uma coluna com o indice deflator
salario_nom_deflacionado <- cbind(salario_nominal_anual, ipca$deflator_base2014)
salario_nom_deflacionado$'SalarioReal' <- salario_nom_deflacionado$Salarionominal*salario_nom_deflacionado$`ipca$deflator_base2014`
salario_real <- as.data.frame(salario_nom_deflacionado$'SalarioReal')



#TAXA DE DESEMPREGO
#importando base de dados
taxa_desemprego <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/taxa de desemprego.xls")

#selecionando os dados do estado de SP
taxa_desemprego <- taxa_desemprego[26,]

#selecionando o intervalo desejado (1999-2014)
taxa_desemprego <- taxa_desemprego[,c(10:23) ]

#transpondo a matriz
taxa_desemprego <- t(taxa_desemprego)
taxa_desemprego <- as.data.frame(taxa_desemprego)

#mudando o nome da coluna
colnames(taxa_desemprego)[1] <- "TaxaDesemprego"



#GRAU INFORMALIDADE
#importando base de dados
grau_informalidade <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/grau de informalidade.xls")

#selecionando os dados do estado de SP
grau_informalidade <- grau_informalidade[26,]

#selecionando o intervalo desejado (1999-2014)
grau_informalidade <- grau_informalidade[,c(9:22) ]

#transpondo a matriz
grau_informalidade <- t(grau_informalidade)
grau_informalidade <- as.data.frame(grau_informalidade)

#mudando o nome da coluna
colnames(grau_informalidade)[1] <- "GrauInformalidade"



#ANOS DE ESTUDO
#importando base de dados
anos_estudo <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/anos de estudo.xls")

#selecionando os dados do estado de SP
anos_estudo <- anos_estudo[26,]

#selecionando o intervalo desejado (1999-2014)
anos_estudo <- anos_estudo[,c(13:26) ]

#transpondo a matriz
anos_estudo <- t(anos_estudo)
anos_estudo <- as.data.frame(anos_estudo)

#mudando o nome da coluna
colnames(anos_estudo)[1] <- "AnosEstudo"



#COEFICIENTE DE GINI
#importando base de dados
coeficiente_gini <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/coeficientedegini.xls")

#selecionando os dados do estado de SP
coeficiente_gini <- coeficiente_gini[26,]

#selecionando o intervalo desejado (1999-2014)
coeficiente_gini <- coeficiente_gini[,c(13:26) ]

#transpondo a matriz
coeficiente_gini <- t(coeficiente_gini)
coeficiente_gini <- as.data.frame(coeficiente_gini)

#mudando o nome da coluna
colnames(coeficiente_gini)[1] <- "CoeficienteGini"



#POBREZA - N DE DOMICILIOS POBRES
#importando base de dados
n_domicilios_pobres <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/pobreza-numero de domicilios pobres.xls")

#selecionando os dados do estado de SP
n_domicilios_pobres <- n_domicilios_pobres[26,]

#selecionando o intervalo desejado (1999-2014)
n_domicilios_pobres <- n_domicilios_pobres[,c(13:26) ]

#transpondo a matriz
n_domicilios_pobres <- t(n_domicilios_pobres)
n_domicilios_pobres <- as.data.frame(n_domicilios_pobres)

#mudando o nome da coluna
colnames(n_domicilios_pobres)[1] <- "NDomiciliosPobres"



#PIB SP
#importando base de dados
pib_sp <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/PIB SP.xlsx")

#selecionando o intervalo desejado (2002-2014)
pib_sp <- pib_sp[c(1:13) ,]

#vamos tirar o ano de 2010 e a coluna de anos
pib_sp <- pib_sp[-9,-1]

#imputando os valores de 1999 e 2001
anos_faltantes <- data.frame('PIB SP' =c(336838,400629))
colnames(anos_faltantes)[1] <- "PIB SP"
pib_sp <- rbind(anos_faltantes, pib_sp)

#adicionando uma coluna com o pib de sp deflacionado
pib_sp_deflacionado <- cbind(pib_sp, ipca$deflator_base2014)
pib_sp_deflacionado$'PIBSP' <- pib_sp_deflacionado$`PIB SP`*pib_sp_deflacionado$`ipca$deflator_base2014`

pib_sp_real <- as.data.frame(pib_sp_deflacionado$'PIBSP')
colnames(pib_sp_real)[1] <- "PIB SP"


#BOLSA FAMILIA
#importando base de dados
bolsa_familia <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/programabolsafamiliavalortotaldosbeneficiarios.xls")

#selecionando os dados do estado de SP
bolsa_familia <- bolsa_familia[26,]

#selecionando o intervalo desejado (2004-2014)
bolsa_familia <- bolsa_familia[,c(4:14) ]

#deletando 2010
bolsa_familia <- bolsa_familia[,-7]

#transpondo a matriz
bolsa_familia <- t(bolsa_familia)
bolsa_familia <- as.data.frame(bolsa_familia)

#mudando o nome da coluna
colnames(bolsa_familia)[1] <- "BolsaFamilia"

#acrescentando zeros para periodo anterior a 2004
anos_antes_2004 <- data.frame('BolsaFamilia' =c(0,0,0,0), row.names = c(1999,2001,2002,2003))
bolsa_familia <- rbind(anos_antes_2004, bolsa_familia)



#N OCORRENCIAS
#importando base de dados
n_ocorrencias <- read_excel("C:/Users/luisz/Downloads/econometria trabalho/Anual-Estado de Sao Paulo.xlsx")

#selecionando o intervalo desejado (1999-2014)
n_ocorrencias <- n_ocorrencias[c(1:16), ]

#vamos tirar os anos de 2000 e 2010
n_ocorrencias <- n_ocorrencias[-12,]
n_ocorrencias <- n_ocorrencias[-2,]

#soma de ocorrencias
n_ocorrencias$'N Ocorrencias' <- n_ocorrencias$Ano + n_ocorrencias$Homicidio+ n_ocorrencias$Furto + n_ocorrencias$Roubo + n_ocorrencias$FRV 

#deletando as demais colunas
n_ocorrencias <- n_ocorrencias[,6]


#////////////////////////////////////////////////////////////////////////////////////

#JUNTANDO TODOS OS DATAFRAMES
#juntaremos ambos os data frames
df <- cbind(n_ocorrencias, salario_real, taxa_desemprego, grau_informalidade, anos_estudo, coeficiente_gini, n_domicilios_pobres, pib_sp_real, bolsa_familia)

#renomeando colunas do df
colnames(df)[8] <- 'PIB_SP'
colnames(df)[1] <- 'n_ocorrencias'
colnames(df)[2] <- 'Salario_Minimo'


#REGRESSAO LINEAR MULTIPLA:

#regressao
reg1 <- lm(n_ocorrencias ~  Salario_Minimo + TaxaDesemprego + GrauInformalidade + AnosEstudo + CoeficienteGini + NDomiciliosPobres + PIB_SP + BolsaFamilia, data = df)

summary(reg1)


#TESTE DE HIPOTESES

#matriz de coeficientes
betas<- as.data.frame(coef(reg1))
betas

#matriz de erros padrao
ep <- as.data.frame(sqrt(diag(vcov(reg1))))
ep

#estatisticas t para cada coeficiente
t_coeficientes <- betas/ep 
t_coeficientes

#graus de liberdade
dim(df)
n = 14
k = 8
gl <- n - k - 1
gl



#T's CRITICOS
# t critico a 1% de nivel de significancia, para 3 graus de liberdade
nivel_significancia_1 <- 0.01

t_critico_1 <- qt(1 - nivel_significancia_1, df = gl)
t_critico_1

# t critico a 5% de nivel de significancia, para 3 graus de liberdade
nivel_significancia_5 <- 0.05

t_critico_5 <- qt(1 - nivel_significancia_5, df = gl)
t_critico_5

# t critico a 10% de nivel de significancia, para 3 graus de liberdade
nivel_significancia_10 <- 0.1

t_critico_10 <- qt(1 - nivel_significancia_10, df = gl)
t_critico_10



#TESTES DE HIP?TESES
#para o intercepto, esperamos que seja positivo, portanto fazemos um teste unicaudal
# Ho: b0 == 0
# H1: b0 > 0

  t_intercept <- t_coeficientes$`coef(reg1)`[1]
  #1%
  if (t_intercept > t_critico_1) {
    print(paste(rownames(t_coeficientes)[1], "estatisticamente significante a 1%"))
  } else {
    print(paste(rownames(t_coeficientes)[1], "nao estatisticamente significante a 1%"))
  }
  #5%
  if (t_intercept > t_critico_5) {
    print(paste(rownames(t_coeficientes)[1], "estatisticamente significante a 5%"))
  } else {
    print(paste(rownames(t_coeficientes)[1], "nao estatisticamente significante a 5%"))
  }
  #10%
  if (t_intercept > t_critico_10) {
    print(paste(rownames(t_coeficientes)[1], "estatisticamente significante a 10%"))
  } else {
    print(paste(rownames(t_coeficientes)[1], "nao estatisticamente significante a 10%"))
  }
  

#para SalarioMinimo, esperamos que seja negativo, portanto fazemos um teste unicaudal
# Ho: b1 == 0
# H1: b1 < 0

t_salariominimo <- t_coeficientes$`coef(reg1)`[2]

if (t_salariominimo < -t_critico_1) {
  print(paste(rownames(t_coeficientes)[2], "estatisticamente significante a 1%"))
} else {
  print(paste(rownames(t_coeficientes)[2], "nao estatisticamente significante a 1%"))
}

if (t_salariominimo < -t_critico_5) {
  print(paste(rownames(t_coeficientes)[2], "estatisticamente significante a 5%"))
} else {
  print(paste(rownames(t_coeficientes)[2], "nao estatisticamente significante a 5%"))
}

if (t_salariominimo < -t_critico_10) {
  print(paste(rownames(t_coeficientes)[2], "estatisticamente significante a 10%"))
} else {
  print(paste(rownames(t_coeficientes)[2], "nao estatisticamente significante a 10%"))
}

#para taxadesemprego, esperamos que seja positivo, portanto fazemos um teste unicaudal
# Ho: b2 == 0
# H1: b2 > 0

t_taxadesemprego <- t_coeficientes$`coef(reg1)`[3]

if (t_taxadesemprego > t_critico_1) {
  print(paste(rownames(t_coeficientes)[3], "estatisticamente significante a 1%"))
} else {
  print(paste(rownames(t_coeficientes)[3], "nao estatisticamente significante a 1%"))
}

if (t_taxadesemprego > t_critico_5) {
  print(paste(rownames(t_coeficientes)[3], "estatisticamente significante a 5%"))
} else {
  print(paste(rownames(t_coeficientes)[3], "nao estatisticamente significante a 5%"))
}

if (t_taxadesemprego > t_critico_10) {
  print(paste(rownames(t_coeficientes)[3], "estatisticamente significante a 10%"))
} else {
  print(paste(rownames(t_coeficientes)[3], "nao estatisticamente significante a 10%"))
}


#para GrauInformalidade, esperamos que seja positivo, portanto fazemos um teste unicaudal
# Ho: b3 == 0
# H1: b3 > 0

t_grauinformalidade <- t_coeficientes$`coef(reg1)`[4]

if (t_grauinformalidade > t_critico_1) {
  print(paste(rownames(t_coeficientes)[4], "estatisticamente significante a 1%"))
} else {
  print(paste(rownames(t_coeficientes)[4], "nao estatisticamente significante a 1%"))
}

if (t_grauinformalidade  > t_critico_5) {
  print(paste(rownames(t_coeficientes)[4], "estatisticamente significante a 5%"))
} else {
  print(paste(rownames(t_coeficientes)[4], "nao estatisticamente significante a 5%"))
}

if (t_grauinformalidade > t_critico_10) {
  print(paste(rownames(t_coeficientes)[4], "estatisticamente significante a 10%"))
} else {
  print(paste(rownames(t_coeficientes)[4], "nao estatisticamente significante a 10%"))
}


#para anosestudo, esperamos que seja negativo, portanto fazemos um teste unicaudal
# Ho: b4 == 0
# H1: b4 < 0

t_anosestudo <- t_coeficientes$`coef(reg1)`[5]

if (t_anosestudo < -t_critico_1) {
  print(paste(rownames(t_coeficientes)[5], "estatisticamente significante a 1%"))
} else {
  print(paste(rownames(t_coeficientes)[5], "nao estatisticamente significante a 1%"))
}

if (t_anosestudo < -t_critico_5) {
  print(paste(rownames(t_coeficientes)[5], "estatisticamente significante a 5%"))
} else {
  print(paste(rownames(t_coeficientes)[5], "nao estatisticamente significante a 5%"))
}

if (t_anosestudo < -t_critico_10) {
  print(paste(rownames(t_coeficientes)[5], "estatisticamente significante a 10%"))
} else {
  print(paste(rownames(t_coeficientes)[5], "nao estatisticamente significante a 10%"))
}


#para coeficientegini, esperamos que seja positivo, portanto fazemos um teste unicaudal
# Ho: b5 == 0
# H1: b5 > 0

t_coeficientegini <- t_coeficientes$`coef(reg1)`[6]

if (t_coeficientegini > t_critico_1) {
  print(paste(rownames(t_coeficientes)[6], "estatisticamente significante a 1%"))
} else {
  print(paste(rownames(t_coeficientes)[6], "nao estatisticamente significante a 1%"))
}

if (t_coeficientegini  > t_critico_5) {
  print(paste(rownames(t_coeficientes)[6], "estatisticamente significante a 5%"))
} else {
  print(paste(rownames(t_coeficientes)[6], "nao estatisticamente significante a 5%"))
}

if (t_coeficientegini > t_critico_10) {
  print(paste(rownames(t_coeficientes)[6], "estatisticamente significante a 10%"))
} else {
  print(paste(rownames(t_coeficientes)[6], "nao estatisticamente significante a 10%"))
}

#para ndomiciliospobres, esperamos que seja positivo, portanto fazemos um teste unicaudal
# Ho: b6 == 0
# H1: b6 > 0

t_ndomiciliospobres <- t_coeficientes$`coef(reg1)`[7]

if (t_ndomiciliospobres > t_critico_1) {
  print(paste(rownames(t_coeficientes)[7], "estatisticamente significante a 1%"))
} else {
  print(paste(rownames(t_coeficientes)[7], "nao estatisticamente significante a 1%"))
}

if (t_ndomiciliospobres  > t_critico_5) {
  print(paste(rownames(t_coeficientes)[7], "estatisticamente significante a 5%"))
} else {
  print(paste(rownames(t_coeficientes)[7], "nao estatisticamente significante a 5%"))
}

if (t_ndomiciliospobres > t_critico_10) {
  print(paste(rownames(t_coeficientes)[7], "estatisticamente significante a 10%"))
} else {
  print(paste(rownames(t_coeficientes)[7], "nao estatisticamente significante a 10%"))
}


#para bolsafamilia, esperamos que seja negativo, portanto fazemos um teste unicaudal
# Ho: b8 == 0
# H1: b8 < 0

t_bolsafamilia <- t_coeficientes$`coef(reg1)`[9]

if (t_bolsafamilia < -t_critico_1) {
  print(paste(rownames(t_coeficientes)[9], "estatisticamente significante a 1%"))
} else {
  print(paste(rownames(t_coeficientes)[9], "nao estatisticamente significante a 1%"))
}

if (t_bolsafamilia < -t_critico_5) {
  print(paste(rownames(t_coeficientes)[9], "estatisticamente significante a 5%"))
} else {
  print(paste(rownames(t_coeficientes)[9], "nao estatisticamente significante a 5%"))
}

if (t_bolsafamilia < -t_critico_10) {
  print(paste(rownames(t_coeficientes)[9], "estatisticamente significante a 10%"))
} else {
  print(paste(rownames(t_coeficientes)[9], "nao estatisticamente significante a 10%"))
}


colnames(t_coeficientes)[1] <- 'Estatísticas t'

modelo <- reg1 
plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos", main = "Análise de Resíduos") # Plotar uma linha horizontal em zero para facilitar a visualização abline(h = 0, col = "red")




modelo <- lm(n_ocorrencias ~ Salario_Minimo + TaxaDesemprego + GrauInformalidade + AnosEstudo + CoeficienteGini + NDomiciliosPobres + PIB_SP + BolsaFamilia, data = df) # Gráfico de resíduos versus valores ajustados plot(modelo$fitted.values, modelo$residuals, xlab = "Valores Ajustados", ylab = "Resíduos", main = "Gráfico de Resíduos versus Valores Ajustados") # Gráfico de resíduos versus variáveis independentes par(mfrow = c(3, 3)) # Configurar layout para exibir vários gráficos em uma grade # Gráficos de dispersão dos resíduos versus cada variável independente plot(df$Salario_Minimo, modelo$residuals, xlab = "Salário Mínimo", ylab = "Resíduos", main = "Resíduos versus Salário Mínimo") plot(df$TaxaDesemprego, modelo$residuals, xlab = "Taxa de Desemprego", ylab = "Resíduos", main = "Resíduos versus Taxa de Desemprego") plot(df$GrauInformalidade, modelo$residuals, xlab = "Grau de Informalidade", ylab = "Resíduos", main = "Resíduos versus Grau de Informalidade") # Repita o processo para as outras variáveis independentes # Restaurar o layout padrão de plotagem par(mfrow = c(1, 1))

#http://127.0.0.1:24537/graphics/60a404b4-0366-4a64-bcfb-f1754794394e.png
# Gráficos de dispersão dos resíduos versus cada variável independente 
plot(df$Salario_Minimo, modelo$residuals, xlab = "Salario Minimo", ylab = "Residuos", main = "Residuos versus Salário Mínimo") plot(df$TaxaDesemprego, modelo$residuals, xlab = "Taxa de Desemprego", ylab = "Residuos", main = "Residuos versus Taxa de Desemprego") plot(df$GrauInformalidade, modelo$residuals, xlab = "Grau de Informalidade", ylab = "Residuos", main = "Residuos versus Grau de Informalidade") # Repita o processo para as outras variáveis independentes # Restaurar o layout padrão de plotagem par(mfrow = c(1, 1))

# Gráfico de resíduos versus variáveis independentes 
par(mfrow = c(3, 3)) # Configurar layout para exibir vários gráficos em uma grade 
# Gráficos de dispersão dos resíduos versus cada variável independente 
plot(df$Salario_Minimo, modelo$residuals, xlab = "Salário Mínimo", ylab = "Resíduos", main = "Resíduos versus Salário Mínimo") plot(df$TaxaDesemprego, modelo$residuals, xlab = "Taxa de Desemprego", ylab = "Resíduos", main = "Resíduos versus Taxa de Desemprego") plot(df$GrauInformalidade, modelo$residuals, xlab = "Grau de Informalidade", ylab = "Resíduos", main = "Resíduos versus Grau de Informalidade") 
# Repita o processo para as outras variáveis independentes # Restaurar o layout padrão de plotagem par(mfrow = c(1, 1))
df
plot(df$Ano, df$n_ocorrencias, xlab = "Ano", ylab = "Número de Ocorrências", pch = 20, main = "Regressão Linear Múltipla") abline(lm(n_ocorrencias ~ Salario_Minimo + TaxaDesemprego + GrauInformalidade + AnosEstudo + CoeficienteGini + NDomiciliosPobres + PIB_SP + BolsaFamilia, data = df))
modelo <- lm(n_ocorrencias ~ Salario_Minimo + TaxaDesemprego + GrauInformalidade + AnosEstudo + CoeficienteGini + NDomiciliosPobres + PIB_SP + BolsaFamilia, data = df) # Plotar o gráfico de dispersão dos dados observados
plot(df$Ano, df$n_ocorrencias, xlab = "Ano", ylab = "Número de Ocorrências", pch = 20, col = "blue", main = "Regressão Linear Múltipla") # Adicionar a linha da regressão linear múltipla abline(modelo, col = "red")
# Ajustar o modelo de regressão linear múltipla 
modelo <- lm(n_ocorrencias ~ Salario_Minimo + TaxaDesemprego + GrauInformalidade + AnosEstudo + CoeficienteGini + NDomiciliosPobres + PIB_SP + BolsaFamilia, data = df) # Gerar uma sequência de valores de ano
anos <- seq(1999, 2014, by = 1) # Obter as previsões do modelo para os valores de ano 
previsoes <- predict(modelo, newdata = data.frame(Ano = anos)) 
# Plotar o gráfico de dispersão dos dados observados
plot(df$Ano, df$n_ocorrencias, xlab = "Ano", ylab = "Número de Ocorrências", pch = 20, col = "blue", main = "Regressão Linear Múltipla") # Adicionar a linha da regressão linear múltipla lines(anos, previsoes, col = "red")
# Ajustar o modelo de regressão linear múltipla
modelo <- lm(n_ocorrencias ~ Salario_Minimo + TaxaDesemprego + GrauInformalidade + AnosEstudo + CoeficienteGini + NDomiciliosPobres + PIB_SP + BolsaFamilia, data = df) # Gerar uma sequência de valores de ano 
anos <- seq(1999, 2014, by = 1) # Criar um novo data frame para fazer as previsões 
dados_previsao <- data.frame(Ano = anos, Salario_Minimo = mean(df$Salario_Minimo), TaxaDesemprego = mean(df$TaxaDesemprego), GrauInformalidade = mean(df$GrauInformalidade), AnosEstudo = mean(df$AnosEstudo), CoeficienteGini = mean(df$CoeficienteGini), NDomiciliosPobres = mean(df$NDomiciliosPobres), PIB_SP = mean(df$PIB_SP), BolsaFamilia = mean(df$BolsaFamilia)) # Obter as previsões do modelo para os valores de 
dados_previsao <- data.frame(Salario_Minimo = mean(df$Salario_Minimo), TaxaDesemprego = mean(df$TaxaDesemprego), GrauInformalidade = mean(df$GrauInformalidade), AnosEstudo = mean(df$AnosEstudo), CoeficienteGini = mean(df$CoeficienteGini), NDomiciliosPobres = mean(df$NDomiciliosPobres), PIB_SP = mean(df$PIB_SP), BolsaFamilia = mean(df$BolsaFamilia), Ano = anos)
# Plotar o gráfico de dispersão dos dados observados 
plot(df$Ano, df$n_ocorrencias, xlab = "Ano", ylab = "Número de Ocorrências", pch = 20, col = "blue", main = "Regressão Linear Múltipla") # Adicionar a linha da regressão linear múltipla lines(anos, previsoes, col = "red") # Adicionar as legendas dos anos no eixo x axis(side = 1, at = anos, labels = anos)

matriz_cor <- cor(df[, c("n_ocorrencias", "Salario_Minimo", "TaxaDesemprego", "GrauInformalidade", "AnosEstudo", "CoeficienteGini", "NDomiciliosPobres", "PIB_SP", "BolsaFamilia")]) # Plotar a matriz de correlação 

heatmap(matriz_cor, col = colorRampPalette(c("blue", "white", "red"))(1000), main = "Mapa de Calor - Matriz de Correlação", xlab = "Variáveis", ylab = "Variáveis")
df

