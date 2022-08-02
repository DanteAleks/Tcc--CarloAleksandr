library("tidyverse")
library("Benchmarking")
library("AER")
library("truncnorm")
library("FEAR") #baixar site




setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



BDDEA    <- readRDS("BDDEAF.rds")

summary(BDDEA)
head(BDDEA, 5)

######## FASE 1 descritiva

InsumosF1<-cbind(BDDEA$ServMD, BDDEA$QTsalasMD) 
ProdutosF1<-cbind(BDDEA$QTALUNOS)


# Gráfico (isoquanta) da tecnologia CRS do 1º Estágio
I1<-matrix(c(BDDEA$ServMD/BDDEA$QTALUNOS) ,ncol=1) # divisão de X1/Z
I2<- matrix(c(BDDEA$QTsalasMD/BDDEA$QTALUNOS),ncol=1) # divisão de X2/Z
dea.plot.isoquant(I1, I2, RTS="crs", txt=1:154)

### ainda sem indice de eff apenas pode se notar possivel indicativo


######## FASE 2 descritiva

InsumosF2<-cbind(BDDEA$QTALUNOS) 

### usando a inversa pq esse produtos sao indesejados assim tem que minimizar

ProdutosF2<-cbind( 1/(BDDEA$MdREPROB1), 1/(BDDEA$MdAba1))


# Gráfico (isoquanta) da tecnologia CRS do 2º Estágio
I1<-matrix(c(ProdutosF2[,1]/BDDEA$QTALUNOS) ,ncol=1) # divisão de z1/x1
I2<- matrix(c(ProdutosF2[,2]/BDDEA$QTALUNOS),ncol=1) # divisão de z2/x1
dea.plot.transform(I1, I2, RTS="crs", txt=1:154)


## 131 e 142 nao reprovaram/abandonaram mas a quantidade de alunos do 131 e menor 


### ainda sem indice de eff apenas pode se notar possivel indicativo


######## FASE 3 descritiva

InsumosF3<-cbind(BDDEA$MdREPROB1, BDDEA$MdAba1) 

### usando a inversa pq esse produtos sao indesejados assim tem que minimizar

ProdutosF3<-cbind(BDDEA$MediaEnem)


# Gráfico (isoquanta) da tecnologia CRS do 3º Estágio
I1<-matrix(c(BDDEA$MdREPROB1/BDDEA$MediaEnem) ,ncol=1) # divisão de X1/Z
I2<- matrix(c(BDDEA$MdAba1/BDDEA$MediaEnem),ncol=1) # divisão de X2/Z
dea.plot.isoquant(I1, I2, RTS="crs", txt=1:154)




### ainda sem indice de eff apenas pode se notar possivel indicativo





#orientacao saida pq quero maximizar o numeros de alunos , modelo retorno constante de escala(crs) pq ele e mais restritivo compara geral.

estagio1 <- dea(InsumosF1, ProdutosF1, RTS = "CRS", ORIENTATION = "out", SLACK = TRUE)
BDDEA$eff1<-eff(estagio1)


#eff maior que 1 pq e orientada ao produto
summary(BDDEA$eff1)
table(BDDEA$eff1 == 1)

# Gráfico de Histograma/densidade da Eficiência
hist(BDDEA$eff1, xlab = "Eficiência CRS 1° Estagio", main = NA, probability = TRUE, breaks = 10)
lines(density(BDDEA$eff1))


# Calculando os Benchmarks para as DMUs ineficientes
Bench_crs1<-peers(estagio1)
peers(estagio1, NAMES=TRUE)
print(peers(estagio1, NAMES=TRUE), quote=FALSE)
table(Bench_crs1)


# Calculando a super-eficiência que para calcular quanto que os eficiente podem ter de alunos e ainda continuar eff no caso 26 ele podia ter so 70% dos alunos
est_crs_super1 <- sdea(InsumosF1,ProdutosF1, RTS="crs", ORIENTATION="out")
BDDEA$est_crs_super1<-est_crs_super1$eff

# Histograma da Super_Eficiência
hist(BDDEA$est_crs_super1, xlab = "Super-Efficiency CRS 1 Stage", main = NA)


###### VRS retornos variaveis

#orientacao saida pq quero maximizar o numeros de alunos , modelo retorno constante de escala(crs) pq ele e mais restritivo compara geral.

estagio1rs <- dea(InsumosF1, ProdutosF1, RTS = "VRS", ORIENTATION = "out", SLACK = TRUE)
BDDEA$eff1rs<-eff(estagio1rs)


#eff maior que 1 pq e orientada ao produto
summary(BDDEA$eff1rs)
table(BDDEA$eff1rs == 1)

# Gráfico de Histograma/densidade da Eficiência
hist(BDDEA$eff1rs, xlab = "Eficiência VRS 1° Estagio", main = NA, probability = TRUE, breaks = 10)
lines(density(BDDEA$eff1rs))


# Calculando os Benchmarks para as DMUs ineficientes
Bench_vrs1<-peers(estagio1rs)
peers(estagio1rs, NAMES=TRUE)
print(peers(estagio1rs, NAMES=TRUE), quote=FALSE)
table(Bench_vrs1)


# Calculando a super-eficiência que para calcular quanto que os eficiente podem ter de alunos e ainda continuar eff no caso 85 ele podia ter so 85% dos alunos
est_vrs_super1 <- sdea(InsumosF1,ProdutosF1, RTS="vrs", ORIENTATION="out")
BDDEA$est_vrs_super1<-est_vrs_super1$eff

# Histograma da Super_Eficiência
hist(BDDEA$est_vrs_super1, xlab = "Super Eficiência VRS 1° Estagio", main = NA)



##################  2° Estagio

#orientacao saida pq quero minimizar o numeros de abandono e reprovacao , modelo retorno constante de escala(crs) pq ele e mais restritivo compara geral.

estagio2 <- dea(InsumosF2, ProdutosF2, RTS = "CRS", ORIENTATION = "out", SLACK = TRUE)
BDDEA$eff2<-eff(estagio2)
round(1/BDDEA$eff2, 4) ###verificando a inversa por causa dos valores altos

#eff maior que 1 pq e orientada ao produto
summary(BDDEA$eff2)
table(BDDEA$eff2 == 1)
round(BDDEA$eff2, 5)
# Gráfico de Histograma/densidade da Eficiência
hist(BDDEA$eff2, xlab = "Eficiência CRS 2° Estagio", main = NA, probability = TRUE, breaks = 30)
lines(density(BDDEA$eff2))


# Calculando os Benchmarks para as DMUs ineficientes
Bench_crs2<-peers(estagio2)
peers(estagio2, NAMES=TRUE)
print(peers(estagio2, NAMES=TRUE), quote=FALSE)
table(Bench_crs2)


# Calculando a super-eficiência que para calcular quanto que os eficiente podem ter de alunos e ainda continuar eff no caso 26 ele podia ter so 70% dos alunos
est_crs_super2 <- sdea(InsumosF2,ProdutosF2, RTS="crs", ORIENTATION="out")
BDDEA$est_crs_super2<-est_crs_super2$eff

# Histograma da Super_Eficiência
# hist(BDDEA$est_crs_super2, xlab = "Super Eficiência CRS 2° Estagio", main = NA, breaks = 20)


###### VRS retornos variaveis

#orientacao saida pq quero maximizar o numeros de alunos , modelo retorno constante de escala(crs) pq ele e mais restritivo compara geral.

estagio2rs <- dea(InsumosF2, ProdutosF2, RTS = "VRS", ORIENTATION = "out", SLACK = TRUE)
BDDEA$eff2rs<-eff(estagio2rs)


#eff maior que 1 pq e orientada ao produto
summary(BDDEA$eff2rs)
table(BDDEA$eff2rs == 1)

# Gráfico de Histograma/densidade da Eficiência
hist(BDDEA$eff2rs, xlab = "Eficiência VRS 2° Estagio", main = NA, probability = TRUE, breaks = 20)
lines(density(BDDEA$eff2rs))


# Calculando os Benchmarks para as DMUs ineficientes
Bench_vrs2<-peers(estagio2rs)
peers(estagio2rs, NAMES=TRUE)
print(peers(estagio2rs, NAMES=TRUE), quote=FALSE)
table(Bench_vrs2)


# Calculando a super-eficiência que para calcular quanto que os eficiente podem ter de alunos e ainda continuar eff no caso 85 ele podia ter so 85% dos alunos
est_vrs_super2 <- sdea(InsumosF2,ProdutosF2, RTS="vrs", ORIENTATION="out")
BDDEA$est_vrs_super2<-est_vrs_super2$eff

# Histograma da Super_Eficiência
# hist(BDDEA$est_vrs_super2, xlab = "Super Eficiência VRS 2° Estagio", main = NA , breaks = 20)


##################  3° Estagio

#orientacao saida pq quero minimizar o numeros de abandono e reprovacao , modelo retorno constante de escala(crs) pq ele e mais restritivo compara geral.

estagio3 <- dea(InsumosF3, ProdutosF3, RTS = "CRS", ORIENTATION = "out", SLACK = TRUE)
BDDEA$eff3<-eff(estagio3)
# round(1/BDDEA$eff3, 4) ###verificando a inversa por causa dos valores altos

#eff maior que 1 pq e orientada ao produto
summary(BDDEA$eff3)
table(BDDEA$eff3 == 1)
round(BDDEA$eff3, 5)
# Gráfico de Histograma/densidade da Eficiência
hist(BDDEA$eff3, xlab = "Eficiência CRS 3° Estagio", main = NA, probability = TRUE, breaks = 10)
lines(density(BDDEA$eff3))


# Calculando os Benchmarks para as DMUs ineficientes
Bench_crs3<-peers(estagio3)
peers(estagio3, NAMES=TRUE)
print(peers(estagio3, NAMES=TRUE), quote=FALSE)
table(Bench_crs3)


# Calculando a super-eficiência que para calcular quanto que os eficiente podem ter de alunos e ainda continuar eff no caso 26 ele podia ter so 70% dos alunos
est_crs_super3 <- sdea(InsumosF3,ProdutosF3, RTS="crs", ORIENTATION="out")
BDDEA$est_crs_super3<-est_crs_super3$eff

# Histograma da Super_Eficiência
# hist(BDDEA$est_crs_super3, xlab = "Super Eficiência CRS 3° Estagio", main = NA)


###### VRS retornos variaveis

#orientacao saida pq quero maximizar o numeros de alunos , modelo retorno constante de escala(crs) pq ele e mais restritivo compara geral.

estagio3rs <- dea(InsumosF3, ProdutosF3, RTS = "VRS", ORIENTATION = "out", SLACK = TRUE)
BDDEA$eff3rs<-eff(estagio3rs)


#eff maior que 1 pq e orientada ao produto
summary(BDDEA$eff3rs)
table(BDDEA$eff3rs == 1)

# Gráfico de Histograma/densidade da Eficiência
hist(BDDEA$eff3rs, xlab = "Eficiência VRS 3° Estagio", main = NA, probability = TRUE, breaks = 10)
lines(density(BDDEA$eff3rs))


# Calculando os Benchmarks para as DMUs ineficientes
Bench_vrs3<-peers(estagio3rs)
peers(estagio3rs, NAMES=TRUE)
print(peers(estagio3rs, NAMES=TRUE), quote=FALSE)
table(Bench_vrs3)


# Calculando a super-eficiência que para calcular quanto que os eficiente podem ter de alunos e ainda continuar eff no caso 85 ele podia ter so 85% dos alunos
est_vrs_super3 <- sdea(InsumosF3,ProdutosF3, RTS="vrs", ORIENTATION="out")
BDDEA$est_vrs_super3<-est_vrs_super3$eff

# Histograma da Super_Eficiência
# hist(BDDEA$est_vrs_super3, xlab = "Super Eficiência VRS 3° Estagio", main = NA)









# Identificação dos outliers da 1º Estágio 


#library(FEAR) ###baixar no site
tap<-ap(X=t(x), Y=t(y), NDEL=5)
print(cbind(tap$imat,tap$r0), na.print = "", digits = 2)
outlier.ap.plot(tap$ratio)


### networking e multiplicar o inverso das eficiencias 
#### usar o bootstrap para saber qual modelo usar