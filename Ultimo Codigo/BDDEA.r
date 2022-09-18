library("tidyverse")
library("Benchmarking")
library("AER")
library("truncnorm")
library("xlsx")
#baixar site




setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


BDDEA    <- readRDS("BDDEAF.rds")
BDDEA <- BDDEA %>% mutate(ID=row_number())
BDDEA<-BDDEA %>% mutate(alunosap= round(QTALUNOS*(MdAprov/100)))
BDDEA<-BDDEA %>% mutate(alunosab= round(QTALUNOS*(MdAba1/100)))
BDDEA<-BDDEA %>% mutate(alunosrep= round(QTALUNOS*(MdREPROB1/100))) %>% 
  mutate(alunosrep = ifelse(alunosrep == 0,  0.001, alunosrep)) %>% 
  mutate(alunosab = ifelse(alunosab == 0,  0.001, alunosab)) %>% 
  filter(ID != 26)
summary(BDDEA)
head(BDDEA, 5)




########################################
# Estagio 1
########################################








######## FASE 1 descritiva

InsumosF1<-cbind(BDDEA$ServMD, BDDEA$QTsalasMD) 
ProdutosF1<-cbind(BDDEA$QTALUNOS)


# Gráfico (isoquanta) da tecnologia CRS do 1º Estágio
I1<-matrix(c(BDDEA$ServMD) ,ncol=1) # divisão de X1/Z
I2<- matrix(c(BDDEA$QTsalasMD),ncol=1) # divisão de X2/Z
dea.plot.isoquant(I1, I2, RTS="crs", txt=1:dim(InsumosF1)[1])
### ainda sem indice de eff apenas pode se notar possivel indicativo


















######## FASE 2 descritiva

InsumosF2<-cbind(BDDEA$QTALUNOS) 

### usando a inversa pq esse produtos sao indesejados assim tem que minimizar


ProdutosF2<-cbind(BDDEA$alunosap)


# Gráfico (fronteira) da tecnologia vRS do 2º Estágio
I1<-matrix(c(ProdutosF2[,1]/BDDEA$QTALUNOS) ,ncol=1) # divisão de z1/x1
# I2<- matrix(c(ProdutosF2[,2]/BDDEA$QTALUNOS),ncol=1) # divisão de z2/x1
 dea.plot.frontier(InsumosF2, ProdutosF2, RTS="vrs", txt=1:154)
 dea.plot.frontier(InsumosF2, ProdutosF2, RTS="crs", txt=1:154)
 

### ainda sem indice de eff apenas pode se notar possivel indicativo


######## FASE 3 descritiva

 

 
InsumosF3<-cbind(BDDEA$alunosrep, BDDEA$alunosab) 

### usando a inversa pq esse produtos sao indesejados assim tem que minimizar

ProdutosF3<-cbind(BDDEA$MediaEnem)


# Gráfico (isoquanta) da tecnologia CRS do 3º Estágio
I1<-matrix(c(BDDEA$MdREPROB1/BDDEA$MediaEnem) ,ncol=1) # divisão de X1/Z
I2<- matrix(c(BDDEA$MdAba1/BDDEA$MediaEnem),ncol=1) # divisão de X2/Z
dea.plot.isoquant(I1, I2, RTS="crs", txt=1:154)




### ainda sem indice de eff apenas pode se notar possivel indicativo





#orientacao saida pq quero maximizar o numeros de alunos , modelo retorno constante de escala(crs) pq ele e mais restritivo compara geral.

estagio1 <- Benchmarking::dea(InsumosF1, ProdutosF1, RTS = "CRS", ORIENTATION = "out", SLACK = TRUE)
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
#era antes round(1/BDDEA$eff2, 4) ###verificando a inversa por causa dos valores altos

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

estagio3 <- dea(InsumosF3, ProdutosF3, RTS = "CRS", ORIENTATION = "out")
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


####################   DESCRITIVA

summary(BDDEA)

BDDEAOUT    <- readRDS("BDDEAF.rds")
BDDEAOUT <- BDDEAOUT %>% mutate(ID=row_number())
BDDEAOUT<-BDDEAOUT %>% mutate(alunosap= round(QTALUNOS*(MdAprov/100)))
BDDEAOUT<-BDDEAOUT %>% mutate(alunosab= round(QTALUNOS*(MdAba1/100)))
BDDEAOUT<-BDDEAOUT %>% mutate(alunosrep= round(QTALUNOS*(MdREPROB1/100))) %>% 
  mutate(alunosrep = ifelse(alunosrep == 0,  0.001, alunosrep)) %>% 
  mutate(alunosab = ifelse(alunosab == 0,  0.001, alunosab)) %>% 
  filter(ID == 26)


table(BDDEA$Dependência.Administrativa,(BDDEA$ServMD<7))
table(BDDEA$Dependência.Administrativa,(BDDEA$QTALUNOS>990))

table(BDDEA$Dependência.Administrativa,(BDDEA$MdAprov >96.43))
table(BDDEA$Dependência.Administrativa,(BDDEA$MdAba1>3.533))
table(BDDEA$Dependência.Administrativa,(BDDEA$MdREPROB1>10.3))


table(BDDEA$Dependência.Administrativa,(BDDEA$QTsalasMD>17))


sd(BDDEA$ServMD)
sd(BDDEA$QTALUNOS)
sd(BDDEA$QTsalasMD)
sd(BDDEA$MediaEnem)
sd(BDDEA$alunosap)
sd(BDDEA$alunosab)
sd(BDDEA$alunosrep)





# Identificação dos outliers da 1º Estágio usando o volume como parametro
library("FEAR") 
###################26 foi retirada como outlier
tap1<-ap(X=t(InsumosF1), Y=t(ProdutosF1), NDEL=15)
print(cbind(tap1$imat,tap1$r0), na.print = "", digits = 2)
outlier.ap.plot(tap1$ratio)


# unidades outlier 1 [ 68   26   97   94   83 ]
OUT1<- c(68, 26, 97, 94, 83)

T1<- BDDEA %>% filter(ID %in% OUT1) %>% select(est_crs_super1, est_vrs_super1)

T1

summary(BDDEA$est_crs_super1)
summary(BDDEA$est_vrs_super1)


# Identificação dos outliers da 2º Estágio
tap2<-ap(X=t(InsumosF2), Y=t(ProdutosF2), NDEL=15)
print(cbind(tap2$imat,tap2$r0), na.print = "", digits = 2)
outlier.ap.plot(tap2$ratio)

# unidades outlier 2 {97  103   80  123   68  121   37   79 }



OUT2<- c(97, 103, 80, 123, 68, 121, 37, 79)

T2<- BDDEA %>% filter(ID %in% OUT2) %>% select(est_crs_super2, est_vrs_super2)

T2

summary(BDDEA$est_crs_super2)
summary(BDDEA$est_vrs_super2)


# Identificação dos outliers da 3º Estágio
tap3<-ap(X=t(InsumosF3), Y=t(ProdutosF3), NDEL=15)
print(cbind(tap3$imat,tap3$r0), na.print = "", digits = 2)
outlier.ap.plot(tap3$ratio)


# unidades outlier 3 {63   97  106   79   68  103  121   54  117  }

OUT3<- c(78, 75, 80, 62, 123, 26, 63, 97, 106, 79, 68, 103, 121, 54, 117)

T3 <- BDDEA %>% filter(ID %in% OUT3) %>% select(est_crs_super3, est_vrs_super3)

T3

summary(BDDEA$est_crs_super3)
summary(BDDEA$est_vrs_super3)




#a partir da super eff se removeu a dmu 26

# Teste estatístico usando bootstrapping das premissas do modelo (CVS vs VRS) do 1º Estágio
nrep<-2000
e<-1/estagio1$eff
ev<-1/estagio1rs$eff
sum(e)/sum(ev)
InsumosF1=t(InsumosF1)
ProdutosF1=t(ProdutosF1)
Bc <- boot.sw98(InsumosF1,ProdutosF1,NREP=nrep,RTS=3)
Bv <- boot.sw98(InsumosF1,ProdutosF1,NREP=nrep,RTS=1,XREF=InsumosF1,YREF=ProdutosF1,DREF=1/e)
s <- colSums(1/Bc$boot)/colSums(1/Bv$boot)

critValue(s,0.05)


###########
# A estimativa de S é 0.8948057
#  Ho: T is CRS; 
# ou seja não rejeitamos que existam retornos constantes de escala pois S=0.8109906. 
# Se fôssemos fazer mais cálculos sob este modelo, assumiríamos, 
# portanto, retornos constantes de escala e usaríamos uma tecnologia CRS.
# Em outras palavras, se o valor estimado de S for menor que o valor crítico 0.8109906, 
# rejeitamos a hipótese. Correspondentemente, como a estimativa de S=0.8948057, 
# não rejeitamos a hipótese.
###             CRS               #######










# Teste estatístico usando bootstrapping das premissas do modelo (CVS vs VRS) do 2º Estágio
nrep<-2000
e<-1/estagio2$eff
ev<-1/estagio2rs$eff
sum(e)/sum(ev)
InsumosF2=t(InsumosF2)
ProdutosF2=t(ProdutosF2)
Bc <- boot.sw98(InsumosF2,ProdutosF2,NREP=nrep,RTS=3)
Bv <- boot.sw98(InsumosF2,ProdutosF2,NREP=nrep,RTS=1,XREF=InsumosF2,YREF=ProdutosF2,DREF=1/e)
s <- colSums(1/Bc$boot)/colSums(1/Bv$boot)

critValue(s,0.05)

############### vrs pois s < crit






# Teste estatístico usando bootstrapping das premissas do modelo (CVS vs VRS) do 3º Estágio
nrep<-2000
e<-1/estagio3$eff
ev<-1/estagio3rs$eff
sum(e)/sum(ev)
InsumosF3=t(InsumosF3)
ProdutosF3=t(ProdutosF3)
Bc <- boot.sw98(InsumosF3,ProdutosF3,NREP=nrep,RTS=3)
Bv <- boot.sw98(InsumosF3,ProdutosF3,NREP=nrep,RTS=1,XREF=InsumosF3,YREF=ProdutosF3,DREF=1/e)
s <- colSums(1/Bc$boot)/colSums(1/Bv$boot)

critValue(s,0.05)

############### crs pois s > crit



### networking e multiplicar o inverso das eficiencias 


### Anexos


ANEXOA<- BDDEA %>% select(ID, NO_ENTIDADE, est_crs_super1, est_vrs_super1, est_crs_super2, est_vrs_super2, est_crs_super3, est_vrs_super3)

write.xlsx(ANEXOA, "ANEXOA.xlsx")



# Intervalos de confiança 1º Estágio
I<-InsumosF1
O<-ProdutosF1
d1E<-FEAR::dea(I,O,RTS = 3, ORIENTATION = 1) # com CRS orientado aos outputs
b1E<-boot.sw98(I,O, RTS = 3, ORIENTATION = 1, NREP = 2000)
plot(b1E$dhat,ylim = c(.5,10), main = NA, xlab = "DMU",ylab ="Efficiency CRS Otput Orientation")
points(b1E$dhat.bc, pch=5)
for(i in 1:153) lines(rep(i,2), b1E$conf.int[i,], type="o",pch=3)


# Intervalos de confiança 2º Estágio
I2<-InsumosF2
O2<-ProdutosF2
d2E<-FEAR::dea(I2,O2,RTS = 1, ORIENTATION = 1) # com VRS orientado aos outputs
b2E<-boot.sw98(I2, O2, RTS = 1, ORIENTATION = 1, NREP = 2000)
plot(b2E$dhat,ylim = c(.5,2), main = NA, xlab = "DMU",ylab ="Efficiency CRS Output Orientation")
points(b2E$dhat.bc, pch=5)
for(i in 1:153) lines(rep(i,2), b2E$conf.int[i,], type="o",pch=3)

# Intervalos de confiança 3º Estágio
I3<-InsumosF3
O3<-ProdutosF3
d3E<-FEAR::dea(I3,O3,RTS = 3, ORIENTATION = 1) # com CRS orientado aos outputs
b3E<-boot.sw98(I3,O3, RTS = 3, ORIENTATION = 1, NREP = 2000)
plot(b3E$dhat,ylim = c(.5,3), main = NA, xlab = "DMU",ylab ="Efficiency CRS Otput Orientation")
points(b3E$dhat.bc, pch=5)
for(i in 1:153) lines(rep(i,2), b3E$conf.int[i,], type="o",pch=3)

ANEXOB<- BDDEA %>% select(ID, NO_ENTIDADE, Dependência.Administrativa) %>% cbind(b1E$dhat, b1E$dhat.bc, b1E$conf.int) %>% cbind(b2E$dhat, b2E$dhat.bc, b2E$conf.int) %>% cbind(b3E$dhat, b3E$dhat.bc, b3E$conf.int)

ANEXOBS<- ANEXOB %>% select(ID, NO_ENTIDADE, Dependência.Administrativa, `b1E$dhat`, `b1E$dhat.bc`, `b2E$dhat`, `b2E$dhat.bc`, `b3E$dhat`, `b3E$dhat.bc`) 
summary(ANEXOBS)




ANEXOBS1 <- ANEXOBS %>% mutate(A1 = (1/(ANEXOBS$`b1E$dhat`)),
                                A2 = (1/(ANEXOBS$`b1E$dhat.bc`))) %>% 
                        mutate(B1 = (1/(ANEXOBS$`b2E$dhat`)),
                                B2 = (1/(ANEXOBS$`b2E$dhat.bc`))) %>% 
                        mutate(C1 = (1/(ANEXOBS$`b3E$dhat`)),
                                C2 = (1/(ANEXOBS$`b3E$dhat.bc`))) %>% select(ID, NO_ENTIDADE, Dependência.Administrativa, A1,A2,B1,B2,C1,C2) %>% 
                        mutate(Dependência.Administrativa = case_when(Dependência.Administrativa == "Estadual" ~ "Publica", 
                                                                       Dependência.Administrativa == "Federal" ~ "Publica",
                                                                       Dependência.Administrativa == "Privada" ~ "Privada"))









Pu <- ANEXOBS1 %>% filter(Dependência.Administrativa== "Publica")

summary(Pu)

sd(Pu$A2)

sd(Pu$B2)

sd(Pu$C2)

PI <- ANEXOBS1 %>% filter(Dependência.Administrativa== "Privada")

summary(PI)


sd(PI$A2)

sd(PI$B2)

sd(PI$C2)



summary(ANEXOBS1)

sd(ANEXOBS1$A1)
sd(ANEXOBS1$A2)


sd(ANEXOBS1$B1)
sd(ANEXOBS1$B2)

sd(ANEXOBS1$C1)
sd(ANEXOBS1$C2)


boxplot(ANEXOBS1$A1, ANEXOBS1$A2, names = c("Eficiência Deterministica","Eficiência com correção de viés"))
boxplot(ANEXOBS1$B1, ANEXOBS1$B2, names = c("Eficiência Deterministica","Eficiência com correção de viés"))
boxplot(ANEXOBS1$C1, ANEXOBS1$C2, names = c("Eficiência Deterministica","Eficiência com correção de viés"))


write.xlsx(ANEXOB, "ANEXOB.xlsx")



ANEXOC <- ANEXOBS1 %>%  mutate(EF_Final = round((A2*B2*C2), 7)) %>% 
                        select(ID, NO_ENTIDADE, Dependência.Administrativa,EF_Final) 


boxplot(ANEXOC$EF_Final ~ ANEXOC$Dependência.Administrativa,
        xlab = "Escolas",
        ylab = "Eficiência de Shephard")

PU1 <- ANEXOC %>% filter(Dependência.Administrativa== "Publica")
summary(PU1)
sd(PU1$EF_Final)
PI1 <- ANEXOC %>% filter(Dependência.Administrativa== "Privada")
summary(PI1)
sd(PI1$EF_Final)

write.xlsx(ANEXOC, "ANEXOC.xlsx")
