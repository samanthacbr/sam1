library(haven)
library(qwraps2)
library(summarytools)
library(devtools)
library(knitr)
library(dplyr)
library(stargazer)
library(lmtest)
library(sandwich)
library(devtools)
library(fansi)
library(RCT)
library(kableExtra)
library(rstatix)
library(data.table)
library(mfx)
library(perm)
library(coin)  
library(magrittr)
library(exactRankTests)
library(powerMediation)
library(tidyverse)
library(ggeffects)
library(ggplot2)
install.packages(kableExtra)
?kable
setwd("~/Maestría/Econometría/Tarea 1")
nam<-read_dta("Names.dta")

attach(nam)

#1
#realidad la asignación de las observaciones al grupo de
#tratamiento o control puede ser determinada por alguno de los controles.

###  preg 2

options(qwraps2_markup = "latex")

our_sum<-list("Licenciatura"=list("media (sd)"= ~qwraps2::mean_sd(college)),
              "Años de experiencia"=list("media (sd)"= ~qwraps2::mean_sd(yearsexp)),
              "Experiencia militar"=list("media (sd)"= ~qwraps2::mean_sd(military)),
              "Email"=list("media (sd)"= ~qwraps2::mean_sd(email)),
              "Hoyos de empleo"=list("media (sd)"= ~qwraps2::mean_sd(empholes)),
              "Honores"=list("media (sd)"= ~qwraps2::mean_sd(honors)),
              "Habilidades computacionales"=list("media (sd)"= ~qwraps2::mean_sd(computerskills)),
              "Habilidades especiales"=list("media (sd)"= ~qwraps2::mean_sd(specialskills)))

whole1 <-summary_table(nam,our_sum)
whole2<- summary_table(nam, summaries=our_sum, by=c("black","high"))
both<-cbind(whole1,whole2)

print(both,rtitle="Tabla 1. Estadísticas descriptivas",
      cnames=c("Todos los CVS", "Nombres blancos","Nombres Afro-Am.",
               "Calidad mayor","Calidad menor"))
knitr::kable(both,caption="Estadísticas descriptivas",col.names=c("Total","Blancos","Afro-am.","Baja calidad","Alta calidad")) 
install.packages("hms")
install.packages("kableExtra",dependencies=TRUE)
# %>% kable_styling(position = "center")

##


#Verificar balance

variables23<-data.frame(college,yearsexp,military,email,empholes,honors,
                       computerskills,specialskills,black)

tabla23<-balance_table(variables23,treatment="black")
         
stargazer(as.data.frame(tabla23),type="text",summary=FALSE,digits=2,title="Tabla de Balance",
          covariate.labels=c( "No.","Variables", "Media control","Media tratamiento", "P-value"))

knitr::kable(tabla23,format="latex",caption="Tabla de balance")

######## preg 3

#Asumiendo que la distribucion de nombres fue aleatoria, 
#da evidencia de si existe discriminacion racial en el call 
#back utilizando: 

#(i) un estimador de Neyman, 

#ATE

ey1<-nam %>%
  filter(black==1)%>%
  pull(call_back)%>%
  mean()

ey0<-nam %>%
  filter(black==0)%>%
  pull(call_back)%>%
  mean()

gate <- ey1 - ey0

#lecuanda


yiT<-call_back[black==1]
yiC<-call_back[black==0]
myiT<-mean(yiT)
myiC<-mean(yiC)

#Tau
(tau<-abs(myiT-myiC))

#varianza

#lecuanda (length=2435)
(vartau<-var(call_back[black==1]/length(yiT))+var(call_back[black==0]/length(yiC)))

#tanteo
(vartau<-var(call_back[black==1]/1440)+var(call_back[black==0])/1440)

sqrt(vartau)

(tstat<-tau/sqrt(vartau))

2*(1-pnorm(tstat))   

#real: 0.007785

# Es equivalente a

t.test(x=call_back[black==1],call_back[black==0],alternative="two.sided",
       mu=0,var.equal = FALSE,conf.level=.95)

#tstat= -4.1147
#tau= -0.03203285

-0.03203285/-4.1147

#sd= 0.007784978
#var=6.060588e-05
##Rechazamos

#d = \frac{m-\mu}{s}
stat.test<-nam %>% t_test(call_back ~ black, detailed = TRUE)

st<-as.data.frame(stat.test)

attach(st)

st1 <- st %>% dplyr::select(estimate, estimate1,estimate2,n1,statistic,p) %>% tibble::rownames_to_column()%>%
  pivot_longer(-rowname) %>% pivot_wider(names_from=rowname,values_from=value)

st1<-as.data.frame(st1)
rownames(st1)<-c("Valor absoluto Diferencia","P(call_back|black=0)","P(call_back|black=1)","N","t-stat","p-value")
View(st1)
st2<- subset(st1,select=-c(name))
View(st2)
knitr::kable(st2,format="latex",caption="Estimador de Neyman")

#%>% kable_styling(position = "center")

#dar el valor de la varianza... tau/t
library("ANOVAreplication")



#(ii) una estimacion de OLS con errores heterocedasticos,


a1<-lm(call_back ~ black, data = nam)
summary(a1)
cov1<-coeftest(a1, vcov = vcovHC(a1, type="HC3"))
robust_se <- sqrt(diag(cov1))

#(iii) una estimacion de OLS agregando controles (ustedes deberan decidir cuales) y
b1<-lm(call_back ~ black + female + chicago + I(female*black) + high + I(high*black), data = nam)
e1<-lm(call_back ~ black + female + high + chicago , data = nam)


summary(b1)
summary(e1)

#(iv) un probit sin controles.


f1 <- glm(call_back ~ black,  family = binomial(link = "probit"),  data = nam)

nam$prob_pred<-predict.glm(f1,type="response")
attach(nam)


#(a) Indica la prueba de hipotesis que estaras contrastando en cada estimacion.


#ols errores hetero: $$CallBack_{i}=\beta_{0}+\beta_{1} Black_i +u_{i}$$

# ols con controles: #$$CallBack_{i}=\beta_{0}+\beta_{1} Black_i +\beta_{2} Female_i+\beta_{3} (Female_i*Black_i)+
#\beta_{4} High_i+\beta_{5} (High_i*Black_i)+u_{i}$$


#probit: $$Pr(Y=1 \mid X) = \Phi(X'\beta)$$, donde $$ X'\beta= \beta_{0} \beta_{1} Black_i$$

#(b) Reporta los resultados de tus 4 estimaciones con una tabla con el formato usual
#que empleamos el semestre pasado.

stargazer(a1,b1,f1, title="Modelos discriminación racial",
          type="text",
          column.labels = c("MPL univariado",
                            "MPL multivariado"),
          header=FALSE,
          column.sep.width = "2pt", 
          font.size = "small",se=list(robust_se,NULL,NULL))

## Poner la tabla de neyman y explicar que es lo mismo que ols


#(c) Asegurate que los resultados reportados en cada columna sean comparables. Es
#decir, deberan estar reportados en las mismas unidades para poder hacer una
#comparacion a lo largo de las columnas.


#(d) Elige una de las columnas para llevar a cabo una interpretacion del coeciente
#relevante que estas estimando. Da evidencia como parte de esta interpretacion
#de la importancia del efecto. Es decir, >consideras que es un efecto peque~no o
#grande?

#Si observamos la columna 1 y 3, que son los modelos univariados, en ambas el coeficiente de *black* es 
#significativo al .01 y con signo negativo. En el caso de MCO (1), podríamos interpretarlo como "Ceteris
#paribus, en promedio, los afro-americanos tienen 3.2 puntos porcentuales menos probabilidad de recibir un 
#"call-back" que la gente blanca. 
#Sin embargo, si observamos la columna 2, o el MPL multivariado, observamos que al agregar controles *black*
#pierde significancia.

##Pregunta 4

#generamos las variables para evaluar
#checar si sí es así

nam$yc01<- nam$prob_pred+.01
nam$yt01<- nam$prob_pred-.01

attach(nam)


#2

#checar si lo de paired true está bien
perm.test(DV ~ IV, paired=TRUE, alternative="two.sided",all.perms = TRUE, 
          num.sim = 20000)$p.value
#1
DV <- c(nam$yt01, nam$yc01)
IV <- factor(rep(c("T", "C"), c(length(nam$yt01), length(nam$yc01))))
pvalue(oneway_test(DV ~ IV, alternative="two.sided", 
                   distribution=approximate(B=9999)))


#3

B <- 99999
dee <- nam$yt01-nam$yc01
m0 <- mean(dee)

# perform a one-sample randomization test on d
# for the null hypothesis H0: mu_d = 0   vs H1 mu_d != 0  (i.e. two tailed)
# here the test statistic is the mean
rndmdist <- replicate(B,mean((rbinom(length(dee),1,.5)*2-1)*dee))

# two tailed p-value:
sum( abs(rndmdist) >= abs(m0))/length(rndmdist)


#lecuanda

library(EnvStats)
x<-call_back[black==1]
y<-call_back[black==0]
twoSamplePermutationTestLocation(x, y,
                                 alternative="two.sided",seed = 123)

#valor p igual a cero

#Ho implica que todos los individuos el tratamiento no tiene efecto,
#en este caso sería exactamente igual a la probabilidad de ser negro más 1 pp
#(o que la probabilidad de call back si eres negro es 1 pp menor)
# esto implicaría que el efecto de tratamiento es constante: el mismo para 
#todos los individuos. La probabilidad de tener call back siendo blanco es la
#misma de ser negro + 1pp. Rechazamos *Ho* lo que implica que se rechaza que la probabilidad de CB de los blancos sea 1pp
#mayor que la de todos los individuos afro-americanos. Arturo: estoy evaluando si el efecto constante del tratamiento es 
#igual a 1 pp 



#Pregunta 5

# Estraificación por
# sexo
# cd
#industria
#Empleando todas las combinaciones posibles de las variables (i)-(iii),
#utiliza el metodo de Neyman para calcular el efecto de discrminacion en cada estrato
#(elige el formato que quieras para reportar este resultado, tabla o graca). Utilizando
#los efectos por estrato, calcula el efecto promedio de tratamiento. Compara
#este estimador promedio y la varianza con el resultado que obtuviste en la pregunta
#(3).

nam$industria=ifelse(manuf==1,1,
               ifelse(transcom==1,2,
               ifelse(bankreal==1,3,
               ifelse(trade==1,4,
               ifelse(busservice==1,5,
               ifelse(othservice==1,6,
               ifelse(missind==1,7,0)))))))


nam %<>%
  mutate(s = case_when(female == 0 & chicago == 0 & industria ==1 ~ 1,
                       female == 0 & chicago == 0 & industria ==2 ~ 2,
                       female == 0 & chicago == 0 & industria ==3 ~ 3,
                       female == 0 & chicago == 0 & industria ==4 ~ 4,
                       female == 0 & chicago == 0 & industria ==5 ~ 5,
                       female == 0 & chicago == 0 & industria ==6 ~ 6,
                       female == 0 & chicago == 0 & industria ==7 ~ 7,
                       female == 0 & chicago == 1 & industria ==1 ~ 8,
                       female == 0 & chicago == 1 & industria ==2 ~ 9,
                       female == 0 & chicago == 1 & industria ==3 ~ 10,
                       female == 0 & chicago == 1 & industria ==4 ~ 11,
                       female == 0 & chicago == 1 & industria ==5 ~ 12,
                       female == 0 & chicago == 1 & industria ==6 ~ 13,
                       female == 0 & chicago == 1 & industria ==7 ~ 14,
                       female == 1 & chicago == 0 & industria ==1 ~ 15,
                       female == 1 & chicago == 0 & industria ==2 ~ 16,
                       female == 1 & chicago == 0 & industria ==3 ~ 17,
                       female == 1 & chicago == 0 & industria ==4 ~ 18,
                       female == 1 & chicago == 0 & industria ==5 ~ 19,
                       female == 1 & chicago == 0 & industria ==6 ~ 20,
                       female == 1 & chicago == 0 & industria ==7 ~ 21,
                       female == 1 & chicago == 1 & industria ==1 ~ 22,
                       female == 1 & chicago == 1 & industria ==2 ~ 23,
                       female == 1 & chicago == 1 & industria ==3 ~ 24,
                       female == 1 & chicago == 1 & industria ==4 ~ 25,
                       female == 1 & chicago == 1 & industria ==5 ~ 26,
                       female == 1 & chicago == 1 & industria ==6 ~ 27,
                       female == 1 & chicago == 1 & industria ==7 ~ 28,
                       TRUE ~ 0))

ey11 <- nam %>%
  filter(s == 1 & black == 1) %$%
  mean(call_back)
ey10 <- nam %>%
  filter(s == 1 & black == 0) %$%
  mean(call_back)
ey21 <- nam %>%
  filter(s == 2 & black == 1) %$%
  mean(call_back)
ey20 <- nam %>%
  filter(s == 2 & black == 0) %$%
  mean(call_back)
ey31 <- nam %>%
  filter(s == 3 & black == 1) %$%
  mean(call_back)
ey30 <- nam %>%
  filter(s == 3 & black == 0) %$%
  mean(call_back)
ey41 <- nam %>%
  filter(s == 4 & black == 1) %$%
  mean(call_back)
ey40 <- nam %>%
  filter(s == 4 & black == 0) %$%
  mean(call_back)
ey51 <- nam %>%
  filter(s == 5 & black == 1) %$%
  mean(call_back)
ey50 <- nam %>%
  filter(s == 5 & black == 0) %$%
  mean(call_back)
ey61 <- nam %>%
  filter(s == 6 & black == 1) %$%
  mean(call_back)
ey60 <- nam %>%
  filter(s == 6 & black == 0) %$%
  mean(call_back)
ey71 <- nam %>%
  filter(s == 7 & black == 1) %$%
  mean(call_back)
ey70 <- nam %>%
  filter(s == 7 & black == 0) %$%
  mean(call_back)
ey81 <- nam %>%
  filter(s == 8 & black == 1) %$%
  mean(call_back)
ey80 <- nam %>%
  filter(s == 8 & black == 0) %$%
  mean(call_back)
ey91 <- nam %>%
  filter(s == 9 & black == 1) %$%
  mean(call_back)
ey90 <- nam %>%
  filter(s == 9 & black == 0) %$%
  mean(call_back)
ey101 <- nam %>%
  filter(s == 10 & black == 1) %$%
  mean(call_back)
ey100 <- nam %>%
  filter(s == 10 & black == 0) %$%
  mean(call_back)
ey111 <- nam %>%
  filter(s == 11 & black == 1) %$%
  mean(call_back)
ey110 <- nam %>%
  filter(s == 11 & black == 0) %$%
  mean(call_back)
ey121 <- nam %>%
  filter(s == 12 & black == 1) %$%
  mean(call_back)
ey120 <- nam %>%
  filter(s == 12 & black == 0) %$%
  mean(call_back)
ey131 <- nam %>%
  filter(s == 13 & black == 1) %$%
  mean(call_back)
ey130 <- nam %>%
  filter(s == 13 & black == 0) %$%
  mean(call_back)
ey141 <- nam %>%
  filter(s == 14 & black == 1) %$%
  mean(call_back)
ey140 <- nam %>%
  filter(s == 14 & black == 0) %$%
  mean(call_back)
ey151 <- nam %>%
  filter(s == 15 & black == 1) %$%
  mean(call_back)
ey150 <- nam %>%
  filter(s == 15 & black == 0) %$%
  mean(call_back)
ey161 <- nam %>%
  filter(s == 16 & black == 1) %$%
  mean(call_back)
ey160 <- nam %>%
  filter(s == 16 & black == 0) %$%
  mean(call_back)
ey171 <- nam %>%
  filter(s == 17 & black == 1) %$%
  mean(call_back)
ey170 <- nam %>%
  filter(s == 17 & black == 0) %$%
  mean(call_back)
ey181 <- nam %>%
  filter(s == 18 & black == 1) %$%
  mean(call_back)
ey180 <- nam %>%
  filter(s == 18 & black == 0) %$%
  mean(call_back)
ey191 <- nam %>%
  filter(s == 19 & black == 1) %$%
  mean(call_back)
ey190 <- nam %>%
  filter(s == 19 & black == 0) %$%
  mean(call_back)
ey201 <- nam %>%
  filter(s == 20 & black == 1) %$%
  mean(call_back)
ey200 <- nam %>%
  filter(s == 20 & black == 0) %$%
  mean(call_back)
ey211 <- nam %>%
  filter(s == 21 & black == 1) %$%
  mean(call_back)
ey210 <- nam %>%
  filter(s == 21 & black == 0) %$%
  mean(call_back)
ey221 <- nam %>%
  filter(s == 22 & black == 1) %$%
  mean(call_back)
ey220 <- nam %>%
  filter(s == 22 & black == 0) %$%
  mean(call_back)
ey231 <- nam %>%
  filter(s == 23 & black == 1) %$%
  mean(call_back)
ey230 <- nam %>%
  filter(s == 23 & black == 0) %$%
  mean(call_back)
ey241 <- nam %>%
  filter(s == 24 & black == 1) %$%
  mean(call_back)
ey240 <- nam %>%
  filter(s == 24 & black == 0) %$%
  mean(call_back)
ey251 <- nam %>%
  filter(s == 25 & black == 1) %$%
  mean(call_back)
ey250 <- nam %>%
  filter(s == 25 & black == 0) %$%
  mean(call_back)
ey261 <- nam %>%
  filter(s == 26 & black == 1) %$%
  mean(call_back)
ey260 <- nam %>%
  filter(s == 26 & black == 0) %$%
  mean(call_back)
ey271 <- nam %>%
  filter(s == 27 & black == 1) %$%
  mean(call_back)
ey270 <- nam %>%
  filter(s == 27 & black == 0) %$%
  mean(call_back)
ey281 <- nam %>%
  filter(s == 28 & black == 1) %$%
  mean(call_back)
ey280 <- nam %>%
  filter(s == 28 & black == 0) %$%
  mean(call_back)


#diferencias sobrevivir cada estrato
diff1 = ey11 - ey10
diff2 = ey21 - ey20
diff3 = ey31 - ey30
diff4 = ey41 - ey40
diff5 = ey51 - ey50
diff6 = ey61 - ey60
diff7 = ey71 - ey70
diff8 = ey81 - ey80
diff9 = ey91 - ey90
diff10 = ey101 - ey100
diff11= ey111 - ey110
diff12 = ey121 - ey120
diff13 = ey131 - ey130
diff14 = ey141 - ey140
diff15 = ey151 - ey150
diff16 = ey161 - ey160
diff17= ey171 - ey170
diff18 = ey181 - ey180
diff19 = ey191 - ey190
diff20 = ey201 - ey200
diff21 = ey211 - ey210
diff22 = ey221 - ey220
diff23 = ey231 - ey230
diff24 = ey241 - ey240
diff25 = ey251 - ey250
diff26 = ey261 - ey260
diff27 = ey271 - ey270
diff28 = ey281 - ey280


#weights
obs=nrow(nam)

wt1 <- nam %>%
  filter(s == 1 ) %$%
  nrow(.)/obs
wt2 <- nam %>%
  filter(s == 2 ) %$%
  nrow(.)/obs
wt3 <- nam %>%
  filter(s == 3 ) %$%
  nrow(.)/obs
wt4 <- nam %>%
  filter(s == 4 ) %$%
  nrow(.)/obs
wt5 <- nam %>%
  filter(s == 5 ) %$%
  nrow(.)/obs
wt6 <- nam %>%
  filter(s == 6 ) %$%
  nrow(.)/obs
wt7 <- nam %>%
  filter(s == 7 ) %$%
  nrow(.)/obs
wt8 <- nam %>%
  filter(s == 8 ) %$%
  nrow(.)/obs
wt9 <- nam %>%
  filter(s == 9 ) %$%
  nrow(.)/obs
wt10 <- nam %>%
  filter(s == 10 ) %$%
  nrow(.)/obs
wt11 <- nam %>%
  filter(s == 11 ) %$%
  nrow(.)/obs
wt12 <- nam %>%
  filter(s == 12 ) %$%
  nrow(.)/obs
wt13 <- nam %>%
  filter(s == 13 ) %$%
  nrow(.)/obs
wt14 <- nam %>%
  filter(s == 14 ) %$%
  nrow(.)/obs
wt15 <- nam %>%
  filter(s == 15 ) %$%
  nrow(.)/obs
wt16 <- nam %>%
  filter(s == 16 ) %$%
  nrow(.)/obs
wt17 <- nam %>%
  filter(s == 17 ) %$%
  nrow(.)/obs
wt18 <- nam %>%
  filter(s == 18 ) %$%
  nrow(.)/obs
wt19 <- nam %>%
  filter(s == 19 ) %$%
  nrow(.)/obs
wt20 <- nam %>%
  filter(s == 20 ) %$%
  nrow(.)/obs
wt21 <- nam %>%
  filter(s == 21 ) %$%
  nrow(.)/obs
wt22 <- nam %>%
  filter(s == 22 ) %$%
  nrow(.)/obs
wt23 <- nam %>%
  filter(s == 23 ) %$%
  nrow(.)/obs
wt24 <- nam %>%
  filter(s == 24 ) %$%
  nrow(.)/obs
wt25 <- nam %>%
  filter(s == 25 ) %$%
  nrow(.)/obs
wt26 <- nam %>%
  filter(s == 26 ) %$%
  nrow(.)/obs
wt27 <- nam %>%
  filter(s == 27 ) %$%
  nrow(.)/obs
wt28 <- nam %>%
  filter(s == 28 ) %$%
  nrow(.)/obs

sum(wt1,wt2,wt3,wt4,wt5,wt6,wt7,wt8,wt9,wt10,wt11,wt12,wt13,wt14,
    wt15,wt16,wt17,wt18,wt19,wt20,wt21,wt22,wt23,wt24,wt25,wt26,wt27, wt28)


wate = diff1*wt1 + diff2*wt2 + diff3*wt3 + diff4*wt4+ diff5*wt5 + diff6*wt6 + diff7*wt7 + diff8*wt8+
  diff9*wt9 + diff10*wt10 + diff11*wt11 + diff12*wt12+ diff13*wt13 + diff14*wt14 + diff15*wt15 + 
  diff16*wt16+ diff17*wt17 + diff18*wt18 + diff19*wt19 + diff20*wt20+ diff21*wt21 + diff22*wt22 + diff23*wt23 + 
  diff24*wt24+  diff25*wt25 + diff26*wt26 + diff27*wt27+diff28*wt28

# 
Diferencias<-c(diff1,diff2,diff3,diff4,diff5,diff6,diff7,diff8,diff9,diff10,diff11,diff12,diff13,
               diff14,diff15,diff16,diff17,diff18,diff19,diff20,diff21,diff22,diff23,
               diff24,diff25,diff26,diff27,diff28 )

Estrato<-c("Hombre/Boston/Manufacturera","Hombre/Boston/Transporte o comunicaciones","Hombre/Boston/Finanzas, seguros, inmuebles","Hombre/Boston/Comercio","Hombre/Boston/Servicios personales","Hombre/Boston/Servicios sociales","Hombre/Boston/Otro",
           "Hobre/Chicago/Manufacturera","Hombre/Chicago/Transporte o comunicaciones","Hombre/Chicago/Finanzas, seguros, inmuebles","Hombre/Chicago/Comercio","Hombre/Chicago/Servicios personales","Hombre/Chicago/Servicios sociales","Hombre/Chicago/Otro",
           "Mujer/Boston/Manufacturera","Mujer/Boston/Transporte o comunicaciones","Mujer/Boston/Finanzas, seguros, inmuebles","Mujer/Boston/Comercio","Mujer/Boston/Servicios personales","Mujer/Boston/Servicios sociales","Mujer/Boston/Otro",
           "Mujer/Chicago/Manufacturera","Mujer/Chicago/Transporte o comunicaciones","Mujer/Chicago/Finanzas, seguros, inmuebles","Mujer/Chicago/Comercio","Mujer/Chicago/Servicios personales","Mujer/Boston/Servicios sociales","Mujer/Chicago/Otro")


df<-data.frame(Estrato, Diferencias, stringsAsFactors=TRUE)
kable(df, booktabs=T,digits=4,col.names = c("Estrato","Diferencia Probabilidad de call-back entre Afroamericanos y Blancos")) %>% kable_styling(position = "center")

stargazer(wate, gate, type = "text")

###########

#Pregunta 6


#reported está solo la de la interacción
#standard errors are corrected for clustering of the observations at the employment-ad level


#If both robust=TRUE and !is.null(clustervar1) the function overrides the robust command 
#and computes clustered standard errors.

#any req

probitmfx(call_back ~ black+req+ black*req, nam, atmean = TRUE, robust = TRUE, 
          clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

#0.022860 , 0.019067

#experience requirement

probitmfx(call_back ~ black+expreq+ black*expreq, nam, atmean = TRUE, robust = TRUE, 
          clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

#0.010691 , 0.016685

#computer req

probitmfx(call_back ~ black+compreq+ black*compreq, nam, atmean = FALSE, robust = TRUE, 
          clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

#.00092, 0.01600186

#comm skills
probitmfx(call_back ~ black+comreq+ black*comreq, nam, atmean = FALSE, robust = TRUE, 
          clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

#-0.00038614 , 0.02350449

#org skills
probitmfx(call_back ~ black+orgreq+ black*orgreq, nam, atmean = FALSE, robust = TRUE, 
          clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

#0.0278613,  0.0427784

#education
probitmfx(call_back ~ black+educreq+ black*educreq, nam, atmean = FALSE, robust = TRUE, 
          clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

#-0.0308619,  0.0210996

#total number of reqs

#creamos totreq
nam$totreq=expreq+comreq+educreq+compreq+orgreq
attach(nam)

probitmfx(call_back ~ black+totreq+ black*totreq, nam, atmean = FALSE, robust = TRUE, 
          clustervar1 = NULL, clustervar2 = NULL, start = NULL, control = list())

#0.0015161 , 0.0081261

############

#With the chunk option `results = 'asis'`, you can
#write out text as raw Markdown content, which can
#also be mixed with plots.
#
#```{r, mtcars-plots, results='asis'}


attach(nam)
df.sum <- nam %>% dplyr::select(req, expreq, comreq, educreq, compreq, 
         orgreq,totreq) %>% 
  summarise_each(funs(mean = mean, 
                      sd = sd))

df.tidy <- df.sum %>% gather(stat, val) %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  dplyr::select(mean, sd) # reorder columns


#0.022860 , 0.019067
#0.010691 , 0.016685
#.00092, 0.01600186
#-0.00038614 , 0.02350449
#0.0278613,  0.0427784
#-0.0308619,  0.0210996
#0.0015161 , 0.0081261

Requisitos<-c("Alguno","Experiencia","Habilidades computacionales","Habilidades comunicacionales",
              "Habilidades organizacionales","Educacionales","Número total")

marg.effects<-c(0.022860,0.010691,0.00092,-0.00038614,0.0278613,-0.0308619,0.0015161)
marg.eff<-round(marg.effects,digits=3)

marginal.sd<-c(0.019067,0.016685,0.01600186,0.02350449,0.0427784,0.0210996,0.0081261)
marg.sd<-round(marginal.sd,digits=3)

df.tidy$Requisitos=Requisitos
df.tidy$marg.eff=marg.eff
df.tidy$marg.sd=marg.sd

df.tidy<-df.tidy[,c(3,1,2,4,5)]
kable(df.tidy,digits=3,col.names=c("Requisitos","Media","Sd","Efecto marginal","Sd efecto marginal"),
                        rtitle="Tabla 7. Efecto de requisitos en call-backs diferenciados por raza") %>% kable_styling(position = "center")

##########ojooooo

kable(df.tidy,digits=3,format="latex") %>% kable_styling(position = "center")


#######
#PREGUNTA 7

#square term
#square term interaction

years2=yearsexp*yearsexp
d.1<-lm(call_back ~ (yearsexp + years2)*black, data = nam)
d.3<-lm(call_back ~ black + yearsexp + I(yearsexp^2)+I(black*yearsexp)+I(black*yearsexp^2), data = nam)


stargazer(d.3, title="Experiencia y raza en Call-backs",
          type="text",
          column.labels = c("MPL"),
          header=FALSE,
          column.sep.width = "2pt", 
          font.size = "small")
d.4<-glm(formula=call_back ~ black + yearsexp + I(yearsexp^2)+I(black*yearsexp)+I(black*yearsexp^2), data = nam,family=binomial(link="logit"))


plog<-predict(d.4,type="response")
###
d.34<-lm(call_back ~ (yearsexp + yearsexp*yearsexp)*black, data = nam)

d.34factor<-lm(call_back ~ (yearsexp + yearsexp*yearsexp)*as.factor(black), data = nam)

#este toma a vlack como continuo
mydf <- ggpredict(d.34, terms = c("black","yearsexp"))
plot(mydf)

#black como binaria
mydf <- ggpredict(d.34factor, terms = c("black","yearsexp"))
plot(mydf)

#black como binaroia
d.1factor<-lm(call_back ~ (yearsexp + years2)*as.factor(black), data = nam)

mydf <- ggpredict(d.1factor, terms = c("black","yearsexp","years2"))
plot(mydf)

#con d.3
d.3factor<-lm(call_back ~ (yearsexp + years2)*as.factor(black), data = nam)
summary(d.3factor)
mydf <- ggpredict(d.3factor, terms = c("black","yearsexp","years2"))
plot(mydf)




###
#stckflow

#main effect of variable X1: x1+x1^2
#main effect of variable X2: x2
#interaction effect of variables X1 and X2: (X1+X21):X2

#The short answer is that by not including certain terms 
#in the model, you force parts of it to be exactly zero. This imposes an inflexibility to your model that necessarily causes bias, unless those parameters are exactly zero in reality; the situation is analogous to suppressing the intercept (which you can see discussed here).
#https://stats.stackexchange.com/questions/51985/interactions-terms-and-higher-order-polynomials

#So if your hypothesis is that the effect of motives is modified by income, and in particular that this peaks in the middle range, you need to interact motives with both income and income^2
#Interaction with only the linear term would not model a peaking of the interaction in the middle of the income range, and interaction with the quadratic term alone would force the peak effect to be seen at income = 0. So you must include both the linear and quadratic terms in the interaction to model what you are seeking.

library(interplot)##8
#efecto es 3.2
#a) Cuantos CVs cticios necesitara aleatorizar si es que: 
#(i) tu anticipas que los
#efectos (varianza y efecto real) sean iguales a los obtenidos por Bertrand y Mullainathan,
#(ii) quieres un poder estadstico de 85%, 
#(iii) asumes una signicancia
#de 1%, y 
#(iv) vas a dividir 50-50 tratamiento y control?

nam %>% cohens_d(call_back ~ black)

#sale mal
power.t.test(d=0.118,power=.85,sd=.007785,sig.level=.01,type="one.sample",alternative="two.sided")

#Estas 2 me salen equivalentes. 1881 por grupo, 3762 en total
SSizeLogisticBin(p1=0.09650924,p2=0.06447639,B=.5,alpha=.01,power=.85)
power.prop.test(power=.85,sig.level=.01,p1=0.09650924,p2=0.06447639)

#* n is number in each  group

#total_sample_size<-
#  (b) En R o Stata, produce una graca que ilustre el tradeo entre poder estadstico
#y proporcion de tratamiento y control (similar a lo que hicimos con Optimal Design) jando 
#los valores que obtuviste en el inciso anterior (numero de observaciones, efectos reales y signicancia).

#esta se ve mejor

#Poder vs Proporción trat y control
