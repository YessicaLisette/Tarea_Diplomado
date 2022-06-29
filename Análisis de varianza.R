# Leer archivo 
biometría<-read_excel("Datos_biometría.xlsx", sheet=1)
View(Datos_biometría)
head(Datos_biometría)
str(Datos_biometría)
tail(Datos_biometría)

# Convertir en factor variables "Pez","Dosis","N_Biom", "N_Estanque"
biometría$Pez<-as.factor(biometría$Pez)
biometría$Dosis<-as.factor(biometría$Dosis)
biometría$N_Biom<-as.factor(biometría$N_Biom)
biometría$N_Estanque<-as.factor(biometría$N_Estanque)

# Tablas de frecuencia
table(Biom_1$Dosis, Biom_1$N_Estanque)

str(biometría)

# Filtrar datos
install(dplyr)
library(dplyr)
Biom_1<-biometría%>%filter(N_Biom=="1")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_2<-biometría%>%filter(N_Biom=="2")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_3<-biometría%>%filter(N_Biom=="3")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_4<-biometría%>%filter(N_Biom=="4")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_5<-biometría%>%filter(N_Biom=="5")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_6<-biometría%>%filter(N_Biom=="6")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_7<-biometría%>%filter(N_Biom=="7")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_8<-biometría%>%filter(N_Biom=="8")%>%select(Dosis,N_Estanque,Peso,Talla)

# Análisis preliminar
tapply(Biom_1$Talla,Biom_1$Dosis,mean)
tapply(Biom_1$Peso,Biom_1$Dosis, mean)
tapply(Biom_8$Talla,Biom_8$Dosis,mean)
tapply(Biom_8$Peso,Biom_8$Dosis,mean)

boxplot(Biom_8$Peso~Biom_8$Dosis, names=c("0.5", "0.3", "0"), cex.main=1,
        ylab="Peso", col=c("deepskyblue1","deepskyblue3","deepskyblue4"), main="Peso segun dosis de inclusión",
        las=1, xlab="Dosis", ylimit=c(0,50))

# Evaluación de supuestos
# a. Test de normalidad
shapiro.test(Biom_1$Peso)
shapiro.test(Biom_8$Peso)
shapiro.test(biometría$Peso)

shapiro.test(Biom_1$Talla)
shapiro.test(Biom_8$Talla)

# Gráfico de normalidad
ggqqplot(biometría$Peso, main="Peso", col = "olivedrab2")
ggqqplot(biometría$Talla, main="Talla", col = "cornflowerblue")

# b. Homocedasticidad de varianzas
# HO: Var(A) = Var(T) = var(E)
# H1: Existe una Var(i) =/= Var(j)

bartlett.test(biometría$Peso~biometría$Dosis)
fligner.test(biometría$Peso~biometría$Dosis)

bartlett.test(Biom_8$Peso~Biom_8$Dosis)
fligner.test(Biom_8$Peso~Biom_8$Dosis)

# Modelo lineal
lm.aov <- lm(Biom_8$Peso ~ Biom_8$Dosis, data = Biom_8)
aov(lm.aov)

# Evaluación de supuestos
plot(lm.aov$residuals, pch=20, col = "blue")

dwtest(Peso ~ Dosis, data = Biom_8,
       alternative = c("two.sided"), 
       iterations = 15)

# Librerias
library(ggplot2)
library(ggpubr)
library(car)
library(lmtest)
library(dplyr)
library(knitr)

# Homogeneidad de varianzas
plot(lm.aov, 1, pch=20, col = "blue")
leveneTest(Peso ~ Dosis, data = Biom_8,
           center = "median")

# Normalidad
plot(lm.aov, 2, pch=20, col = "blue")

# Histograma de residuales
aov_residuals <- residuals(object = lm.aov)
hist(x= aov_residuals, main = "Histograma de residuales")

# Shapiro Test
shapiro.test(x= aov_residuals)

hist(x= aov_residuals, main = "Histograma de residuales")

# Conclusión: Se realizaron los gráficos y las pruebas para cada uno de
# los supuestos. Los resultados de las pruebas mostraron que no se cumplían 
# los tres supuestos (independencia, homogeneidad de varianzas y normalidad); 
# ya que éstas pruebas presentaron p-valores inferiores al nivel de significación del 5%. 
# Debido al cumplimiento de los tres supuestos, se concluye que no es posible 
# realizar el análisis de varianza.

# Prueba no paramétrica: Kruskall-Wallis
kruskal.test(Biom_8$Peso~Biom_8$Dosis)
# Conclusión: Hay evidencia estadística hay evidencia estadística para decir que al menos un grupo de
# sometido a diferentes dosis difiere en peso de los otros grupos.


plot.design(Biom_8$Peso~as.factor(Biom_8$Dosis), col=2, ylim(5,48),
            las=1)

# Prueba de comparaciones múltiples: DunnTest
install.packages("DescTools")
library(DescTools)
DunnTest(Biom_8$Peso~as.factor(Biom_8$Dosis)) # Todos contra todos

# Conclus: El peso con la inclusión de la dosis de 0.3 g/Kg es > que resto
# en trucha arcoíris. El peso con dosis de 0.5 y control es igual

# Comparaciones Multiples No-paramétricas
install.packages("nparcomp") # Intalar paquete DescTools
library("nparcomp")
gao(Peso~Dosis, data=Biom_8, alpha = 0.05, control = "0", silent = T) # comparado control


