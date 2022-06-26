Datos_biometría<-read_excel("Datos biometría.xlsx", sheet=1)
View(Datos_biometría)
head(Datos_biometría)
str(Datos_biometría)
tail(Datos_biometría)

Datos_biometría$Pez<-as.factor(Datos_biometría$Pez)
Datos_biometría$Dosis<-as.factor(Datos_biometría$Dosis)
Datos_biometría$N_Biom<-as.factor(Datos_biometría$N_Biom)
Datos_biometría$N_Estanque<-as.factor(Datos_biometría$N_Estanque)

summary(Datos_biometría)



# Filtrar datos
install(dplyr)
library(dplyr)
Biom_1<-Datos_biometría%>%filter(N_Biom=="1")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_2<-Datos_biometría%>%filter(N_Biom=="2")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_3<-Datos_biometría%>%filter(N_Biom=="3")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_4<-Datos_biometría%>%filter(N_Biom=="4")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_5<-Datos_biometría%>%filter(N_Biom=="5")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_6<-Datos_biometría%>%filter(N_Biom=="6")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_7<-Datos_biometría%>%filter(N_Biom=="7")%>%select(Dosis,N_Estanque,Peso,Talla)
Biom_8<-Datos_biometría%>%filter(N_Biom=="8")%>%select(Dosis,N_Estanque,Peso,Talla)

table(Biom_1$Peso)
table(Biom_8$Dosis)
table(Biom_6$N_Estanque)
table(Biom_7$Talla)

# Métricas de set de datos

# mean(), sd(), range() y quantile().

# Densidad
plot(density(Biom_6$Peso))
plot(density(Biom_7$Talla))
plot(density(Data_8$Promedio_Peso))
plot(density(Data_8$Promedio_Talla))




Data_1<-Biom_1%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(), 
            Promedio_Peso = mean(Peso, na.rm=T), 
            Promedio_Talla =mean(Talla, na.rm=T))

Data_2<-Biom_2%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(),
            Promedio_Peso = mean(Peso, na.rm=T), 
            Promedio_Talla =mean(Talla, na.rm=T))

Data_3<-Biom_3%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(), 
            Promedio_Peso = mean(Peso, na.rm=T), 
            Promedio_Talla =mean(Talla, na.rm=T))
Data_4<-Biom_4%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(), 
           Promedio_Peso = mean(Peso, na.rm=T), 
           Promedio_Talla =mean(Talla, na.rm=T))

Data_5<-Biom_5%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(), 
           Promedio_Peso = mean(Peso, na.rm=T), 
           Promedio_Talla =mean(Talla, na.rm=T))

Data_6<-Biom_6%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(), 
           Promedio_Peso = mean(Peso, na.rm=T), 
           Promedio_Talla =mean(Talla, na.rm=T))

Data_7<-Biom_7%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(), 
           Promedio_Peso = mean(Peso, na.rm=T), 
           Promedio_Talla =mean(Talla, na.rm=T))

Data_8<-Biom_8%>%group_by(Dosis, N_Estanque)%>%summarize(n = n(), 
            Promedio_Peso = mean(Peso, na.rm=T), 
            Promedio_Talla =mean(Talla, na.rm=T))

table(Data_1$Promedio_Peso)
table(Data_8$Promedio_Peso)

# Distribución acumulada empírica
plot(ecdf(Data_3$Promedio_Talla), main="Distribución acumulada empírica", xlab="Talla Promedio (cm)")
plot(ecdf(Data_5$Promedio_Peso), main="Distribución acumulada empírica", xlab="Peso Promedio (g)")



# Gráfica de dispersión ggplot2

Graf_1<-Data_1%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

Graf_2<-Data_2%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

Graf_3<-Data_3%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

Graf_4<-Data_4%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

Graf_5<-Data_5%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

Graf_6<-Data_6%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

Graf_7<-Data_7%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

Graf_8<-Data_8%>%ggplot(aes(x=Promedio_Peso, y=Dosis,color=N_Estanque))+
  geom_point(size=2)+labs(x= "Promedio_Peso(g)", y= "Dosis")+
  theme_bw()

# Histograma
hist(Biom_8$Peso, col="#FA8072", main="Histograma de Peso_8", las=1, xlab="Peso (g)", 
     ylab="Frecuencia")

hist(Biom_7$Talla, col="#4EEE94", main="Histograma de Talla_7", las=1, xlab="Talla (cm)", 
     ylab="Frecuencia")

hist(Data_8$Promedio_Peso, col="#FFD700", main="Histograma de Promedio Peso_8", las=1,
     xlab="Promedio Peso (g)", ylab="Frecuencia")

hist(Data_8$Promedio_Talla, col="#EE3B3B", main="Histograma de Promedio Talla_8", las=1,
     xlab="Promedio Talla (cm)", ylab="Frecuencia")

ggplot(Data_8, aes(Promedio_Peso))+
  geom_histogram(bins=8, color="#858585", fill="#ADFF2F")+
  labs(title="Histograma", x="Promedio de Peso", 
       y="Frecuencia")

ggplot(Data_8, aes(Promedio_Talla))+
  geom_histogram(bins=8, color="#00008B", fill="#1874CD")+
  labs(title="Histograma", x="Promedio de Talla", 
       y="Frecuencia")

# Diagrama de cajas
boxplot(formula = Peso ~ Dosis,
        data =  Biom_1, col=c("#D2691E","#7FFFD4","#FFD700"))


boxplot(formula = Talla ~ Dosis, 
        data=Biom_1)

boxplot(formula = Peso ~ Dosis, 
        data=Biom_8)

boxplot(formula = Talla ~ Dosis, 
        data=Biom_8)

ggplot(Biom_8, aes(x=Dosis, y=Peso, fill=N_Estanque)) +
  geom_boxplot()+
  labs(y="Peso")

ggplot(Biom_8, aes(x=Dosis, y=Talla, fill=N_Estanque)) +
  geom_boxplot()+
  labs(y="Talla")




library(ggpubr)
ggboxplot(Biom_7, x= "Dosis", y
          ="Talla",col= 1, ylab="Talla (cm)",
          xlab="Dosis", notch = F, add.params = list(color = c("cadetblue")), add="jitter")


# Gráfico de dispersión

# Gráfico de interacción
interaction.plot(Dosis,N_Estanque, Promedio_Peso, fun=mean, type = "b",
                legend=TRUE)

library(psych)
# Gráfica de correlación de variables continuas
pairs.panels(Data_8[,4:5], method = "pearson", hist.col = "#FFA500",  density = TRUE, font=2)

# Generando tablas
install.packages(DT)
library(DT)
datatable(Data_8, caption = "Peso y Talla promedio en función de la Dosis.") 

