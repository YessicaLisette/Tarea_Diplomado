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

str(biometría)

# Filtrando datos y obteniendo promedios

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