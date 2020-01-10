# 9 enero 2020
# Migue y Vero
# Script para calcular promedio para realizar analisis de datos PCRq


#Cargar archivos
cancer_data<-read.csv("../../data/cancer_data.csv")

#Seleccionar datos
control<- data.frame(cancer_data[1:3,-c(1)])

#Calcular promedio
cancer_mean <- data.frame()

for (i in 1:13){
  
  df_mean <- data.frame(mean(control[,i]))
  col_name <- data.frame(colnames(control)[i])
  df_cancer <- cbind(col_name, df_mean)
  cancer_mean <- rbind(cancer_mean, df_cancer)
}

#Calcular desviación estandar

cancer_sd <- data.frame()

for (i in 1:13){
  
  df_sd <- data.frame( sd(control[,i]))
  col_name <- data.frame(colnames(control)[i])
  df_cancer <- cbind(col_name, df_sd)
  cancer_sd <- rbind(cancer_sd, df_cancer)
}

#Combinar promedio, sd y genes en una tabla

cancer_exprel<-data.frame(cancer_mean,cancer_sd$sd.control...i..)


#Cambiar nombres
colnames(cancer_exprel)<- c("genes_control", "promedio_control", "sd_control")

#Calcular deltaCt
cancer_exprel$deltaCt_control<- cancer_exprel$promedio-cancer_exprel[1,2]

#exportar datos

write.table(cancer_exprel, "../../data/control.txt")


