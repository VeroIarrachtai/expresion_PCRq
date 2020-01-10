# 9 enero 2020
# Migue y Vero
# Script para calcular promedio para realizar analisis de datos PCRq

#Cargar archivos
cancer_data<-read.csv("../../data/cancer_data.csv")

#Seleccionar datos
test<- data.frame(cancer_data[10:12,-c(1)])

#Calcular promedio
cancer_mean <- data.frame()

for (i in 1:13){
  
  df_mean <- data.frame(mean(test[,i]))
  col_name <- data.frame(colnames(test)[i])
  df_cancer <- cbind(col_name, df_mean)
  cancer_mean <- rbind(cancer_mean, df_cancer)
}

#Calcular desviación estandar

cancer_sd <- data.frame()

for (i in 1:13){
  
  df_sd <- data.frame( sd(test[,i]))
  col_name <- data.frame(colnames(test)[i])
  df_cancer <- cbind(col_name, df_sd)
  cancer_sd <- rbind(cancer_sd, df_cancer)
}

#Combinar promedio, sd y genes en una tabla

cancer_exprel<-data.frame(cancer_mean,cancer_sd$sd.test...i..)


#Cambiar nombres
colnames(cancer_exprel)<- c("genes_test", "promedio_test", "sd_test")

#Calcular deltaCt
cancer_exprel$deltaCt_test<- cancer_exprel$promedio-cancer_exprel[1,2]

#exportar datos

write.table(cancer_exprel, "../../data/test.txt")


