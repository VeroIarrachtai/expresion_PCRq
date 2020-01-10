# 9 enero 2020
# Migue y Vero
# Script para calcular promedio para realizar analisis de datos PCRq

#Cargar librerias
library(ggplot2)

#Cargar los datos

control<-read.delim("../../data/control.txt", sep = "")
test<- read.delim("../../data/test.txt", sep = "")
test_2<- read.delim("../../data/test_2.txt", sep = "")
test_3<- read.delim("../../data/test_3.txt", sep = "")


#Calcular varianza
control$varianza_control<- control$sd_control^2
test$varianza_test<- test$sd_test^2
test_2$varianza_test_2<- test_2$sd_test^2
test_3$varianza_test_3<- test_3$sd_test^2

colnames(control)<-c("genes", "promedio", "sd", "deltaCt", "varianza")
colnames(test)<-c("genes", "promedio", "sd", "deltaCt", "varianza")
colnames(test_2)<-c("genes", "promedio", "sd", "deltaCt", "varianza")
colnames(test_3)<-c("genes", "promedio", "sd", "deltaCt", "varianza")

#Calcular deltadeltaCt test-control
deltadeltaCt_test<- test$deltaCt-control$deltaCt
deltadeltaCt_test_2<- test_2$deltaCt-control$deltaCt
deltadeltaCt_test_3<- test_3$deltaCt-control$deltaCt

exp_test<- 2^-deltadeltaCt_test
exp_test_2<- 2^-deltadeltaCt_test_2
exp_test_3<- 2^-deltadeltaCt_test_3

#Calcular raíz
raiz_test<-sqrt(control$varianza+test$varianza)
raiz_test_2<-sqrt(control$varianza+test_2$varianza)
raiz_test_3<-sqrt(control$varianza+test_3$varianza)


#Agregar todos lod tratamientos a una dataframe
conttest_all<- rbind(control,test,test_2, test_3)
conttest_all$tratamientos<- c(rep("control",13),rep("test",13), 
                          rep("test_2",13),rep("test_3",13))
conttest_all$exp<-c(rep(1,13),exp_test,exp_test_2,exp_test_3)


#Plotear todos los tratamientos y todos los genes
ggplot(conttest_all, aes(x=genes, y=exp, fill=tratamientos)) +
  geom_bar(position="dodge", stat="identity")+
  geom_linerange(aes(x=genes, ymin=exp-sd, ymax=exp+sd), width=0.5, 
                 colour= "black", alpha= 0.9, size= 0.5, 
                 position = position_dodge(0.9))


#Plot por gen para todos los tratamientos

library(dplyr)
gen_interes <- select(filter(conttest_all, genes == "PYGO"),
                 c(promedio:exp))

ggplot(gen_interes, aes(x=tratamientos, y=exp, fill=tratamientos)) +
  geom_bar(position="dodge", stat="identity")+
  geom_errorbar(aes(x=tratamientos, ymin=exp-sd, ymax=exp+sd), width=0.2, 
                 colour= "black", alpha= 0.9, size= 0.5, 
                 position = position_dodge(0.9))


#Intento Loop

cancer_mean <- data.frame()

for (i in 1:13){
  
  df_mean <- data.frame(mean(test[,i]))
  col_name <- data.frame(colnames(test)[i])
  df_cancer <- cbind(col_name, df_mean)
  cancer_mean <- rbind(cancer_mean, df_cancer)
}

