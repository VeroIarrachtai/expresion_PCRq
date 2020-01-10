# 9 enero 2020
# Migue y Vero
# Script para calcular promedio para realizar analisis de datos PCRq


#Cargar archivos
cancer_data<-read.csv("../../data/cancer_data.csv")

#Subsets de tratamientos y control

control<- cancer_data[1:3,-1]
trat_1<- cancer_data[4:6,-1]
trat_2<- cancer_data[7:9,-1]
trat_3<- cancer_data[10:12,-1]

# Diferencia entre condicion control y condicion experimental en Gen HouseK y GoI. Sacar promedio. 
deltaCp_houseK<- c(control[1,1]-trat_1[1,1],
        control[2,1]-trat_1[2,1],
        control[3,1]-trat_1[3,1])
mean_deltaCp_houseK<-mean(deltaCp_houseK)


deltaCp_GoI1<- c(control[1,2]-trat_1[1,2],
                 control[2,2]-trat_1[2,2],
                 control[3,2]-trat_1[3,2])
mean_GoI1<-mean(deltaCp_GoI1)

deltaCp_GoI2<- c(control[1,3]-trat_1[1,3],
               control[2,3]-trat_1[2,3],
               control[3,3]-trat_1[3,3])
mean_GoI2<-mean(deltaCp_GoI2)

deltaCp_GoI3<- c(control[1,4]-trat_1[1,4],
               control[2,4]-trat_1[2,4],
               control[3,4]-trat_1[3,4])
mean_GoI3<-mean(deltaCp_GoI3)


#Eficiencia

m_HouseK<-3.108
exp_HouseK<- -1/m_HouseK
efi_HouseK<- 10^exp_HouseK


m_GoI<-3.459
exp_GoI<- -1/m_GoI
efi_GoI<- 10^exp_GoI

#Expresion relativa

numerador<- efi_GoI^(mean_GoI1)
denominador<- efi_HouseK^(mean_deltaCp_houseK)
exprel1<- numerador/denominador

numerador<- efi_GoI^(mean_GoI2)
denominador<- efi_HouseK^(mean_deltaCp_houseK)
exprel2<- numerador/denominador

numerador<- efi_GoI^(mean_GoI3)
denominador<- efi_HouseK^(mean_deltaCp_houseK)
exprel3<- numerador/denominador


tabla<- data.frame(c("betaCatenina","Dvl", "C-Jun"), c(exprel1,exprel2,exprel3))
colnames(tabla)<- c("gen","exprel")

#Plot
library(ggplot2)
ggplot(tabla, aes(x=gen, y=exprel, fill=gen)) +
  geom_bar(position="dodge", stat="identity")
