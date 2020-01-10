#Prueba Shapiro
datos_interes <- select(filter(iris, iris$Species == "versicolor"),
                      c(Sepal.Length:Petal.Width))

shapiro.test(datos_interes$Sepal.Width)
shapiro.test(datos_interes$Sepal.Length)
shapiro.test(iris$Sepal.Width)


plotn <- function(x,main="Histograma de frecuencias \ny distribución normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

plotn(datos_interes$Sepal.Width,main="Distribución normal")#Grafico de x


datos<- read.csv("../../data/cancer_data.csv")
datos_interes <- select(filter(datos, datos$Linea_ccelular == "CaSki"),
                        c(Actina:COX2))



shapiro.test(datos$C.Jun)
shapiro.test(datos_interes$C.Jun)
shapiro.test(datos$MMP7)


plotn(datos$C.Jun,main="Distribución NO normal")#Grafico de x
plotn(datos_interes$C.Jun,main="Distribución normal")#Grafico de x

# https://rpubs.com/RubenOrtiz/192363




