#install.packages("arules")
#install.packages("arulesViz")
#install.packages("stringr")
#install.packages("sqldf")
library(arules)
library(arulesViz)
library(stringr)
#library(sqldf)


dataset = read.csv('periodico.csv', header = T);
dataset_ejemplo = read.csv('ejemplo.csv', header = T);

dataset$ID<-NULL
names(dataset)[4]<-"items"

#BUSQUEDA DE TRANSACCIONES BOT

# Usando POSIXct para manipular las fechas
dataset$entryTime <- as.numeric(as.POSIXct(dataset$entry))
dataset$exitTime <- as.numeric(as.POSIXct(dataset$exit))
# Calculamos el numero de iteraciones en una transaccion
dataset$numIter <- str_count(as.character(dataset$items),'item')
# Revisamos si es robot. Calculamos los segundos por transaccion y dividimos entre el numero de 
# articulos visitados por dicha transaccion
dataset$duracion <- (dataset$exitTime - dataset$entryTime)
dataset$isRobot <- ((dataset$exitTime - dataset$entryTime)/dataset$numIter <= 20)
# Quitamos los robots del dataset original
datasetRobotsOut <- dataset[dataset$isRobot=="FALSE",]

# DEMANDA #1
contenidos = c("deportes","politica","variedades","internacional","nacionales","sucesos",
               "comunidad","negocios","opinion"); 
article=list();
for (i in 1:nrow(datasetRobotsOut)) {
  items=unlist(regmatches(unlist(datasetRobotsOut[i,4]),gregexpr("[0-9]+",
                                                                 unlist(datasetRobotsOut[i,4]))))
  row = 1
  for (row in items) {
    if (nchar(article[i]) != 4) {
      article[i] = paste(article[i],",",sep = "")
    }
    numero = as.numeric(row)
    if (nchar(article[i]) == 4) {
      if (numero%%9==0) {
        article[i] = paste(contenidos[as.numeric(row)%/%9],"/articulo9",sep = "")
      }else{
        article[i] = paste(contenidos[(as.numeric(row)%/%9)+1], "/articulo",
                           as.character(numero%%9),sep = "")
      }
    }else{
      if (numero%%9==0) {
        article[i] = paste(article[i], contenidos[as.numeric(row)%/%9],"/articulo9",sep = "")
      }else{
        article[i] = paste(article[i], contenidos[(as.numeric(row)%/%9)+1], "/articulo",
                           as.character(numero%%9),sep = "")
      }
    }
  }
}
article<-as.data.frame(as.factor(unlist(article)))
#article2<- data.frame(matrix(unlist(article)))
datasetRobotsOut$articles <- article

datasetRobotsOut$items<-NULL
datasetRobotsOut$entryTime<-NULL
datasetRobotsOut$exitTime<-NULL
datasetRobotsOut$numIter<-NULL
datasetRobotsOut$isRobot<-NULL

# DEMANDA #4

# Ordenamos de mayor a menor
dataTimeOrderHigher <- datasetRobotsOut[order(-datasetRobotsOut$duracion),]
# Obtenemos los 10 primeros registros con la mayor duración
dataTimeHigher10 <- dataTimeOrderHigher[1:10,]
plot(dataTimeHigher10$duracion)

# Ordenamos por duracion de menor a mayor
dataTimeOrderLess <- datasetRobotsOut[order(datasetRobotsOut$duracion),]
# Obtenemos los 10 primeros registros con la menor duración
dataTimeLess10 <- dataTimeOrderLess[1:10,]
plot(dataTimeLess10$duracion)
















