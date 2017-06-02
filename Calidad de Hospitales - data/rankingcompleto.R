#Directorio actual
setwd("C:/Users/Ma.Fernanda/Desktop/Programaci-n_Actuarial_III/Calidad de Hospitales - data")
getwd()
## 1 GRAFICAR LA TASA DE MORTALIDAD DE 30 DÍAS POR ATAQUE AL CORAZÓN
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
head(outcome) 
ncol(outcome) 
names(outcome) 
outcome[, 11]<-as.numeric(outcome[, 11])
hist(outcome[, 11]) 
## 4 RANKING DE HOSPITALES EN TODOS LOS ESTADOS
rankingcompleto<-function(resultado, num){
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    filas <- nrow(outcome)
    estado <- unique(outcome[,7])
    hospital <- vector("character", 54)
    if (resultado == "ataque") {
        columna <- 11
    } else if (resultado == "falla") {
        columna <- 17
    } else if (resultado == "neumonía") {
        columna <- 23
    } else {
        columna <- 2
    }
    if (columna>10) {
        for (i in 1:54) {
            es<- estado[i]
            x <- vector("numeric")
            y <- vector("numeric")
            xl <- 0
            for (k in 1:filas) {
                if(outcome[k,7] == es) {
                    xl <- length(x) + 1
                    length(x) <- xl
                    length(y) <- xl
                    x[xl] <- outcome[k,2]
                    y[xl] <- outcome[k,columna]
                }
            } 
            advertencia <- getOption("warn")
            options(warn = -1)
            c <- as(y,"numeric")
            options(warn = advertencia)
            a<-data.frame(x,c,stringsAsFactors = FALSE)
            b<- a[order(c,x),]
            if (num == "mejor") {
                hospital[i] <- b[1,1]
            } else if (num == "peor") {
                ficasos <- nrow(y[complete.cases(y),])
                hospital[i] <- b[ficasos,1]
            } else {
                hospital[i] <- b[num,1]
            }
        }
        m <- data.frame(hospital,estado,stringsAsFactors = FALSE)
        n <- m[order(estado,hospital),]
        n
    } else {
        "Resultado Invalido"
    }
}
rankingcompleto("ataque","mejor")
