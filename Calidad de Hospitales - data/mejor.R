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
## 2 ENCONTRAR EL MEJOR HOSPITAL EN UN ESTADO
mejor <- function(estado,resultado){
    outcome <- read.csv("outcome-of-care-measures.csv")
    N <- FALSE
    filas <-nrow(outcome)
    if(resultado== "ataque"){
        columna <- 11
    } else if (resultado== "falla"){
        columna <- 17
    } else if (resultado== "neumonia"){
        columna <- 23
    }
    if (!((resultado == "ataque")|(resultado == "falla")| (resultado == "neumonia"))){
        stop("Resultado Inválido")
    }
    x <- matrix(outcome[,columna],filas,1)
    outcome[,columna] <- suppressWarnings(as.numeric(x))
    outcome[,2] <- as.character(outcome[,2])
    y <- outcome[grep(estado,outcome$State),]
    ordenar <- y[order(y[,columna], y[,2], na.last=NA),]
    z <- matrix(outcome[,7], filas,1)
    for (i in 1:length(z))
        if(estado == z[i]){
            N <- TRUE
        }
    if(!N){
        stop("Estado Inválido")
    }
    ordenar[1,2]
}

##Pruebas##

mejor("TX", "ataque")
mejor("TX", "falla") 
mejor("MD", "ataque")
mejor("MD", "neumonia")
mejor("BB", "ataque")
mejor("NY", "ataque")