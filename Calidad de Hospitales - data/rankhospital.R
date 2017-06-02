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
## 3 JERARQUÍA DE HOSPITAL POR RESULTADO EN UN ESTADO
rankhospital <- function(estado, resultado, num) {
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    filas <- nrow(outcome)
    if (resultado == "infarto") {
        columna <- 11
    } else if (resultado == "falla") {
        columna <- 17
    } else if (resultado == "neumonía") {
        columna <- 23
    } else {
        columna <- 2
    }
    x <- vector("numeric")
    y <- vector("numeric")
    if (columna>10){
        xl <- 0
        for (i in 1:filas) {
            if (outcome[i,7] == estado) {
                xl <- length(x) + 1
                length(x) <- xl
                length(y) <- xl
                x[xl] <- outcome[i,2]
                y[xl] <- outcome[i,columna]
            }
        }
        if (xl>0) {
            advertencia <- getOption("warn")
            options(warn = -1)
            c <- as(y,"numeric")
            options(warn = advertencia)
            a <- data.frame(x,c,stringsAsFactors = FALSE)
            b <- a[order(c,x),]
            if (num == "mejor") {
                b[1,1]
            } else if (num == "peor") {
                lpeor <- nrow(b[complete.cases(b),])
                b[lpeor,1]
            } else {
                b[num,1]
            }
        } else {
            "Estado Invalido"
        }
    } else {
        "Resultado Invalido"
    }
}
##Ejemplos del caso
rankhospital("TX", "falla", 4)
rankhospital("MD", "infarto", "peor")
rankhospital("MN", "infarto", 5000)
rankhospital("MD","falla",5)