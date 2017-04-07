getwd()
directorio <- setwd("C:/Users/Ma.Fernanda/Desktop/Programaci-n_Actuarial_III/Specdata")
completos <- function(directorio,id=1:332){
    a <- vector("numeric")
    for (i in id) {
        if(i<10){
            i = paste("00",i,sep="")
        } else if(i>=10 && i<100){
            i = paste("0",i,sep="")
        } else{
            i = paste(i,sep="")
        }
        leer <- read.csv(paste(i,".csv",sep=""),header = TRUE)
        datos <- complete.cases(leer)
        reales <- leer[datos,2:3]
        numeda <- nrow(reales)
        a <- c(a,numeda)
    }
    dafra <- data.frame(ID = id,NOBS = a)
    print(dafra)
}

ler <- read.csv("001.csv")
dat <- complete.cases(ler)
real <- ler[dat, 2:3]
relaci <- cor(real[,1],real[,2])
relaci

completos(directorio,1:10)