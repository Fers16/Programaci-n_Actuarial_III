# Directorio actual
getwd()
directorio <- setwd("C:/Users/Ma.Fernanda/Desktop/Programaci-n_Actuarial_III/Specdata")
mediacontaminante <- function(directorio,contaminante,id=1:332){
    pri <- 0 #vector("numeric")
    seg <- 0
    ter <- 0
    if(contaminante == "sulfate"){
        seg=1
    } else if(contaminante == "nitrate"){
        seg=2
    }
    for(i in id){
        if(i<10){
            i = paste("00",i,sep = "")
        } else if(i>=10 && i<100){
            i = paste("0",i,sep = "")
        } else{
            i = paste(i,sep="")
        }
        
        archivo <- read.csv(paste(i,".csv",sep = ""))
        completa <- complete.cases(archivo)
        reales <- archivo[completa,2:3]
        pri <- pri + sum(reales[,B])
        C <- C + nrow(reales)
        
    }
    media <- A/C
    media
}