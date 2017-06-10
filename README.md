---
title: "README"
author: "Guillermina Tlapale - Fernanda Garzón"
date: "9 de junio de 2017"
output: html_document
---
    
# PROGRAMACIÓN ACTUARIAL III
    
### CASO 3.- RECONOCIMIENTO DE ACTIVIDAD HUMANA CON CELULARES (Samsung Galaxy S)
    
#### EXPLICACIÓN DEL CASO    
    
El propósito de este proyecto es demostrar nuestra habilidad para recolectar, trabajar y limpiar base de datos.
El objetivo es preparar un conjunto ordenado de información que pueda ser trabajado en análisis posteriores.
Incluye:
    
* La base de datos ordenada
* Dirección de GitHub para el repositorio
* Un libro de códigos para describir las variables, la base de datos y las transformaciones que realices llamado "CodeBook.md".
* Un archivo "README.md" en el repositorio con tus códigos. Este archivo explica el funcionamiento de tus scripts y como se conectan entre ellos. Deberás crear un código en R llamado "correr_analisis.R" que haga lo siguiente:
    
1.  Une los datos de test con los de training, para crear un solo conjunto de datos.
2.  Extrae únicamente las medidas de media y desviación estándar de cada medición.
3.  Usa nombres de actividad para describir los nombres de las actividades en la base de datos.
4.  Coloca etiquetas apropiadas en la base de datos con nombres de variables que las describan.
5.  Con los datos del paso 4, creamos una segunda base de datos independiente con el promedio de cada variable para cada actividad y cada sujeto.

#### FUNCIONAMIENTO DEL CÓDIGO

Con el código dentro de correr_análisis.R realizamos lo siguiente:

Primero que nada, establecemos el directorio de trabajo, seleccionando nuestra carpeta de github:

    > setwd("C:/Users/Ma.Fernanda/Desktop/Programaci-n_Actuarial_III/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset")
    
Asignamos variables a cada parte de nuestro código:

    Xtrain <- read.table(file="./train/X_train.txt", sep="", header=FALSE)
    Ytrain <- read.table("./train/y_train.txt", header=FALSE)
    Xtest <- read.table(file="./test/X_test.txt", sep="", header=FALSE)
    Ytest <- read.table("./test/y_test.txt", header=FALSE)

Unimos los datos de los archivos test con los de training, así, creamos un solo conjunto de datos.

    datosX <- rbind(Xtrain, Xtest)
    datosy <- rbind(Ytrain, Ytest)
    
Al igual que las actividades (features) y los datos que ocuparemos:

    features <- read.table(file="./features.txt", header=FALSE)
    colnames(datosX) <- features$V2
    
Más tarde, llamamos a la función "grep" para usarla de esta manera:

    selecVars <- grep(pattern="mean\\(|std\\(", ignore.case=TRUE, x=colnames(datosX))
    selecVars <- setdiff(selecVars, grep(pattern="mag", ignore.case=TRUE, x=colnames(datosX)))

Re-Declaramos nuestra variable y definimos las actividades:

    datosX <- datosX[, selecVars]
    datosX$activity <- factor(unlist(datosy), labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

Ha llegado el momento de usar los otros archivos, y declararlos:

    subjectTrain <- read.table("./train/subject_train.txt")
    subjectTest <- read.table("./test/subject_test.txt")
    datosX$subject <- unlist(rbind(subjectTrain, subjectTest))

Comenzamos a hacer nuestra base de datos final:

    ActivitySubjectMeans <- aggregate(datosX[, !(colnames(datosX) %in% c("actividad", "sujeto"))],
    by=list(datosX$subject, datosX$activity), mean)

Y damos orden, mediante etiquetas:

    colnames(ActivitySubjectMeans)[1] = "Sujeto"
    colnames(ActivitySubjectMeans)[2] = "Actividad"
    ActivitySubjectMeans <- ActivitySubjectMeans[order(ActivitySubjectMeans$Subject), ]

Por último, colocamos la base de datos ordenada en un archivo de texto, con los 30 sujetos y los promedios de cada una de las 66 mediciones, en cada una de las seis actividades, creamos nuestra tabla:

    write.table(ActivitySubjectMeans, "Base_de_datos_final.txt", row.names=FALSE)