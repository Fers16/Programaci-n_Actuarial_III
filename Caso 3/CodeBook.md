---
title: "CodeBook"
author: "Guillermina Tlapale - Fernanda Garzón"
date: "9 de junio de 2017"
output: html_document
---

# PROGRAMACIÓN ACTUARIAL III
    
## CASO 3.- RECONOCIMIENTO DE ACTIVIDAD HUMANA CON CELULARES (Samsung Galaxy S)
    
### Libro de código
    
#### Descripción de la base de datos
    
Se realizaron experimentos a un grupo de 30 personas, entre 19 y 48 años, cada persona realizó seis actividades (caminar, caminar escaleras arriba,caminar escaleras abajo, estar sentado, estar quieto y estar acostado) mientras utilizaban un teléfono Samsung Galaxy SII en la cintura.

Utilizando su acelerómetro y giroscopio incorporados, se capturo la aceleración lineal 3-axial y la velocidad angular 3-axial a una velocidad constante de 50Hz. Los experimentos fueron grabados en video para etiquetar los datos manualmente. El conjunto de datos obtenido ha sido dividido aleatoriamente en dos series, donde se seleccionó el 70% de los voluntarios para generar los datos de entrenamiento y el 30% para los datos de prueba.

Las señales de los sensores (acelerómetro y giroscopio) fueron pre-procesadas aplicando filtros de ruido y luego muestreadas en ventanas corredizas de 2.56 seg y 50% de solapamiento. La señal de aceleración del sensor, que tiene componentes gravitacionales y de movimiento corporal, se separó usando un filtro de paso bajo Butterworth en la aceleración del cuerpo y la gravedad. La fuerza de la gravedad se supone que tiene sólo commponentes de baja frecuencia, por lo que se utilizó un filtro con una frecuencia de corte de 0,3 Hz. A partir de cada ventana, se obtuvo un vector de características calculando variables a partir del dominio de tiempo y frecuencia. 


### Base de datos

Los datos originales se conforman por:
    
    Un vector de 561 variables de tiempo y dominio de la frecuencia
    Sus etiquetas de actividades.
    Un identificador del voluntario que participo en el experimento

Las señales fueron utilizadas para estimar variables del vector, para cada patrón:"XYZ" es utilizado para denotar señales de 3 ejes en direcciones X, Y, Z.

En un listado tenemos las características:

    tBodyAcc-XYZ
    tGravityAcc-XYZ
    tBodyAccJerk-XYZ
    tBodyGyro-XYZ
    tBodyGyroJerk-XYZ
    tBodyAccMag
    tGravityAccMag
    tBodyAccJerkMag
    tBodyGyroMag
    tBodyGyroJerkMag
    fBodyAcc-XYZ
    fBodyAccJerk-XYZ
    fBodyGyro-XYZ
    fBodyAccMag
    fBodyAccJerkMag
    fBodyGyroMag
    fBodyGyroJerkMag

Las variables obtenidas a partir de estas señales son:

    mean(): Mean value
    std(): Standard deviation
    mad(): Median absolute deviation
    max(): Largest value in array
    min(): Smallest value in array
    sma(): Signal magnitude area
    energy(): Energy measure. Sum of the squares divided by the number of values.
    iqr(): Interquartile range
    entropy(): Signal entropy
    arCoeff(): Autorregresion coefficients with Burg order equal to 4
    correlation(): correlation coefficient between two signals
    maxInds(): index of the frequency component with largest magnitude
    meanFreq(): Weighted average of the frequency components to obtain a mean  frequency
    skewness(): skewness of the frequency domain signal
    kurtosis(): kurtosis of the frequency domain signal
    bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of  each window.
    angle(): Angle between to vectors.

Además, se obtuvieron datos extra, mediante las señales, en una muestra de ventana de señal y son utilizados en la variable angle():

    gravityMean
    tBodyAccMean
    tBodyAccJerkMean
    tBodyGyroMean
    tBodyGyroJerkMean


### Transformaciones

1.  Unir los datos de test con los de training, para crear un solo conjunto de datos.
2.  Extracción de las medidas de media y desviación estándar de cada medición. 
3.  Renombre de las actividades para describir de mejor manera la base de datos y asignar valores entre 1 y 30 para cada sujeto de prueba.
4.  Colocación de etiquetas apropiadas en la base de datos con nombres de variables que las describan.
5.  Creación de la base de datos final, con el promedio de cada variable para cada actividad y cada sujeto.


### INTEGRANTES:

* Garzón Zurita María Fernanda
* Tlapale Tlapale Guillermina