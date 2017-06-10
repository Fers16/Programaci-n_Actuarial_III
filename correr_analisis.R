getwd()

Xtrain <- read.table(file="./train/X_train.txt", sep="", header=FALSE)
Ytrain <- read.table("./train/y_train.txt", header=FALSE)
Xtest <- read.table(file="./test/X_test.txt", sep="", header=FALSE)
Ytest <- read.table("./test/y_test.txt", header=FALSE)

datosX <- rbind(Xtrain, Xtest)
datosy <- rbind(Ytrain, Ytest)

features <- read.table(file="./features.txt", header=FALSE)
colnames(datosX) <- features$V2

selecVars <- grep(pattern="mean\\(|std\\(", ignore.case=TRUE, x=colnames(datosX))
selecVars <- setdiff(selecVars, grep(pattern="mag", ignore.case=TRUE, x=colnames(datosX)))

datosX <- datosX[, selecVars]
datosX$activity <- factor(unlist(datosy), labels=c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

subjectTrain <- read.table("./train/subject_train.txt")
subjectTest <- read.table("./test/subject_test.txt")
datosX$subject <- unlist(rbind(subjectTrain, subjectTest))

ActivitySubjectMeans <- aggregate(
    datosX[, !(colnames(datosX) %in% c("actividad", "sujeto"))],
    by=list(datosX$subject, datosX$activity), mean)

colnames(ActivitySubjectMeans)[1] = "Sujeto"
colnames(ActivitySubjectMeans)[2] = "Actividad"

ActivitySubjectMeans <- ActivitySubjectMeans[order(ActivitySubjectMeans$Subject), ]

write.table(ActivitySubjectMeans, "Base_de_datos_final.txt", row.names=FALSE)