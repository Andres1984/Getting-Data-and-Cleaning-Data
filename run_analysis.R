## Project Getting Data and Cleaning Data

# Name of the project 

# Andrés Martínez


CC<-setwd("C:/Users/Andres/Documents/test.dir/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset/All")

# Upload The Data

features     <- read.table('features.txt',header=FALSE); #imports features.txt
activityType <- read.table('activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain <- read.table('subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       <- read.table('x_train.txt',header=FALSE); #imports x_train.txt
yTrain       <- read.table('y_train.txt',header=FALSE); #imports y_train.txt



subjectTest <- read.table('subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       <- read.table('x_test.txt',header=FALSE); #imports x_test.txt
yTest       <- read.table('y_test.txt',header=FALSE); #imports y_test.txt



#Give the names to the columns

colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";



colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


#Concatennate the Data

tgData <- cbind(yTrain,subjectTrain,xTrain);


Data <- cbind(yTest,subjectTest,xTest);
FData <- rbind(tgData,Data);

cnames  <- colnames(FData); 

lV <- (grepl("activity..",cnames) | grepl("subject..",cnames) | grepl("-mean..",cnames) & !grepl("-meanFreq..",cnames) & !grepl("mean..-",cnames) | grepl("-std..",cnames) & !grepl("-std()..-",cnames));

FData <- FData[lV==TRUE];


FData <- merge(FData,activityType,by='activityId',all.x=TRUE);

cnames  = colnames(FData); 

for (i in 1:length(cnames)) 
{
  cnames[i] = gsub("\\()","",cnames[i])
  cnames[i] = gsub("-std$","StdDev",cnames[i])
  cnames[i] = gsub("-mean","Mean",cnames[i])
  cnames[i] = gsub("^(t)","time",cnames[i])
  cnames[i] = gsub("^(f)","freq",cnames[i])
  cnames[i] = gsub("([Gg]ravity)","Gravity",cnames[i])
  cnames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",cnames[i])
  cnames[i] = gsub("[Gg]yro","Gyro",cnames[i])
  cnames[i] = gsub("AccMag","AccMagnitude",cnames[i])
  cnames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",cnames[i])
  cnames[i] = gsub("JerkMag","JerkMagnitude",cnames[i])
  cnames[i] = gsub("GyroMag","GyroMagnitude",cnames[i])
};


colnames(FData) = cnames;

FDN  <- FData[,names(FData) != 'activityType'];


tidyData <- aggregate(FDN[,names(FDN) != c('activityId','subjectId')],by=list(activityId=FDN$activityId,subjectId = FDN$subjectId),mean);


tidyData    <- merge(tidyData,activityType,by='activityId',all.x=TRUE);

write.table(tidyData, 'tidyData.txt',row.names=TRUE,sep='\t');
