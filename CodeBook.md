###Script.
Actually, original script contains comments (starting with #) explaining how script works.  
 
#load data from files  
test<- read.table("C:/Desktop/UCI HAR Dataset/test/X_test.txt",header=F)  
testlabel<- read.table("C:/Desktop/UCI HAR Dataset/test/y_test.txt",header=F)  
train<- read.table("C:/Desktop/UCI HAR Dataset/train/X_train.txt",header=F)  
trainlabel<- read.table("C:/Desktop/UCI HAR Dataset/train/y_train.txt",header=F)  
feature<- read.table("C:/Desktop/UCI HAR Dataset/features.txt",header=F)  
actlabel<- read.table("C:/Desktop/UCI HAR Dataset/activity_labels.txt",header=F)  
subjecttest<- read.table("C:/Desktop/UCI HAR Dataset/test/subject_test.txt",header=F)  
subjecttrain<- read.table("C:/Desktop/UCI HAR Dataset/train/subject_train.txt",header=F)  

#task1#################  
#Merging the training and the test sets  
combine<-rbind(test,train)  
  
#task2#and#task4#################   
#labeling the data set with descriptive variable names.   
colnames(combine)<-feature[,2]  
  
#making a vector called "labels" for extracting only the measurements on the mean and   standard deviation for each measurement.  
  
#extarcting feature including "mean"  
mean<-grep("\\-mean", feature[,2])   
    
#extarcting feature including "meanfreq"  
meanfreq<-grep("\\-meanFreq", feature[,2])   
  
#extarcting feature including "mean" but excluding "meanfreq"  
cleanmean<-setdiff(mean, meanfreq)  
  
#extarcting feature including "std"  
std<-grep("\\-std", feature[,2])   
  
#combining those features to make a vector called "labels" and conducting sorting as well.  
labels<-sort(c(cleanmean,std))  

  
#slicing by using vector created above  
combineavesd<-combine[,labels]  
  
#task3#################  
#Merging the labels for training and the test sets  
combinelabel<-rbind(testlabel,trainlabel)  
  
#using information from activity label file, converting numeric activity labels into the   #discriptive activity names  
for (i in 1:length(actlabel[,1])) {  
 combinelabel[,1]<-sub( actlabel[i,1],  actlabel[i,2], combinelabel[,1])  
}  
  
activity_labeled_combineavesd<-cbind(combinelabel,combineavesd)  
  
#task4  
#In order to label the data set with descriptive variable names,  
#original variable names were trasnformed into more descriptive ones  
  
name<-colnames(activity_labeled_combineavesd)  
name<-gsub( "V1", "Activity_name", name)  
name<-gsub( "Acc", "Acceleration", name)  
name<-gsub( "std", "SD", name)  
name<-gsub( "Gyro", "Angular_velocity", name)  
name<-gsub( "Mag", "Magnitude", name)  
name<-gsub( "std", "SD", name)  
name<-gsub( "X", "X_axis_signal", name)  
name<-gsub( "Y", "Y_axis_signal", name)  
name<-gsub( "Z", "Z_axis_signal", name)  
name[2:41]<-sub( "t", "Time_domain", name[2:41])  
name[42:67]<-sub( "f", "Frequency_domain", name[42:67])  
colnames(activity_labeled_combineavesd)<-name  
  
  
#task5#################  
#Merging the training and the test subject information  
subjects<-rbind(subjecttest,subjecttrain)  
  
#Merging the dataset from task4 and subject information  
  
data5<-cbind(subjects,activity_labeled_combineavesd)    
colnames(data5)<-sub( "V1", "Subject", colnames(data5))  
  
  
m <- data.frame(matrix(nrow=length(unique(data5[,1]))*length(unique(data5[,2])),ncol = 68))  
colnames(m)<-colnames(data5)  
  
l=1  
for (i in unique(data5[,1])) {  
 for (j in unique(data5[,2])) {  
   m[l,1]=i  
   m[l,2]=j  
   l=l+1
}  
}  
  
p=1  
for (i in unique(data5[,1])) {  
 for (j in unique(data5[,2])) {  
  m[p,3:68]=colMeans(data5[(data5[,1]==i & data5[,2]==j),3:68])  
  p=p+1  
}   
}  
    
write.table(m,"week3.txt",row.name=FALSE,sep=" ")  

  