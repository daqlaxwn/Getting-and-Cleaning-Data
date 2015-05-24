##get colnames
dcolnames <- read.table(paste0("./UCI HAR Dataset/features.txt"), header=FALSE)
lblnames <- read.table(paste0("./UCI HAR Dataset/activity_labels.txt"), header=FALSE,
                       col.names=c("label","activity"))

##get data
for(set_name in c("test","train")) {
    fpath <- paste0("./UCI HAR Dataset/", set_name, "/")
    fname1 <- paste0("X_", set_name, ".txt")
    fname2 <- paste0("y_", set_name, ".txt")
    fname3 <- paste0("subject_", set_name, ".txt")
    if(set_name=="test") {
        X_test <- read.table(paste0(fpath,fname1), header=FALSE, col.names=dcolnames[,2])
        y_test <- read.table(paste0(fpath,fname2), header=FALSE, col.names="label")
        subject_test <- read.table(paste0(fpath,fname3), header=FALSE, col.names="subject")
    }
    else {
        X_train <- read.table(paste0(fpath,fname1), header=FALSE, col.names=dcolnames[,2])
        y_train <- read.table(paste0(fpath,fname2), header=FALSE, col.names="label")
        subject_train <- read.table(paste0(fpath,fname3), header=FALSE, col.names="subject")
    }
}

##merge data & revise column names & name the activities
x_merge <- rbind(X_test,X_train)
colnames(x_merge) <- dcolnames[,2]
x_merge <- x_merge[,agrepl("mean()",colnames(x_merge))|agrepl("std()",colnames(x_merge))]
y_merge <- rbind(y_test,y_train)
y_merge <- merge(y_merge,lblnames,by="label")
data_merge <- cbind(x_merge,y_merge)
data_merge <- cbind(data_merge,rbind(subject_test,subject_train))
tmp <- aggregate(x=data_merge,by=list(data_merge$activity,data_merge$subject),FUN=mean)[,1:68]
colnames(tmp)[1:2]<-c("activity","subject")
tmp <- tmp[order(tmp$subject,tmp$activity),]

write.table(x=tmp,file="./UCI HAR Dataset/data_subject_activity_mean.txt",row.names=FALSE)
