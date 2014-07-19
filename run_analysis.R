run_analysis <- function() {
        ##First load all the required files for the project
        #Load the labels for activity and variable names
        acti<-read.table("Dataset/activity_labels.txt")
        acti<-acti[,2]
        features<-read.table("Dataset/features.txt")
        features<-features[,2]
        
        #load actual datas and assemble them
        trainset<-load_assemble(features,"train")
        testset<-load_assemble(features,"test")
        tables<-list(trainset,testset)
        #Merge train and test using the sujectid 
        onedata<-do.call("rbind",tables)
        
        #Extract all column with mean() and std()
        coln<-colnames(onedata)
        indm<-grepl("mean\\(\\)",coln)
        inds<-grepl("std\\(\\)",coln)
        ind<-(indm | inds)
        #adding back id and activity
        ind[1]<-TRUE
        ind[length(coln)]<-TRUE
        one<-onedata[,ind]
        
        #Convert activity code to name of activity (acti was loaded from labels)
        #Have to coerce one$activity into integer 
        #Have to coerce acti into character
        for (i in seq_along(one$Activity)) {
                one$Activity[i]<-as.character(acti[as.integer(one$Activity[i])])
        }
        
        #clean up name of columns lower case remove - and ()
        col<-colnames(one)
        col<-tolower(col)
        col<-gsub("-","",col)
        col<-gsub("\\(\\)","",col)
        colnames(one)<-col
        
        ## Provide tidy table with average of each variable with regards to activity and subject 
        
        #first split with regards to id and activity
        onesplit <-split(one,list(one$id,one$activity))
        
        #lapply function specialmean that gives back single row table with mean 
        onemean <-lapply(onesplit,specialmean)
        #We assemble all the rows back in one table
        onemean <-do.call("rbind",onemean)
        
        #moving the column activity next to id
        len<-length(colnames(onemean))
        neworder <- list(onemean[,1],onemean[,len],onemean[,2:(len-1)])
        onemean <- do.call("cbind",neworder)
        
        #rename the columns indicating they have been averaged, fix id and activity
        coln<-colnames(onemean)
        coln<-lapply(coln,function(x) paste0(x,"avg"))
        coln[1]<-"id"
        coln[2]<-"activity"
        colnames(onemean) <-coln
        
        #export the data in tidydata.txt file
        write.table(onemean,"tidydata.txt")
        
}


load_assemble <- function(names,directory) {
        ## This function loads the different data in the directory and assemble
        ## It will also name all the column appropriately
        
        #creating the filename
        filename<-list("subject","X","y")
        prefix<-paste0("Dataset/",directory,"/")
        suffix<-paste0("_",directory,".txt")
        #sapply to create list of filenames
        filename<-sapply(filename,function(x,y) paste0(prefix,x,suffix))
        
        #lapply to read list of files
        result<-lapply(filename,read.table)                
        #bind them together subject id, variables,activity 
        result<-do.call("cbind",result)
        
        #renaming the columns appropriately
        cnames<-c("id",as.character(names),"Activity")
        colnames(result)<-cnames
        result          
}

specialmean <- function(table) {
        ##This function will average on all variables and ignore id and activity
        output<-table[1,]
        len <-length(output)
        for (i in 2:(len-1)) {output[i]<-mean(table[,i])}
        output
}


        
        
        
