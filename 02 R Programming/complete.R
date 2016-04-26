complete <- function(directory, id=1:332){
        file.names <- list.files(directory)
        observations <-data.frame()
        
        for(i in id){
                temp.file<-read.csv(file.path(directory,file.names[i]))
                nobs<-sum(complete.cases(temp.file))
                observations<-rbind(observations, c(i,nobs))
        }
        colnames(observations)<-c("id","nobs")
        return(observations)

}