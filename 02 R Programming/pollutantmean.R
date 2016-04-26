pollutantmean <- function(directory, pollutant,id=1:332){
        file.names <- list.files(directory)
        poll.data <-numeric()
        for(i in id){
                
                temp.file<-read.csv(file.path(directory,file.names[i]))
                temp.data<-temp.file[[pollutant]][!is.na(temp.file[[pollutant]])]
                poll.data<-c(poll.data,temp.data)
                
        }
        return(mean(poll.data))

}