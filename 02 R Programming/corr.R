corr <- function(directory, threshold=0){
        cpl<-complete(directory)
        above.thresh<-cpl[cpl$nobs > threshold, ]
        file.names<-list.files(directory)
        cor.sulf.nitr<-numeric()
        
        for(i in above.thresh$id){
                temp.file<-read.csv(file.path(directory,file.names[i]))
                cor.sulf.nitr <- c(cor.sulf.nitr, cor(temp.file$sulfate, temp.file$nitrate, use = "pairwise.complete.obs"))
                
        }
        return(cor.sulf.nitr)

}