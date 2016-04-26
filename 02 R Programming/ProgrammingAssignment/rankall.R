rankall <- function(outcome, num = "best") {
        
        ## Check that state and outcome are valid
        ##outcome_name: "heart attack", "heart failure", "pneumonia"
        ##states <- data[ , 7]
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        
        if ((outcome %in% outcomes) == FALSE) {
                stop(print("invalid outcome"))
        }
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##subset data for selected outcome
        if (outcome==outcomes[1]) out <- subset(data[,c(2,7,11)])
        if (outcome==outcomes[2]) out <- subset(data[,c(2,7,17)])
        if (outcome==outcomes[3]) out <- subset(data[,c(2,7,23)])
        
        ##subset for available outcome data only
        out_avail <- subset(out, out[,3]!="Not Available")
        ##split dataset into a list with a dataframe for each state
        by_state<-split(out_avail, out_avail$State)
        
       ##select the hospital with rank num for each state
         if (num == "best") {
                ranks<-sapply(by_state, function(x){x[order(as.double(x[,3]),x[,1]),][1,]}) ##take first
        }
                
        else if (num == "worst") {
                ranks<-sapply(by_state, function(x){x[order(as.double(x[,3]),x[,1], decreasing= TRUE),][1,]}) ##take last
        }
                
        else {
                ranks<-sapply(by_state, function(x){x[order(as.double(x[,3]),x[,1]),][num,]}) 
        }
        
        ##transpose matrix and select only hospital and state, set col names
        ranks<-t(ranks[1:2,])
        colnames(ranks)<-c("hospital", "state")
     
        
        ## Return ranking as data frame with correct names
        ranks<-as.data.frame(ranks)
        ranks$state <- row.names(ranks)
        ranks
}
