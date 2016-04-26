best <- function(state, outcome) {
        
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        ##outcome_name: "heart attack", "heart failure", "pneumonia"
        states <- data[ , 7]
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if ((state %in% states) == FALSE) {
                stop(print("invalid state"))
        }
        else if ((outcome %in% outcomes) == FALSE) {
                stop(print("invalid outcome"))
        }

        ##subset data for selected outcome and state
        if (outcome==outcomes[1]) state_out <- subset(data[,c(2,7,11)], State == state)
        if (outcome==outcomes[2]) state_out <- subset(data[,c(2,7,17)], State == state)
        if (outcome==outcomes[3]) state_out <- subset(data[,c(2,7,23)], State == state)
        
        ##subset for available outcome data only
        state_out_avail <- subset(state_out, state_out[,3]!="Not Available")
        
        ## Return hospital name in that state with lowest 30-day death rate
        ##best<-tapply(state_out_avail$Hospital.Name, as.numeric(state_out_avail[,3]), min)
        ##best<-state_out_avail[which.min(as.double(state_out_avail[,3])),]
        
        state_out_avail[order(as.double(state_out_avail[,3]), state_out_avail$Hospital.Name),][1,1]
        
}
