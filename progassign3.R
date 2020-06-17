
# outcome data ---
outcome <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])
outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
test <- outcome[,c(2,7,11)]
# Finding the best hospital in a state
#Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
#outcome name. The function reads the outcome-of-care-measures.csv le and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specied outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of \heart attack", \heart failure", or \pneumonia". Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.
#Handling ties. If there is a tie for the best hospital for a given outcome, then the hospital names should
#be sorted in alphabetical order and the rst hospital in that set should be chosen (i.e. if hospitals \b", \c",
#and \f" are tied for best, then hospital \b" should be returned).

best <- function(state, sick) {
    ## Read outcome data
    outcome <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(state%in%unique(outcome$State))) stop("invalid state")
    disease <- c("heart attack", "heart failure", "pneumonia")
    #if (!(outcome%in%disease)) stop("invalid outcome")
    if (!any(grepl(sick, disease))) stop("invalid outcome")
    ## Return hospital name in that state with lowest 30-day death rate
    if (sick == "heart attack"){
        out.sub <- outcome[,c(2,7,11)]
    } else if (sick == "heart failure"){
        out.sub <- outcome[,c(2,7,17)]
    } else {
        out.sub <- outcome[,c(2,7,23)]
    } 
    
    names(out.sub)[3] <- "mortality"
    out.sub <- out.sub[out.sub$State ==state, ]
    out.sub$mortality <- as.numeric(out.sub$mortality)
    #lowest <- min(out.sub[[3]], na.rm = TRUE)
    best.hospital <- out.sub$Hospital.Name[which(out.sub$mortality == min(out.sub$mortality, na.rm = TRUE))]
    #return(best.hospital)
    if (length(best.hospital) ==1) return(best.hospital)
    else if (length(best.hospital > 1)) {
        best.hospital.sub <- best.hospital[order(best.hospital)==1]
        return(best.hospital.sub)
    } else {
        print("No hospital meet the criteria.")
    }
    
}

#The function should check the validity of its arguments. If an invalid state value is passed to best, the
#function should throw an error via the stop function with the exact message \invalid state". If an invalid
#outcome value is passed to best, the function should throw an error via the stop function with the exact
#message \invalid outcome".

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")

best("SC", "heart attack")
best("NY", "pneumonia")
best("MD", "pneumonia")

#hw
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")

# Ranking hospitals by outcome in a state
rankhospital <- function(state, sick, num = "best") {
    ## Read outcome data
    outcome <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    if (!(state%in%unique(outcome$State))) stop("invalid state")
    disease <- c("heart attack", "heart failure", "pneumonia")
    #if (!(outcome%in%disease)) stop("invalid outcome")
    if (!any(grepl(sick, disease))) stop("invalid outcome")
    ## Return hospital name in that state with lowest 30-day death rate
    if (sick == "heart attack"){
        out.sub <- outcome[,c(2,7,11)]
    } else if (sick == "heart failure"){
        out.sub <- outcome[,c(2,7,17)]
    } else {
        out.sub <- outcome[,c(2,7,23)]
    } 
    
    names(out.sub)[3] <- "mortality"
    out.sub <- out.sub[out.sub$State ==state, ]
    out.sub$mortality <- as.numeric(out.sub$mortality)
    out.sub <- na.omit(out.sub)
    out.sub <- out.sub[order(out.sub$mortality, out.sub$Hospital.Name), ]
    out.sub$Rank <-1:nrow(out.sub)
    ## Return hospital name in that state with the given rank 30-day death rate
    if (num == "best") {best.hospital <- out.sub$Hospital.Name[which(out.sub$Rank == 1)]
    } else if (num == "worst") {best.hospital <- out.sub$Hospital.Name[which(out.sub$Rank == nrow(out.sub))]
    } else if (num > nrow(out.sub)) {best.hospital <- NA 
    } else {
        best.hospital <- out.sub$Hospital.Name[which(out.sub$Rank == num)]
    }
    
    return(best.hospital)
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")

# Ranking hospitals in all states
rankall <- function(sick, num = "best") {
    ## Read outcome data
    outcome <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    ## Check that state and outcome are valid
    #if (!(state%in%unique(outcome$State))) stop("invalid state")
    disease <- c("heart attack", "heart failure", "pneumonia")
    #if (!(outcome%in%disease)) stop("invalid outcome")
    if (!any(grepl(sick, disease))) stop("invalid outcome")
    ## Return hospital name in that state with lowest 30-day death rate
    if (sick == "heart attack"){
        out.sub <- outcome[,c(2,7,11)]
    } else if (sick == "heart failure"){
        out.sub <- outcome[,c(2,7,17)]
    } else {
        out.sub <- outcome[,c(2,7,23)]
    } 
    
    names(out.sub)[3] <- "mortality"
    #out.sub <- out.sub[out.sub$State ==state, ]
    out.sub$mortality <- as.numeric(out.sub$mortality)
    out.sub <- na.omit(out.sub)
    ## For each state, find the hospital of the given rank
    out.sub <- out.sub[order(out.sub$State, out.sub$mortality, out.sub$Hospital.Name), ]
    out.sub$Rank <- NULL
    out.sub$Rank.bw <- NULL
    state <- unique(out.sub$State)
    for (s in state){
        out.sub[out.sub$State==s,"Rank"] <- 1:nrow(out.sub[out.sub$State==s,])
        out.sub[out.sub$State==s&out.sub$Rank==1,"Rank.bw"] <- "best"
        out.sub[out.sub$State==s&out.sub[out.sub$State==s,"Rank"] == nrow(out.sub[out.sub$State==s,])]
        
        if (out.sub[out.sub$State==s,"Rank"] == 1) {out.sub[out.sub$State==s,"Rank.bw"] <- "best"
        } else if (out.sub[out.sub$State==s,"Rank"] == nrow(out.sub[out.sub$State==s,])){
            out.sub[out.sub$State==s,"Rank.bw"] <- "worst"
        } else {out.sub[out.sub$State==s,"Rank.bw"] <- "middle"}
    }
    head(out.sub)
    # subset by rank---
    if (num == "best") {best.hospital <- out.sub[which(out.sub$Rank.bw == "best"), c("Hospital.Name", "State")]
    } else if (num == "worst") {best.hospital <- out.sub[which(out.sub$Rank.bw == "worst"), c("Hospital.Name", "State")]
    } else {
        best.hospital <- out.sub[which(out.sub$Rank == num), c("Hospital.Name", "State")]
    }
    
    names(best.hospital) <- c("hospital", "state")
    
    if (length(best.hospital$state) < length(state)) {
        best.hospital.sub <- data.frame(hospital = NA, state = setdiff(state, best.hospital$state))
        best.hospital.final <- rbind(best.hospital, best.hospital.sub)
        best.hospital.final <- best.hospital.final[order(best.hospital.final$state),]
        return(best.hospital.final)
    } else {
        return(best.hospital)
    }
    
    
}


