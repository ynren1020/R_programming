# function to calculate pollutant mean ---
pollutantmean <- function(directory, pollutant, id = 1:332){
    filepaths <- list.files(directory, "\\.csv$", full.names = TRUE)
    df <- do.call(rbind, lapply(filepaths, function(file){read.csv(file, 
                                                                   stringsAsFactors = FALSE, 
                                                                   header = TRUE )}))
    if (length(id) == 1) {
        df <- df[df$ID == id, ]
    } else {df <- df[df$ID%in%id, ]}
    
    # mean of pollutant 
    mean_pollutant <- mean(df[[pollutant]], na.rm = TRUE)
    return(mean_pollutant)
}

hw1.1 <- pollutantmean("./specdata", "sulfate", 100:150)

# part 2 ---
complete <- function(directory, id){
    filepaths <- list.files(directory, "\\.csv$", full.names = TRUE)
    df <- do.call(rbind, lapply(filepaths, function(file){read.csv(file, 
                                                                   stringsAsFactors = FALSE, 
                                                                   header = TRUE )}))
    if (length(id) == 1) {
        df <- df[df$ID == id, ]
    } else {df <- df[df$ID%in%id, ]}
    
    # number of complete cases ---
    nobs <- NULL
    for (i in seq_along(id)){
    nobs[i]<- nrow(na.omit(df[df$ID ==id[i],]))
    }
    df.complete <- data.frame(id = id, nobs = nobs)
    return(df.complete)
}

# part 3 ---
corr <- function(directory, threshold = 0){
    filepaths <- list.files(directory, "\\.csv$", full.names = TRUE)
    df <- do.call(rbind, lapply(filepaths, function(file){read.csv(file, 
                                                                   stringsAsFactors = FALSE, 
                                                                   header = TRUE )}))
    df <- na.omit(df)
    res_corr <- NULL
    for (i in unique(df$ID)){
        if (nrow(df[df$ID == i,]) > threshold) {
            res_corr[i] <- cor(df[df$ID == i,2], df[df$ID == i,3])
        } else {res_corr[i] <- NA}
    }
    return(res_corr)
}


RNGversion("3.5.1")  
set.seed(42)
cc <- complete("./specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


cr <- corr("./specdata")       
cr <- na.omit(cr)
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)    
cr <- na.omit(cr)
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("./specdata", 2000)                
n <- length(cr)                
cr <- corr("./specdata", 1000) 
cr <- na.omit(cr)
cr <- sort(cr)
print(c(n, round(cr, 4)))

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- na.omit(corr("specdata"))             
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- na.omit(corr("specdata", 129))             
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- na.omit(corr("specdata", 2000))               
n <- length(cr)                
cr <- na.omit(corr("specdata", 1000))                
cr <- sort(cr)
print(c(n, round(cr, 4)))


