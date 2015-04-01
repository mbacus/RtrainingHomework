##    R TRAINING 4 HOMEWORK
##
##    (data not provided; names changed to preserve data confidentiality)
##
##    The following code takes data from different files, and defines/uses
##    two main functions, dataCount() and sumMerge(), and other "sub-functions"
##    to manipulate these data to build a data frame summarizing the counts of
##    unique entries or sums of values per specific month of a certain year in
##    yyyymm format
##    The code then deparses the output data frame for use in R Training 5. 

rm(list=ls())

catName <- "YrMonth"        ##    sets the name for the output data frame's 
                            ##    1st column, containing the specific month
                            ##    of a certain year in yyyymm format			

catMin <- 201302            ##    sets the first month in the data range	
catMax <- 201410            ##    sets the last month in the data range


##    dataCount() takes a specified csv file in a specified directory & counts
##                the no. of unique entries in a certain column per month
##
##    Takes 4 arguments:
##    1. table        file name, e.g. "foo" for a csv file named foo
##    2. valSource    name of the column whose unique entries per month 
##                    will be counted
##    3. catSource    name of the column containing the date and time
##                    that correspond to each entry in valSource in such
##                    format as "01-JAN-15 12.00.00.000000000 AM"
##    4. directory    the directory that contains the file to be loaded,
##                    e.g. "folder 1"; the default is the working directory
##
##    Calls 3 functions: dataSource(), dateFormat(), and uniqueCount()
##
##    Returns a data frame w/ the 2nd column containing the count of unique 
##    entries in valSource for each month under the 1st column (w/ header
##    defined by catName and bounded by catMin and catMax) in yyyymm format

dataCount <- function(table, valSource, catSource, directory=getwd()){
	
        data <- dataSource(table, directory)
        data <- data.frame(data[[catSource]],data[[valSource]])
	
        colnames(data) <- c(catSource,valSource)
        data[[catName]] <- dateFormat(data, catSource)
		
        y <- data.frame(tapply(data[[valSource]],data[[catName]],uniqueCount))
        y <- data.frame(as.numeric(row.names(y)),y)
        colnames(y) <- c(catName,valSource)
        y <- y[catMin<=y[,1]&y[,1]<=catMax,]
        row.names(y) <- 1:nrow(y)
        y
}


##    sumMerge() sums the values in a specified vector per yyyymm month
##               in a corresponding vector, and appends this information 
##               in a specified data frame
##
##    Takes 4 arguments:
##    1. val          the vector whose sum of values per month will be computed
##    2. cat          the vector containing the yyyymm month corresponding
##                    to each value in val
##    3. valName      the header of the column in the output data frame that will
##                    contain the sum of values in val per yyyymm month
##    4. baseData     the data frame to w/c the column with header defined by 
##                    valName will be appended, corresponding to each month in
##                    baseData's 1st column (w/ header defined by catName and
##                    values bounded by catMin and catMax) in yyyymm format
##
##    Returns a data frame extending baseData by a column to contain the sum of 
##    values in val for each yyyymm month under the 1st column (w/ header  
##    defined by catName and values bounded by catMin and catMax)

sumMerge <- function(val,cat,valName,baseData){
        z <- data.frame(tapply(val,cat,sum))
        z <- data.frame(as.numeric(row.names(z)),z)
        colnames(z)[which(names(z)==names(z[2]))] <- valName
        colnames(z)[which(names(z)==names(z[1]))] <- catName
        z <- z[catMin<=z[,1]&z[,1]<=catMax,]
        row.names(z) <- 1:nrow(z)
        baseData <- merge(baseData,z,by.x=catName,by.y=catName)
}


##    dataSource() loads a specified csv file in a specified directory into R
##
##    Takes 2 arguments:
##    1. table        file name, e.g. "foo" for a csv file named foo
##    2. directory    the directory that contains the file to be loaded,
##                    e.g. "folder 1"; the default is the working directory

dataSource <- function(table, directory=getwd()){
        filename <- paste(table,".csv",sep="")
        filepath <- file.path(directory,filename)
        data <- read.csv(filepath)
}


##    dateFormat() coverts a specified column of a data frame from a 
##                 "01-JAN-15 12.00.00.000000000 AM" format into yyyymm,
##                 e.g. 201401
##
##    Takes 2 arguments:
##    1. data         the data frame containing the column to be converted
##    2. catSource    the column to be converted

dateFormat <- function(data,catSource){
        data[[catSource]] <- strptime(data[[catSource]], "%d-%b-%y %I.%M.%OS %p")
        data[[catSource]] <- as.Date(data[[catSource]])
        data[[catName]] <- data.frame(as.numeric(format(data[[catSource]],"%Y%m")))
}


##    dateFormat2() converts a month number (e.g. 1 for January) and a year  
##                  into yyyymm format (e.g. 201501 for January 2015)
##
##    Takes 2 arguments:
##    1. mon          the month number
##    2. yr           the year

dateFormat2 <- function (mon,yr) as.numeric(paste(yr,sprintf("%02i",mon),sep=""))


##    uniqueCount() counts the number of unique entries in a given vector x

uniqueCount <- function(x) length(unique(x))



##    Now the data manipulation using the above-defined functions begins...


##    Building the output data frame's first 3 columns:
##    (names changed to preserve data confidentiality)

y1 <- dataCount("train4File1","VALUE_1","TIME_STAMP_1")
y2 <- dataCount("train4File2","VALUE_2","TIME_STAMP_2")
y <- merge(y1,y2,by.x=catName,by.y=catName)

rm(y1,y2)
hold0 <- y                  ##    value holder to aid review


##    Building the output data frame's next 4 columns:
##    (name changed to preserve data confidentiality)

data1 <- dataSource("train4File3")
data1[1] <- data.frame(mapply(dateFormat2,data1[,2],data1[,1]))

for(i in 4:7)y <- sumMerge(data1[[i]],data1[[1]],names(data1[i]),y)

rm(data1)
hold1 <- y                  ##    value holder to aid review


##    Building the output data frame's 8th column:
##    (name changed to preserve data confidentiality)

data2 <- dataSource("train4File4")
data2 <- data.frame(data2[13],data2[12])
colnames(data2)[which(names(data2)==names(data2[1]))] <- catName
data2[1] <- dateFormat(data2, catName)
y <- sumMerge(data2[[2]],data2[[1]],names(data2[2]),y)

rm(data2)
hold2 <- y                  ##    value holder to aid review


##    Building the output data frame's 9th and last column:
##    (names changed to preserve data confidentiality)

data3 <- dataSource("train4File5")
data3[1] <- data.frame(mapply(dateFormat2,data3[,2],data3[,1]))
for(j in 4:16){
        for(i in 1:nrow(data3)){
                data3[i,3]<-data3[i,3]+data3[i,j]
        }
}

data3 <- data.frame(data3[1],data3[,3])
RTraining4Ans <- sumMerge(data3[[2]],data3[[1]],"VALUE_8",y)


##    Deparsing the output data frame for R Training 5:
      dput(RTraining4Ans, file = "RTraining4Ans.R")         

rm(list=setdiff(ls(), "RTraining4Ans"))
RTraining4Ans