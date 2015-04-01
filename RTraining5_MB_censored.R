##    R TRAINING 5 HOMEWORK
##
##    (data not provided; names changed to preserve data confidentiality)
##
##    The following code takes the sum per month of a certain value, then
##    incorporates time value into the sums by applying a specified
##    depreciation factor. The time-adjusted sums are then aggregated per month
##    to represent the sums of depreciated values per month.
##    The code then reads "RTraining4Ans.R", the deparsed output data frame 
##    from "RTraining4_MB.R", and then merges the aggregated depreciated
##    values of each yyyymm month


outputName <- "AMOUNT_DEP"  ##    sets the name of the column to be added
                            ##    to Training 4 homework's output data frame

catName <- "YrMonth"        ##    sets the name for the output data frame's 
                            ##    1st column, containing the specific month
                            ##    of a certain year in yyyymm format			

catMin <- 201302            ##    sets the first month in the data range	
catMax <- 201410            ##    sets the last month in the data range

DepFactor <- 1-0.84/12      ##    sets the depreciation factor 


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


##    sepYrMo()    coverts a specified vector from yyyymm format into a data frame
##                 containing years in the 1st column and months in the 2nd

sepYrMo <- function (yrMo){
        Yr<-data.frame(Yr=as.numeric(substr(yrMo,start=1, stop=4)))
        data.frame(Yr,Mo=as.numeric(substr(yrMo,start=5, stop=6)))
}



##    Now the data manipulation begins...
##
##    Getting the data frame with the 1st column containing time stamps of such
##    format as "01-JAN-15 12.00.00.000000000 AM" with corresponding values of
##    a certain variable in the second column
##    (names changed to preserve data confidentiality)

data2 <-read.csv("train5File.csv")
data2 <- data.frame(data2[13],data2[12])


##    Coverting the time stamps in the 1st column into yyyymm format,e.g. 201401
##    and then aggregating the values in the 2nd column corresponding to each
##    yyyymm month

colnames(data2)[which(names(data2)==names(data2[1]))] <- catName
data2[1] <- dateFormat(data2, catName)
dataSome <- data.frame(tapply(data2[[2]],data2[[1]],sum))
dataSome <- data.frame(as.numeric(row.names(dataSome)),dataSome)


##    Generating permutations of "from and to" months from the yyyymm month 
##    corresponding to each aggregated value of the variable under consideration.
##    The 1st column contains the yyyymm month in w/c a value has not yet
##    accumulated depreciation while the 2nd column contains the yyyymm month
##    for which its depreciated value is to be computed.

dataMo <- data.frame(dataSome[1],dumCol=rep(0,nrow(dataSome)))
fro_to <-as.matrix(data.frame(merge(dataMo, dataMo, by.x="dumCol", by.y="dumCol")[,c(2,3)],AMOUNT=rep(0,nrow(dataMo)^2)))

for (i in 1:nrow(fro_to)){
        fro_to[i,3]<-dataSome[mapply(prod,dataSome[,1]==fro_to[i,1],1:nrow(dataSome)),2]
}


##    Computing the depreciated values by applying the depreciation factor
##    defined by DepFactor

fro<-sepYrMo(fro_to[,1])
to<-sepYrMo(fro_to[,2])
moDiff<-matrix(rep(0,nrow(fro_to)))

for (i in 1:nrow(fro_to)){
        moDiff[i] <- (to[i,1]-fro[i,1])*12+to[i,2]-fro[i,2]
        fro_to[i,3]<-fro_to[i,3]*((DepFactor)^moDiff[i,])*(1-(moDiff[i,]<0))
}


##    Aggregating the depreciated values per yyyymm month

fro_to_sum<-data.frame(aggregate(fro_to[,3]~fro_to[,2],data=fro_to,sum))
fro_to_sum <- fro_to_sum[catMin<=fro_to_sum[,1]&fro_to_sum[,1]<=catMax,]
names(fro_to_sum) <- c(catName,outputName)
row.names(fro_to_sum) <- 1:nrow(fro_to_sum)


##    Getting the data frame generated and deparsed from "RTraining4_MB.r" and
##    then merging the aggregated depreciated values of each yyyymm month

RTraining4Ans <- dget("RTraining4Ans.R")
RTraining5Ans <- merge(RTraining4Ans,fro_to_sum,by.x=catName, by.y=catName)

rm(list=setdiff(ls(), "RTraining5Ans"))
RTraining5Ans