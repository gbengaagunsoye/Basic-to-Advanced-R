#1a
#First we performed a little bit of preprocessing because the name field contains full names separated by empty space, hence
#the datafram was inbalance (C) Gbenga S. and Victor 2020
myHeader = names(sprint.m.df)[2:9]
myHeader
sprint.m.df <- read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/sprint.m.dat", header=T, sep = "", fill = T, stringsAsFactors = T, row.names = NULL)
sprint.w.df <- read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/sprint.w.dat", header=T, sep = "", fill = T, stringsAsFactors = T, row.names = NULL)
head(sprint.m.df, 5)
head(sprint.w.df, 5)
sprint.m.df$Name <- paste(sprint.m.df$Wind, sprint.m.df$Name, sep=" ")
sprint.w.df$Name <- paste(sprint.w.df$Wind, sprint.w.df$Name, sep=" ")
names(sprint.m.df)
sprint.m.df = sprint.m.df[names(sprint.m.df)!= "Wind"]
sprint.w.df = sprint.w.df[names(sprint.w.df)!= "Wind"]
head(sprint.m.df["Country"])
head(sprint.w.df["Country"])
names(sprint.m.df) = myHeader
names(sprint.w.df) = myHeader
head(sprint.m.df, 5)
head(sprint.w.df, 5)
head(sprint.w.df["Wind"])
dim(sprint.w.df)
dim(sprint.m.df)


#1b
input.value = "4,8"
input.value = strsplit(input.value,split=",",fixed=TRUE)[[1]]
input.value = as.numeric(paste(input.value, collapse = "."))
class(input.value)

#1c
wind.measurements = as.factor(c("-2,0", "0,0", "0,6", "+1,7"))
typeof(wind.measurements)
wind.measurements[1]

myresult = as.character(levels(wind.measurements))[wind.measurements]
myresult = strsplit(myresult,split=",",fixed=TRUE)[[1]]
myresult = as.numeric(paste(myresult, collapse = "."))
myresult

factor.to.numeric = function(myVector){
  result <- vector(mode = "numeric", length = length(myVector))
  char_vect = as.character(levels(myVector))[myVector]
  for(i in 1:length(char_vect)){
    output.value = strsplit(char_vect[i],split=",",fixed=TRUE)[[1]]
    output.value = as.numeric(paste(output.value, collapse = "."))
    result[i] <- output.value
  }
  return(result)
}

factor.to.numeric(as.factor(c("0", "1,5")))

wind.measurements = factor.to.numeric(wind.measurements)
wind.measurements
typeof(wind.measurements)

#1d
head(sprint.w.df["Wind"])
sprint.m.df$Wind = lapply(sprint.m.df$Wind, function(i) factor.to.numeric(i))
sprint.w.df$Wind = lapply(sprint.w.df$Wind, function(i) factor.to.numeric(i))
head(sprint.m.df["Wind"],5)
sprint.w.df["Wind"]

sprint.w.df$Wind[which(sprint.w.df$Wind == "NA")] = 0
sprint.m.df$Wind[which(sprint.m.df$Wind == "NA")] = 0
sum(which(sprint.m.df$Wind == "NA"))
typeof(sprint.m.df$Wind)

sprint.w.df.new<- read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/sprint.w.dat", 
                             header=T, sep = "", fill = T, stringsAsFactors = T, row.names = NULL)
sprint.w.df.new[[13,3]]
sprint.w.df[[13,3]]

typeof(sprint.w.df["Country"])
typeof(sprint.w.df.new["Rank"])

#1e

min(as.numeric(sprint.w.df$Time))
plot(as.numeric(sprint.m.df$Time), sprint.m.df$Wind, xlab="Wind", pch=19, ylab="Time",
     main = "plot of 100m sprint time versus the wind measurements")
plot(as.numeric(sprint.w.df$Time), sprint.w.df$Wind, xlab="Wind", pch=19, ylab="Time",
     main = "Women's Plot of 100m sprint time versus the wind measurements")

#2a
head(sprint.m.df["Birthdate"])

date.to.numeric = function(myVector){
  if(is.na(myVector)){
    result = 0
  }
  else{
  char_vect = as.character(levels(myVector))[myVector]
  output.value = rev(strsplit(char_vect,split=".",fixed=TRUE)[[1]])
  output.value = as.numeric(paste(output.value, collapse = ""))
  result <- output.value
  }
  return(result)
}


date.to.numeric(as.factor("16.08.2009"))
sprint.m.df$Birthdate = lapply(sprint.m.df$Birthdate, function(i) date.to.numeric(i))
sprint.m.df$Date = lapply(sprint.m.df$Date, function(i) date.to.numeric(i))
head(sprint.m.df$Birthdate)
warnings()
sprint.w.df$Wind = lapply(sprint.w.df$Wind, function(i) factor.to.numeric(i))

