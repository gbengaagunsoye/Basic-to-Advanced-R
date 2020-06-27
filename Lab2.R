1-pnorm(1.644854)

pros.dat =
  as.matrix(read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/pros.dat"))
#pros.dat

#2a
dim(pros.dat)
# prints the first 6 rows and all columns
pros.dat[1:6, 1:9]
pros.dat[1:6, ]
pros.dat[92:97, ]

#2b
head(pros.dat)
tail(pros.dat)

#2c
rownames(pros.dat)
colnames(pros.dat)
pros.dat[1:5,]

#2d
pros.dat[,c('lcavol', 'lweight')]
pros.dat.sub = as.matrix(pros.dat[,c('lcavol', 'lweight')])
dim(pros.dat.sub)
head(pros.dat.sub)

#2e
pros.dat.sub <- transform(pros.dat.sub, lcadensity = lweight / lcavol)
head(pros.dat.sub)
lcadensity= pros.dat.sub[, 'lcadensity']

#2f
pros.dat = cbind(pros.dat,lcadensity)
head(pros.dat)
pros.dat.sub$lcadensity <- NULL

#3a
hist(pros.dat.sub$lcavol, col =2, breaks =20)
hist(pros.dat.sub$lweight, col =2, breaks =20)
plot(pros.dat.sub$lweight,pros.dat.sub$lcavol,xlab="Log Cancer Weight",ylab="Log Cancer Volume",
     main="Plot of lweight vs lcavol")

#3b
names(pros.dat.sub)
plot(pros.dat$age, pros.dat$lweight, xlab="Log Cancer Age",ylab="Log Cancer Weight",
     main="Plot of lweight vs lcavol")

plot(pros.dat$age, pros.dat$lcavol, xlab="Log Cancer Age",ylab="Log Cancer Volume",
     main="Plot of Log Cancer Volume vs  Log Cancer Age")

pros.dat =
  as.matrix(read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/pros.dat"))
attach(pros.dat)
pros.dat.sub2 = as.matrix(pros.dat[,c('age', 'lweight')])
attach(pros.dat.sub2)
dim(pros.dat.sub2)
pros.dat.sub2
as.data.frame(pros.dat.sub2["age"])
is.recursive(pros.dat.sub2)
pros.dat.sub2
#as.data.frame(pros.dat.sub2)
age_weight = as.data.frame(pros.dat.sub2[,c('age', 'lweight')])
age_weight$age
plot(age_weight$lweight,age_weight$age,xlab="Log Cancer age",ylab="Log Cancer weight",
     main="Scatter Plot of Log Cancer weight vs age")
head(pros.dat)
pros.dat
pros.dat <- transform(pros.dat, lcadensity = lweight / lcavol)
head(pros.dat)

#3c
hist(pros.dat$lcadensity)
age_density = as.data.frame(pros.dat[,c('age', 'lcadensity')])
plot(age_density$age, age_density$lcadensity, xlab="Log Cancer age", ylab="Log Cancer density",
     main="Scatter Plot of Log Cancer density vs age")
age_density$age
head(pros.dat)

#3d
pros.dat[-length(pros.dat)]
length(pros.dat)
pros.dat = pros.dat[,-10]
head(pros.dat)

#4a
pros.dat = as.matrix(read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/pros.dat"))
has.svi = vector(length=97)
pros.dat <- transform(pros.dat, lcadensity = lweight / lcavol)
pros.dat = pros.dat[,-10]
pros.dat$svi[93]
for( i in 1:97){
  if (pros.dat$svi[i]==1)
    has.svi[i] <- TRUE
}

sum <-  0
for(i in 1:97){
  if (has.svi[i]==TRUE)
    sum <-  sum + 1
}
print(sum)
typeof(pros.dat)

#4b
pros.dat.svi = pros.dat[pros.dat[,c('svi')]==1,]
pros.dat.no.svi = pros.dat[pros.dat[,c('svi')]!=1,]

dim(pros.dat.svi)
dim(pros.dat.no.svi)

#4c
pros.dat.no.svi.avg = vector(length=ncol(pros.dat))
pros.dat.svi.avg = vector(length=ncol(pros.dat))
for( i in 1:ncol(pros.dat)){
  pros.dat.svi.avg[i] = mean(pros.dat.svi[,i])
  pros.dat.no.svi.avg[i] = mean(pros.dat.no.svi[,i])
  cat(pros.dat.svi.avg[i],"\t", pros.dat.no.svi.avg[i], "\n")
}


#5a
pros.dat.svi.sd = vector(length=ncol(pros.dat))
i = 1
typeof(pros.dat.svi.sd)
pros.dat.svi.sd
pros.dat.svi.sd[i] = sd(pros.dat.svi[,i])
pros.dat.svi.sd[i]

#5b
pros.dat.no.svi.sd = vector(length=ncol(pros.dat))
i = 1
pros.dat.no.svi.sd[i] = sd(pros.dat.no.svi[,i])
pros.dat.no.svi.sd[i]
ncol(pros.dat)

#5c
cat("pros.dat.svi.sd","\t", "pros.dat.no.svi.sd", "\n")
for( i in 1:ncol(pros.dat)){
  pros.dat.svi.sd[i] = sd(pros.dat.svi[,i])
  pros.dat.no.svi.sd[i] = sd(pros.dat.no.svi[,i])
  cat(pros.dat.svi.sd[i],"\t", pros.dat.no.svi.sd[i], "\n")
}

pros.dat.no.svi.avg = vector(length=ncol(pros.dat))
pros.dat.svi.avg = vector(length=ncol(pros.dat))
for( i in 1:ncol(pros.dat)){
  pros.dat.svi.avg[i] = mean(pros.dat.svi[,i])
  pros.dat.no.svi.avg[i] = mean(pros.dat.no.svi[,i])
  cat(pros.dat.svi.avg[i],"\t", pros.dat.no.svi.avg[i], "\n")
}

48.80952 - 17.63158
25.73445 - 25.06676


#5d

pros.dat = 
  read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/pros.dat")
sapply(pros.dat, mean)

par("mar")
par(mar=c(1,1,1,1))

lapply(pros.dat, plot(x=pros.dat$svi, y=pros.dat$lcavol))

mapply(baseplot, c("V1", "V3"), c("V2", "V4"))
pros.dat2 = pros.dat[,-5]
length(pros.dat2)

par(mfrow=c(3, 3))
myPlot<-function(index){ plot(pros.dat2[,index] ~ 
                                pros.dat$svi,
                              main=names(pros.dat2)[index],pch=16,xlab="SVI",ylab=names(pros.dat2)[index])}
lapply(1:8,FUN=myPlot)

names(pros.dat2)

t.test.by.ind = function(x, ind) {
  stopifnot(all(ind %in% c(0, 1)))
  return(t.test(x[ind == 0], x[ind == 1]))
}

tests = lapply(pros.dat2, FUN=t.test.by.ind)


rio = read.csv("http://www.stat.cmu.edu/~ryantibs/statcomp/data/rio.csv")
typeof(rio)
dim(rio)
head(rio)

?factor
