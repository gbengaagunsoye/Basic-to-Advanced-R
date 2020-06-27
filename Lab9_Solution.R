pros.df = 
  read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/pros.dat")
dim(pros.df)
head(pros.df, 3)
x = min(pros.df$lcp)
log(0.25)
y = which(pros.df$lcp > min(pros.df$lcp))
all_pros.df.subset = pros.df[y,]
pros.df.subset = pros.df$lcp[pros.df$lcp > min(pros.df$lcp)]
all_pros.df.subset = pros.df[y,]
head(pros.df.subset, 3)
hist(pros.df.subset, col =2, breaks =20)

#1b
pros.df.subset.corr = cor(all_pros.df.subset)
pros.df.subset.corr
install.packages("corrplot")
library(corrplot)
version
corrplot(pros.df.subset.corr, title = "Correlation of pros.df.subset")

#challenge: Heatmap
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = pros.df.subset.corr, col = palette, symm = TRUE)

#1c
pros.lm = lm(formula = pros.df$lpsa ~ pros.df$lcavol, data = pros.df)
pros.subset.lm = lm(formula = all_pros.df.subset$lpsa ~ all_pros.df.subset$lcavol, data = all_pros.df.subset)
coef(pros.lm)
coef(pros.subset.lm)

#1d
plot(pros.df$lcavol, pros.df$lpsa, pch = 20, xlab="Log Cancer Volume", ylab="log PSA score", main = "Plot of LPSA vs LCAVOL")
points(all_pros.df.subset$lcavol,all_pros.df.subset$lpsa, col="red", pch = 20)
summary(pros.lm)
abline(pros.lm)
abline(pros.subset.lm, col="red")
legend(-1, 5, legend=c("pros.lm", "pros.subset.lm"),
       col=c("black", "red"), lty=1:1, cex=0.8)

#1e
par("mar")
par(mar=c(5.1, 4.1, 4.1, 2.1))
pros.df.svi = pros.df[(which(pros.df$svi == 1)),]
pros.df.no.svi = pros.df[(which(pros.df$svi == 0)),]
head(pros.df.no.svi)
all_pros.df.subset.svi = all_pros.df.subset[(which(all_pros.df.subset$svi == 1)),]
all_pros.df.subset.no.svi = all_pros.df.subset[(which(all_pros.df.subset$svi == 0)),]
head(all_pros.df.subset.svi)
par(mfrow=c(1, 2))

pros.subset.svi.lm = lm(formula = all_pros.df.subset.svi$lpsa ~ all_pros.df.subset.svi$lcavol,
                        data = all_pros.df.subset.svi)
pros.subset.no.svi.lm = lm(formula = all_pros.df.subset.no.svi$lpsa ~ all_pros.df.subset.no.svi$lcavol,
                        data = all_pros.df.subset.no.svi)
plot(all_pros.df.subset.svi$lcavol, all_pros.df.subset.svi$lpsa, 
     pch = 20, xlab="Log Cancer Volume", ylab="log PSA score", main = "Plot of Subset LPSA vs LCAVOL for SVI and non SVI")
points(all_pros.df.subset.no.svi$lcavol,all_pros.df.subset.no.svi$lpsa, col="red", pch = 20)
summary(pros.subset.svi.lm)
abline(pros.subset.svi.lm)
abline(pros.subset.no.svi.lm, col="red")
legend(1.5, 5.0, legend=c("pros.subset.svi.lm", "pros.subset.no.svi.lm"),
       col=c("black", "red"), lty=1:1, cex=0.8)

#2a

headers = read.csv("http://www.stat.cmu.edu/~ryantibs/statcomp/data/wage.csv", 
                   skip = 16, header = F, nrows = 1, as.is = T)
wage.df = read.csv("http://www.stat.cmu.edu/~ryantibs/statcomp/data/wage.csv", skip = 17, header = F)
wage.df = wage.df[,2:12]
colnames(wage.df)= headers
dim(wage.df)
head(wage.df, 3)

#2b
wage.df[typeof(wage.df)=="integer"]
par(mfrow=c(1,3))
hist(wage.df$year, col="red")
hist(wage.df$age, col="blue")
hist(wage.df$wage, col="green")

#2c
is.numeric(wage.df)


#3a

wage.lm = lm(wage.df$wage~wage.df$year+wage.df$age, data=wage.df)
coef(wage.lm)
summary(wage.lm)

#3b
wage.sum = summary(wage.lm)
typeof(wage.sum)
tail(wage.sum,3)
coefficients = as.data.frame(wage.sum$coefficients)
wage.se = coefficients$`Std. Error`[2:3]

#3c
par(mfrow=c(2,2)) 
plot(wage.lm)

#3d
wage.df.lt250 = wage.df[(which(wage.df$wage < 250)),]
wage.lm.lt250 = lm(wage.df.lt250$wage~wage.df.lt250$year+wage.df.lt250$age, data=wage.df.lt250)
coef(wage.lm.lt250)
summary(wage.lm.lt250)
par(mfrow=c(2,2)) 
plot(wage.lm.lt250)

#3e
wage.df.lt250.predict = wage.df.lt250[(which(wage.df.lt250$age ==30)),]
head(wage.df.lt250.predict)
predict(wage.lm.lt250, c(30))
wage.lm.lt250
#how do I provide new data to predict on?

#4a
wage.high = wage.df[(which(wage.df$wage > 250)),]
as.factor(wage.high$wage)
##We had to convert wage.high to factor for it to fit into the logistic regression model
wage.glm = glm(formula = as.factor(wage.high$wage)~wage.high$year+wage.high$age, family = "binomial",
                        data=wage.high)
coef(wage.glm)
summary(wage.glm)

#4b
wage.glm2 = glm(formula = as.factor(wage.high$wage)~wage.high$year+wage.high$age+wage.high$education, family = "binomial",
               data=wage.high)
coef(wage.glm2)
summary(wage.glm2)

#4c
y = unique(wage.df["education"])
for(x in y){
  if(is.na(which(wage.high$education == x)))
    print("Incomplete")
}
  

#4d

#5a
install.packages("gam")
library(gam)
wage.glm2 = glm(formula = as.factor(wage.high$wage)~wage.high$year+wage.high$age+wage.high$education,
                family = "binomial", data=wage.high)
coef(wage.glm2)
summary(wage.glm2)
tail(wage.glm.sum,3)
wage.glm.sum$df.residual
coefficients = as.data.frame(wage.glm.sum$coefficients)
