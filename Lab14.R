set.seed(1)
n = 30
x = sort(runif(n, -3, 3))
y = 2*x + 2*rnorm(n)
x0 = sort(runif(n, -3, 3))
y0 = 2*x0 + 2*rnorm(n)

par(mfrow=c(1,2))
xlim = range(c(x,x0)); ylim = range(c(y,y0))
plot(x, y, xlim=xlim, ylim=ylim, main="Training data")
plot(x0, y0, xlim=xlim, ylim=ylim, main="Test data")

poly(1:15, x)
fit = lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
coef(summary(fit))

for(k in 1:15){
  lm(y ~ poly(x, k), data = x)
}







#2a
pros.df = read.table(
  "https://web.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data")
dim(pros.df)
head(pros.df)

train = as.data.frame(pros.df[(which(pros.df$train == "TRUE")),])
test = pros.df[(which(pros.df$train == "FALSE")),]
dim(train)
dim(test)
names(train)
train.modified = as.data.frame(train[,names(train)])
train.lm = lm(formula=as.factor(train$lpsa)~as.factor(train$lcavol)+as.factor(train$lweight), data=train)
test.lm = lm(test$lpsa~test$lcavol+test$lweight, data=test)
head(train)
coef(test.lm)
summary(test.lm)

#2b
train.lm2 = lm(train$lpsa~train$age+train$gleason+train$pgg45, data=train)
pred = predict(train.lm2, test$lpsa)


#3a
wage.df = read.csv("http://www.stat.cmu.edu/~ryantibs/statcomp/data/wage.csv", 
                   skip=16)
dim(wage.df)
head(wage.df, 5)
#create a list of random number ranging from 1 to number of rows from actual data 
#and 70% of the data into training data
data1 = sort(sample(nrow(wage.df), nrow(wage.df)*0.5))
train<-wage.df[data1,]
test<-wage.df[-data1,]
dim(train)
dim(test)

#3b
train.lm = lm(wage.df$wage~wage.df$year+wage.df$age+wage.df$education, data=train)
train.lm2 = glm(wage.df$wage~wage.df$year+s(wage.df$age)+wage.df$education, data=train)
par(mfrow=c(2,2))
plot(train.lm2)



head(train)
coef(test.lm)
summary(test.lm)
