state.df = data.frame(state.x77, Region=state.region, Division=state.division)

#1a
state.df = data.frame(state.df, Abbr=sample(c(T,F), nrow(state.df), rep=TRUE))
head(state.df, 3, 11)

#1b
state.df$Region = NULL
head(state.df, 4)

#1c
state.df$longitude = sample(1:100, nrow(state.df), replace=TRUE)
head(state.df, 3, 12)

#1d
#state.df.ne.1 = subset(state.df, state.df.longitude <-100)
# Using subset(), we can just use the column names directly (i.e., no need for
# using $)
state.df.ne.1 = subset(state.df, Division == "New England")
# Get same thing by extracting the appropriate rows manually
state.df.ne.2 = state.df[state.df$Division == "New England", ]
all(state.df.ne.1 == state.df.ne.2)


#daply(state.df, .(Region), function(df) mean.sd(df$Frost)) # Get back array
#ddply(state.df, .(Region), function(df) mean.sd(df$Frost)) # Get back df
#dlply(state.df, .(Region), function(df) mean.sd(df$Frost)) # Get back list
# First create a variable that indicates whether the area is big or not
state.df$AreaBig
# Now use (say) ddply() to compute the mean and sd Frost, for each region, but
# separately over big and small areas
#ddply(state.df, .(Region, AreaBig), function(df) mean.sd(df$Frost))

# We can also create factor variables on-the-fly with I() 
#ddply(state.df, .(Region, I(Area > 50000)), function(df) mean.sd(df$Frost))

#daply(.data, .variables, .fun = NULL, ..., .progress = "none",
 #     .inform = FALSE, .drop_i = TRUE, .drop_o = TRUE, .parallel = FALSE,
 #     .paropts = NULL)


#2a
pros.dat =
  as.matrix(read.table("http://www.stat.cmu.edu/~ryantibs/statcomp/data/pros.dat"))

#2b
head(pros.dat)
tail(pros.dat)

#2c
rownames(pros.dat)
colnames(pros.dat)

pros.dat.svi.sd = vector(length=ncol(pros.dat))
i = 1

#hist(pros.dat$age, breaks=100, xlab="Residuals", ylab="Frequency", main="Histogram of Residuals")


#3a
rio = read.csv("http://www.stat.cmu.edu/~ryantibs/statcomp/data/rio.csv")
rownames(rio)
colnames(rio)

#3b
rio$total = sample(1:100, nrow(rio), replace=TRUE)
head(state.df, 3, 12)