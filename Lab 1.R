set.seed(09262019)
rbinom(n=20, size=10, prob=0.5)

#1a
bin.draws.0.5 <- rbinom(n=300, size=15, prob=0.5)
bin.draws.0.5[1:25]
tail(bin.draws.0.5,275)

#1b
`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
bin.draws.0.5[6] %+=% bin.draws.0.5[1] 
max(bin.draws.0.5[2], bin.draws.0.5[5])
which(bin.draws.0.5==3)


#1c
mean(bin.draws.0.5)
sd(bin.draws.0.5)

#1d
summary(bin.draws.0.5)

#1e
typeof(bin.draws.0.5)
bin.draws.0.5.char = as.character(bin.draws.0.5)
typeof(bin.draws.0.5.char)
summary(bin.draws.0.5.char)


#2a
par(mfrow=c(1,1))
hist(bin.draws.0.5)

#2b
tabulate(bin.draws.0.5)

#2c
plot(bin.draws.0.5)


#2d
plot(1:300, bin.draws.0.5)


#3a

bin.draws.0.2 <- rbinom(n=300, size=15, prob=0.2)
bin.draws.0.2
bin.draws.0.3 <- rbinom(n=300, size=15, prob=0.3)
bin.draws.0.4 <- rbinom(n=300, size=15, prob=0.4)
bin.draws.0.6 <- rbinom(n=300, size=15, prob=0.6)
bin.draws.0.7 <- rbinom(n=300, size=15, prob=0.7)
bin.draws.0.8 <- rbinom(n=300, size=15, prob=0.8)

#3b
bin.vector = c(mean(bin.draws.0.2),mean(bin.draws.0.3),mean(bin.draws.0.4),mean(bin.draws.0.5),
               mean(bin.draws.0.6),mean(bin.draws.0.7),mean(bin.draws.0.8))

bin.vector

#3c


#4a
