
#2a
shakespeare.lines = readLines("http://www.stat.cmu.edu/~ryantibs/statcomp/data/shakespeare.txt")
shakespeare.lines[1:5]
writeLines(readLines("http://www.stat.cmu.edu/~ryantibs/statcomp/data/shakespeare.txt", 5))
length(shakespeare.lines)
nchar(max(shakespeare.lines))
mean(nchar(shakespeare.lines))
sum(nzchar(shakespeare.lines, keepNA = FALSE)==FALSE)

#2b
shakespeare.lines=(shakespeare.lines[nzchar(shakespeare.lines, keepNA = FALSE)])
length(shakespeare.lines)

#2c
shakespeare.all = paste(shakespeare.lines, collapse=" ")
nchar(shakespeare.all)
sum(nchar(shakespeare.lines))
5690994 - 5560900 

#2d
typeof(shakespeare.all)
shakespeare.words = strsplit(shakespeare.all, split=" ")[[1]]
typeof(shakespeare.words)
length(shakespeare.words)
shakespeare.words.unique = unique(shakespeare.words[shakespeare.words != ""])
length(shakespeare.words.unique)

#2e
hist(nchar(shakespeare.words.unique), col="lightblue", breaks=50, xlab="Word length")

#2f
set.seed(0)
(x = round(runif(5, -1, 1), 2))
sort(x, decreasing=TRUE)
order(x, decreasing=TRUE)
sort(shakespeare.words.unique, decreasing=TRUE)
word_rank = order(shakespeare.words.unique, decreasing=TRUE)
for( x in word_rank[1:5]){
  cat(x,"\t", shakespeare.words.unique[x], "\n")
}

max(shakespeare.words.unique)

#3a
shakespeare.wordtab = table(shakespeare.words)
length(shakespeare.wordtab)
sum(shakespeare.wordtab["thou"])
sum(shakespeare.wordtab["rumour"])
sum(shakespeare.wordtab["gloomy"])
sum(shakespeare.wordtab["assassination"])

#3b
sum(shakespeare.wordtab == 1)
sum(shakespeare.wordtab == 2)
sum(shakespeare.wordtab >= 10)
sum(shakespeare.wordtab > 100)

#3c
shakespeare.wordtab.sorted = sort(shakespeare.wordtab, decreasing=TRUE)
shakespeare.wordtab.sorted[1:25]

#3d
shakespeare.words = shakespeare.words[shakespeare.words != ""]
shakespeare.wordtab = table(shakespeare.words)
shakespeare.wordtab.sorted = sort(shakespeare.wordtab, decreasing=TRUE)
shakespeare.wordtab.sorted[1:25]

#3e
num_words = length(shakespeare.wordtab.sorted)
plot(1:num_words, as.numeric(shakespeare.wordtab.sorted), xlim = c(1,1000), type = "l", xlab="ranks", ylab="word counts")

#3e Challenge
C = 215; a = 0.57
shakespeare.wordtab.sorted.zipf = C*(1/1:num_words)^a
cbind(shakespeare.wordtab.sorted[1:8], shakespeare.wordtab.sorted.zipf[1:8])

#3e
plot(1:num_words, as.numeric(shakespeare.wordtab.sorted.zipf), type="l", xlim = c(1,1000),
     xlab="ranks", ylab="word counts")
curve(C*(1/x)^a, from=1, to=num_words, col="red", add=TRUE)


#4a
sum(shakespeare.wordtab["and,"])
shakespeare.all = tolower(shakespeare.all)
shakespeare.words.new = strsplit(shakespeare.all, split="[[:space:]]|[[:punct:]]")[[1]]
shakespeare.wordtab.new = table(shakespeare.words.new[shakespeare.words.new != ""])

#4b
length(shakespeare.words.new)
length(shakespeare.words)
length(shakespeare.wordtab.new)
length(shakespeare.wordtab)


#4c
shakespeare.words.new.unique = unique(shakespeare.words.new)

hist(nchar(shakespeare.words.new.unique), col="lightblue", breaks=50, xlab="Word length")

set.seed(0)
sort(shakespeare.words.new.unique, decreasing=TRUE)
word_rank = order(shakespeare.words.new.unique, decreasing=TRUE)
for( x in word_rank[1:5]){
  cat(x,"\t", shakespeare.words.new.unique[x], "\n")
}
max(shakespeare.words.unique)

#4d

shakespeare.wordtab.sorted.new = sort(shakespeare.wordtab.new, decreasing=TRUE)
shakespeare.wordtab.sorted.new[1:25]

num_words = length(shakespeare.wordtab.sorted.new)
plot(1:num_words, as.numeric(shakespeare.wordtab.sorted.new), xlim = c(1,1000), type = "l", xlab="ranks", ylab="new word counts")

#5a
shakespeare.lines[19:23]
shakespeare.lines = trimws(shakespeare.lines)
shakespeare.lines[19:23]
length(shakespeare.lines)

#5b
which(shakespeare.lines == "THE SONNETS")
toc.start = 23

which(shakespeare.lines == "VENUS AND ADONIS")
toc.end = 66

#5c
n = toc.end - toc.start + 1
titles <- vector(, n)
typeof(titles)
titles
shakespeare.lines[23]
for(i in toc.start:toc.end){
  titles[i] <- shakespeare.lines[i]
  print(titles[i])

}


