
#2a
shakespeare.lines = readLines("http://www.stat.cmu.edu/~ryantibs/statcomp/data/shakespeare.txt")

#2b
shakespeare.length = length(shakespeare.lines)
text = paste(shakespeare.lines, collapse=" ")

#2c
words = strsplit(text, split=split)[[1]]
words = words[words != ""]
wordtab = table(words)
wordtab

#2d
number.unique.words=length(wordtab)
number.total.words=sum(wordtab)
longest.word=words[which.max(nchar(words))]

#2f
set.seed(0)
(x = round(runif(5, -1, 1), 2))
sort(x, decreasing=TRUE)
order(x, decreasing=TRUE)

hist(nchar(words), col="lightblue", breaks=0:max(nchar(words)),
     xlab="Word length")