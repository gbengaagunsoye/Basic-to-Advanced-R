#1a
huber <- function(x)
{
  if(abs(x)<= 1)
    result = x^2
  else
    result = 2*abs(x)-1
  return (result)
}
huber(1)
huber(4)

#1b
huber <- function(x,a)
{
  if(abs(x)<= a)
    result = x^2
  else
    result = 2*a*abs(x) - (a^2)
  return (result)
}
huber(3,2)
huber(3,4)

#1c
huber <- function(x,a=1)
{
  if(abs(x)<= a)
    result = x^2
  else
    result = 2*a*abs(x) - (a^2)
  return (result)
}
huber(3)

#1d
huber(a=1, x=3)
huber(1, 3)


#1e
huber <- function(x,a=1)
{
  ifelse(x <= a, x^2, 2*a*abs(x) - (a^2))
}

huber(x=1:6, a=3)

#challenge
x.vals = seq(0, 5, length=21)
x.vals
huber.vals = c(0.0000, 0.0625, 0.2500, 0.5625, 1.0000, 1.5625, 2.2500,
               3.0625, 4.0000, 5.0625, 6.2500, 7.5625, 9.0000, 10.5000,
               12.0000, 13.5000, 15.0000, 16.5000, 18.0000, 19.5000, 
               21.0000)
plot(x.vals, huber.vals, type = "l", xlab="x values", ylab="huber values")

#2a
huber <- function(x)
{
  ifelse(abs(x) <= 1, x*x, 2*abs(x) - 1)
}
x = seq(-2, 2)
x
y = huber(x)
y

plot(x, y, type = "l", xlab="x", ylab="psi(x)", main = "Huber")
axis(1, las=2)

#2b
huber <- function(x)
{
  ifelse(abs(x) <= 1, x^2, 2*abs(x) - 1)
  cat("Invented by the great Swiss statistician Peter Huber!")
}
huber(5)
huber(1)

#2challenge
huber <- function(x)
{
  ifelse(abs(x) <= 1, x^2, 2*abs(x) - 1)
  cat("Invented by the great Swiss statistician Peter Huber!")
}
plot(c(100, 200), c(300, 450), type= "n", xlab = "", ylab = "")
rect(100, 300, 200, 450, border = "red", lty =1, col = "red")
rect(130, 360, 170, 390, border = "white", col = "white")
rect(144, 320, 156, 430, border = "white", col = "white")

394-356
156-144

plot(c(100, 200), c(100, 200), type= "n", xlab = "", ylab = "")
rect(100, 100, 200, 200, border = "red", lty =1, col = "red")
rect(130, 140, 170, 160, border = "white", col = "white")
rect(140, 130, 160, 170, border = "white", col = "white")

#3a
huber.mod = function(x, a=1) {
  x.squared = x^2
  ifelse(abs(x) <= a, x.squared, 2*a*abs(x)-a^2)
}
x.squared = 999
huber.mod(x=3)
x.squared
x.squared <- c(1,3,5)
huber.mod(x=1)
x.squared

#3b
a <- -59.6
huber.mod(x=3, a=2)
a

#3c
huber.sloppy = function(x) {
  ifelse(abs(x) <= a, x^2, 2*a*abs(x)-a^2)
}
a = 1.5
huber.sloppy(x=3)
a = 4.0
huber.sloppy(x=3)
a= 1
huber.sloppy(x=3)
a = "hello"
huber.sloppy(x=3)
a = 5
huber.sloppy(x=3)
a =6
huber.sloppy(x=3)

#3d
huber <- function(x,a=1)
{
  ifelse(x <= a, x^2, 2*a*abs(x)-a^2)
}

huber(x=3, a<-5)
a = 6
huber(x=3, a<-1)
a

#3e
huber(x=3, b<-20)
b

#3Challenges


#4a

# get.wordtab.from.url: get a word table from text on the web
# Inputs:
# - str.url: string, specifying URL of a web page 
# - split: string, specifying what to split on. Default is the regex pattern
#   "[[:space:]]|[[:punct:]]"
# - tolower: Boolean, TRUE if words should be converted to lower case before
#   the word table is computed. Default is TRUE
# - keep.nums: Boolean, TRUE if words containing numbers should be kept in the
#   word table. Default is FALSE
# Output: list, containing lines, words, word table, and some basic summaries
# We included in the return list lines and word (C) Gbenga S. & Victor Spring 2020

get.wordtab.from.url = function(str.url, split="[[:space:]]|[[:punct:]]",
                                tolower=TRUE, keep.nums=FALSE) {
  lines = readLines(str.url)
  text = paste(lines, collapse=" ")
  words = strsplit(text, split=split)[[1]]
  words = words[words != ""]
  
  # Convert to lower case, if we're asked to
  if (tolower) words = tolower(words)
  
  # Get rid of words with numbers, if we're asked to
  if (!keep.nums) 
    words = grep("[0-9]", words, inv=TRUE, val=TRUE)
  
  # Compute the word table
  wordtab = table(words)
  
  return(list(wordtab=wordtab,
              number.unique.words=length(wordtab),
              number.total.words=sum(wordtab),
              longest.word=words[which.max(nchar(words))],
              lines = lines,
              words = words))
}

shakespeare.wordobj = get.wordtab.from.url("http://www.stat.cmu.edu/~ryantibs/statcomp/data/shakespeare.txt")
head(shakespeare.wordobj)
head(shakespeare.wordobj$lines)
head(shakespeare.wordobj$words)
head(shakespeare.wordobj$wordtab)

#4b
#Lab4 5c
#shakespeare.lines = shakespeare.wordobj$lines
#shakespeare.lines = trimws(shakespeare.lines)
#shakespeare.lines


which(shakespeare.lines == "THE SONNETS")
toc.start =  which(shakespeare.lines == "THE SONNETS")[1]
which(shakespeare.lines == "VENUS AND ADONIS")
toc.end = which(shakespeare.lines == "VENUS AND ADONIS")[1]

n = toc.end - toc.start + 1
n
toc.start
#where n is the number of play titles
titles <- vector(mode = "character", n)


for(i in 1:n){
  titles[i] = shakespeare.lines[i+toc.start-1]
  print(titles[i])
  
}
length(titles)


#Lab4 5d
titles.start = vector(mode="integer", length = length(titles))
length(titles.start)


which(shakespeare.lines == titles[1])
                       
for(i in 1:length(titles.start)){
  titles.start[i] = which(shakespeare.lines == titles[i])[2]
}
titles.start
#shakespeare.lines[2915]

#Lab4 5e
titles.end = vector(mode="integer", length = length(titles.start))
length(titles.end)
for(i in 1:length(titles.end)){
  if(i== length(titles.end)){
    titles.end[i] = length(shakespeare.lines)
  }
  else
    titles.end[i] = titles.start[i+1]-1
}
titles.end
which(shakespeare.lines == "THE TWO NOBLE KINSMEN")


#Lab4 5f
grep(pattern="cat",
     x=c("cat", "canned goods", "batman", "catastrophe", "tomcat"))

titles.start = vector(mode="integer", length = length(titles))
length(titles.start)

for(i in 1:length(titles.start)){
  titles.start[i] = grep(pattern = titles[i], shakespeare.lines)[2]
}
titles.start

#Lab4 5e
titles.end = vector(mode="integer", length = length(titles.start))
length(titles.end)
for(i in 1:length(titles.end)){
  if(i== length(titles.end)){
    titles.end[i] = length(shakespeare.lines)
  }
  else
    titles.end[i] = titles.start[i+1]-1
}
titles.end


#4c
titles.start
titles.end

shakespeare.lines.by.play = list(length(titles))
for(i in 1:length(titles)){
      shakespeare.lines.by.play[[i]] = shakespeare.lines[titles.start[i]:titles.end[i]]
}
head(shakespeare.lines.by.play[[44]])
names(shakespeare.lines.by.play) <- titles
names(shakespeare.lines.by.play)
head(shakespeare.lines.by.play["THE SONNETS"])

#4d
lapply(shakespeare.lines.by.play, function(i) head(i, 4))


#5a
#We define this function to take lines as its first argument
#This function takes a string vector passed by the user that
#contains lines of text to be processed.(C)Gbenga S. Agunsoye and Victor U. 2020
# Inputs:
# - split: string, specifying what to split on. Default is the regex pattern
#   "[[:space:]]|[[:punct:]]"
# - tolower: Boolean, TRUE if words should be converted to lower case before
#   the word table is computed. Default is TRUE
# - keep.nums: Boolean, TRUE if words containing numbers should be kept in the
#   word table. Default is FALSE
# Output: list, words, word table, and some basic summaries


get.wordtab.from.lines = function(lines, split="[[:space:]]|[[:punct:]]",
                                tolower=TRUE, keep.nums=FALSE) {
  text = paste(lines, collapse=" ")
  words = strsplit(text, split=split)[[1]]
  words = words[words != ""]
  
  # Convert to lower case, if we're asked to
  if (tolower) words = tolower(words)
  
  # Get rid of words with numbers, if we're asked to
  if (!keep.nums) 
    words = grep("[0-9]", words, inv=TRUE, val=TRUE)
  
  # Compute the word table
  wordtab = table(words)
  
  return(list(wordtab=wordtab,
              number.unique.words=length(wordtab),
              number.total.words=sum(wordtab),
              longest.word=words[which.max(nchar(words))],
              words = words))
}


#5b
shakespeare.wordobj.by.play = lapply(shakespeare.lines.by.play,
                                     function(i) get.wordtab.from.lines(i))
typeof(shakespeare.wordobj.by.play)
head(shakespeare.wordobj.by.play[[1]],4)
shakespeare.wordobj.by.play[[1]]$number.total.words


#5c

"[["(shakespeare.wordobj, "number.total.words")
shakespeare.wordobj[["number.total.words"]]
"[["(shakespeare.wordobj.by.play[["THE SONNETS"]], "number.total.words")

shakespeare.total.words.by.play = lapply(shakespeare.wordobj.by.play, 
                                    function(i) "[["(shakespeare.wordobj.by.play[[1]], "number.total.words"))
shakespeare.unique.words.by.play = lapply(shakespeare.wordobj.by.play, 
                                         function(i) "[["(shakespeare.wordobj.by.play[[1]], "number.unique.words"))

shakespeare.total.words.by.play[1]
shakespeare.unique.words.by.play[1]

#5d

#I Could not generate the total and unique for each element using lapply




#5d

