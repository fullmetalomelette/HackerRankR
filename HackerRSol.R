# WARMUP - Compare the Triplets

data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

ascore <- 0
bscore <- 0

for (i in 1:3) {
  if (data[1,i] > data[2,i]) {
    ascore <- ascore + 1
  }
  else if (data[1,i] < data[2,i]) {
    bscore <- bscore + 1
  }
}

output <- c(ascore,bscore)
write.table(output, sep = " ", eol=" ", append=T, row.names = F, col.names = F)

# WARMUP - A Very Big Sum

data <- read.table("/dev/stdin", sep="",fill=TRUE);

sum <- 0
for (i in 1:dim(data)[2]) {
  sum <- sum + data[2,i]
}

write.table(sum, sep = " ", eol=" ", append=T, row.names = F, col.names = F)

# WARMUP - Diagonal Difference

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)

msize <- data[1,1]

ldiag <- 0
rdiag <- 0

for (i in 1:msize) {
  ldiag <- ldiag + data[(i+1),i]
  rdiag <- rdiag + data[(i+1),(msize-i+1)]
}

sum <- abs(ldiag-rdiag)
write.table(sum, sep = " ", eol=" ", append=T, row.names = F, col.names = F)

# IMPLEMENTATION - Kangaroo

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)

answer <- "NO"
if (((data[3]-data[1])/(data[2]-data[4]))%%1==0 && 
    ((data[3]-data[1])/(data[2]-data[4]))>0 && (data[2] != data[4])){
  answer <- "YES"
}

write.table(answer, sep = " ", eol=" ", append=T, row.names = F, col.names = F, quote=FALSE)

# IMPLEMENTATION - Bigger is Greater

data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)
data <- t(data)

ssize <- data[1]
output <- rep(NA,ssize)

limit1 <- as.numeric(ssize)+1

for (i in 2:(limit1)){ 
  string <- data[i]
  string <- strsplit(string,"")
  string <- unlist(string)
  strsize <- length(string) 
  for (j in 1:strsize) {
    if ((j-strsize)==0) {
      output[i-1] <- "no answer"
      break
    } else if (string[strsize-j+1] > string[strsize-j]) {
      pivot <- string[strsize-j]
      for (k in 1:strsize) {
        if (string[strsize-k+1] > pivot) {
          string[strsize-j] <- string[strsize-k+1]
          string[strsize-k+1] <- pivot
          break
        }
      }
      string[(strsize-j+1):strsize] <- rev(string[(strsize-j+1):strsize])
      output[i-1] <- paste(string,collapse='')
      break
    } 
  }
}

write.table(output, sep = "\n",append=T, row.names = F, col.names = F, quote=FALSE)

# Apply Method

data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)
data <- t(data)

ssize <- data[1]
data <- data[,-1]


limit1 <- as.numeric(ssize)+1

#for (i in 2:(limit1)){ 
npermutation <- function(xstring) {
  string <- xstring
  string <- strsplit(string,"")
  string <- unlist(string)
  strsize <- length(string) 
  for (j in 1:strsize) {
    if ((j-strsize)==0) {
      write.table("no answer", sep = "\n",append=T, row.names = F, col.names = F, quote=FALSE)
      break
    } else if (string[strsize-j+1] > string[strsize-j]) {
      pivot <- string[strsize-j]
      for (k in 1:strsize) {
        if (string[strsize-k+1] > pivot) {
          string[strsize-j] <- string[strsize-k+1]
          string[strsize-k+1] <- pivot
          break
        }
      }
      string[(strsize-j+1):strsize] <- rev(string[(strsize-j+1):strsize])
      write.table(paste(string,collapse=''), sep = "\n",append=T, row.names = F, 
                  col.names = F, quote=FALSE)
      break
    } 
  }
  return(NULL)
}

invisible(sapply(data,npermutation))
#}

# IMPLEMENTATION - Cavity Map

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

msize <- data[1,1] 
grid <- data[-1,]

charsplit <- function(rline) {
  return(as.numeric(strsplit(as.character(rline),"")[[1]]))
}

cgrid <- sapply(grid,charsplit)
cgrid <- t(cgrid)
ogrid <- cgrid

for (i in 2:(msize-1)) {
  for (j in 2:(msize-1)) {
    if ((cgrid[i,j]) > (cgrid[i-1,j]) && (cgrid[i,j]) > (cgrid[i+1,j]) 
        && (cgrid[i,j]) > (cgrid[i,j-1]) && (cgrid[i,j]) > (cgrid[i,j+1])) {
      ogrid[i,j] <- "X"
    }  
  }
}

charjoin <- function (eline) {
  return(paste(eline,collapse=''))
}

output <- apply(ogrid,1,charjoin)

write.table(output,append=T, row.names = F, col.names = F, quote=FALSE)

# IMPLEMENTATION - Save the Prisoner

data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)
tsize <- data[1,1]

tcases <- data[-1,]
for (i in 1:tsize) {
  warn <- (tcases[i,3] - 1 + tcases[i,2] - 1) %% tcases[i,1] + 1
  write.table(warn,append=T, eol = "\n", row.names = F, col.names = F, quote=FALSE)
}

# IMPLEMENTATION - Kaprekar Numbers

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

left <- (data[1,1])
right <- (data[2,1])

range <- rep(left:right)

count <- 0

kcheck <- function(x) {
  sq <- x*x
  splitn <- as.numeric(strsplit(as.character(x),"")[[1]])
  d <- length(splitn) 
  splitsq <- as.numeric(strsplit(as.character(sq),"")[[1]]) 
  size <- length(splitsq) 
  rightpart <- as.numeric(paste(splitsq[(size-d+1):size],collapse=''))
  leftpart <- as.numeric(paste(splitsq[1:(size-d)],collapse=''))
  if ((rightpart+leftpart) == x) {
    write.table(x,eol='\t', append=T, row.names = F, col.names = F, quote=FALSE)
    count <- 1
    return(1)
  } else if (x==1) {
    write.table(1,eol="\t",append=T, row.names = F, col.names = F, quote=FALSE)
    return(1)
  } else {
    return(0)
  }
}

keep <- (sapply(range,kcheck))

if (sum(keep)==0) {
  write.table("INVALID RANGE",sep="", append=T, row.names = F, col.names = F, quote=FALSE)
} 

# STRINGS - Super Reduced String

data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)
data <- as.character(data[1,1])
word <- unlist(strsplit(data,""))
size <- length(word)
final <- word
i <- 1
while (i<(length(word))) {
  if ((i>0) && (word[i+1] == word[i])) {
    word <- word[c(i*-1,(i+1)*-1)]
    i <- i-1
  } else {
    i <- i+1
  }
}

if (length(word) == 0) {
  output <- "Empty String"
} else {
  output <- paste(word,collapse='')
}

write.table(output,sep="", append=T, row.names = F, col.names = F, quote=FALSE)

# STRINGS - Two Strings

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

data <- as.matrix(data)

pairs <- as.numeric(data[1,1])

data <- data[-1]
alph <- letters

alphcheck <- function(word1,word2) {
  alph <- letters
  for (i in 1:26) {
    if (length(which(word1==alph[i]))>0 && length(which(word2==alph[i]))>0) {
      return("YES")
      break
    } else {
      if (i == 26) {
        return("NO")
      }
    }
  }
}


i <- 1
while (i <= ((2*pairs)-1)) {
  output <- alphcheck(unlist(strsplit(data[i],"")),unlist(strsplit(data[i+1],"")))
  write.table(output,sep="",eol="\n", append=T, row.names = F, col.names = F, quote=FALSE)
  i <- i+2
}

# STRINGS - Game of Thrones

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

string <- as.character(data[1,1])
splstring <- strsplit(string,"")[[1]]
size <- length(splstring)

alpha <- letters
lettertrack <- rep(0,26)

for (i in 1:26) {
 for (j in 1:size) {
   if (splstring[j] == alpha[i]) {
     lettertrack[i] <- lettertrack[i] + 1
   }
 } 
}

even <- 0
odd <- 0

for (i in 1:26) {
  if (lettertrack[i] %% 2 != 0) {
    odd <- odd + 1
  }
}

if (odd > 1) {
  write.table("NO",sep="",eol="\n", append=T, row.names = F, col.names = F, quote=FALSE)
} else {
  write.table("YES",sep="",eol="\n", append=T, row.names = F, col.names = F, quote=FALSE)
}

# STRINGS - Palindrome Index

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

tests <- as.numeric(as.character(data[1,1]))
strings <- as.matrix(data[-1,])

palcheck <- function(word) {
  size <- length(word)
  for (i in 1:(floor(size/2))) {
    if (word[i] != word[size-i+1]) {
      if ((word[i] == word[size-i]) && (word[i+1] == word[size-i-1])) {
        return(size-i)
        break
      } else {
        return(i-1)
        break
      }
    }
  }
  return(-1)
}

for (i in 1:tests) {
  output <- palcheck(strsplit(strings[i],"")[[1]])
  write.table(output,sep="",eol="\n", append=T, row.names = F, col.names = F, quote=FALSE)
}

# SORTING  - Quicksort 1

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

pivot <- as.numeric(as.character(data[2,1]))
array <- as.numeric(as.character(data[2,]))

leftp <- 0
rightp <- 0
equalp <- 0

for (i in 1:length(array)) {
  if (array[i] < pivot) {
    leftp <- append(leftp,array[i])
  } else if (array[i] > pivot) {
    rightp <- append(rightp,array[i])
  } else {equalp <- append(equalp,array[i])}
}

leftp <- leftp[-1]
rightp <- rightp[-1]
equalp <- equalp[-1]

write.table(t(c(leftp,equalp,rightp)),sep=" ", append=T, row.names = F, col.names = F, quote=FALSE)

# SEARCH - Sherlock and Array

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)

set <- as.matrix(data)
tests <- as.numeric(set[1,1])

shercheck <- function(x) {
  size <- length(x)
  if (size == 1) {
    return("YES")
  }
  for (i in 2:(size-1)) {
    if (sum(x[1:(i-1)]) == sum(x[(i+1):size])) {
      return("YES")
    }
  }
  return("NO")
}

j <- 2
while (j<=((tests*2))) {
  length <- set[j]
  x <- set[(j+1),1:length]
  output <- shercheck(x)
  write.table(output,sep="",eol="\n", append=T, row.names = F, col.names = F, quote=FALSE)
  j <- j+2
}

# SEARCH - Missing Numbers

data <- read.table("/dev/stdin", sep="",header=FALSE,fill=TRUE)
data <- read.table("test.txt",sep="",fill=TRUE,header=FALSE)


sizea <- data[1,1]
sizeb <- data[3,1]

arraya <- as.numeric(data[2,1:sizea])
arrayb <- as.numeric(data[4,1:sizeb])

libnumber <- rep(0,10000)

for (i in 1:sizea) {
  libnumber[arraya[i]] <- libnumber[arraya[i]] - 1 
}

for (j in 1:sizeb) {
  libnumber[arrayb[j]] <- libnumber[arrayb[j]] + 1
}

for (k in 1:10000) {
  if (libnumber[k] > 0) {
    write.table(k,sep=" ",eol=" ", append=T, row.names = F, col.names = F, quote=FALSE)
  }
}