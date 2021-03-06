# ***********************************
# typing monkeys
# Author: Benjamin Tovar
# Date: August 15, 2014
#
# post: http://tata-box-blog.blogspot.mx/2014/09/if-typing-monkeys-have-met-mr-markov.html
# ***********************************

# ******************************
# load functions
# ******************************

source("functions_lib.R")

# ******************************
# simulate and experiment
# where we left a single
# monkey to type n times
# ****************************** 

# set the alphabet
alphabet <- letters
# > alphabet
#  [1] "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s"
# [20] "t" "u" "v" "w" "x" "y" "z" 

# set the length of the alphabet
l <- length(alphabet)
# compute the probability to type one character randomly
char.prob <- 1/l # char.prob <- 1/26
# expand this value as a vector
uniform.prob <- rep(char.prob,l)
# number of characters to type
n <- 10000
v <- sample(alphabet,n,rep=TRUE,prob=uniform.prob)
# compute the expected number of character frequency
expected.char.freq <- floor(n*char.prob)
# compute the frequency table
v.t <- data.frame(table(v))

# ******************************
# plot the frequency of simulated 
# characters using the uniform model
# ****************************** 

library(ggplot2)

ggplot(v.t) +
	aes(x=v,y=Freq) +
	geom_bar(stat="identity",fill="darkgreen",alpha=0.4) + 
	geom_hline(yintercept = expected.char.freq ,colour="red", 
				linetype = "longdash") + 
	labs(title=paste("Computed number of character frequency after",n,"events",sep=" "),
		x="character",y="frequency")  +
	geom_text(data = data.frame(), aes(20, 450, 
			 label = paste("expected frequency per character =",expected.char.freq,"hits",sep=" ")), 
			 col="red" )


# ******************************
# compare the uniform method vs
# a Markov Chain of order 1 model
# (the M.C model is trained using the first three chapters
# of Dracula)
# ****************************** 

# set the training file path
training.file <- "dracula_chapters_1_to_3_parsed.txt"
# set the target strings
targets <- c("a","by","the","why","what","dracula","where","linux","omglolbbq")
# compute the emission matrices 
# remember that this matrix is only important when the Markov.Chain parameter == TRUE
m.markov.chain <- get.emission.mat(l,alphabet,training.file,Markov.Chain=TRUE,pseudocount=1)
m.uniform <- get.emission.mat(l,alphabet,training.file,Markov.Chain=FALSE,pseudocount=1)
# compute the probability of typing the targets
p.markov.chain <- sapply(targets, function(target) get.p(m.markov.chain,TRUE,target))
p.uniform <- sapply(targets, function(target) get.p(m.uniform,FALSE,target))
# based on the probability values computed, now calculate
# the number of monkeys needed to have at least one successful event.
n.markov.chain<- sapply(p.markov.chain, function(p) get.n.monkeys(p))
n.uniform <- sapply(p.uniform, function(p) get.n.monkeys(p))

# *********************
# plot the relation of n of monkeys based
# on the model
# *********************

k <- length(targets)

# compute the log of base 10 of the number of monkeys
# because the numbers are big big big
n.markov.chain.log <- log(n.markov.chain,10)
n.uniform.log <- log(n.uniform,10)

# plot and sing while science works
plot(n.markov.chain.log,n.uniform.log,
	ylim=c(0,16),xlim=c(0,16),pch=1:k,lwd=3,col=1:k,
	main="Number of monkeys used by the model (log10(x))",xlab="log10(n.markov.chain)",ylab="log10(n.uniform)")
grid()
abline(0,1,col="blue",lty=2)
text(x=(n.markov.chain.log+0),y=(n.uniform.log+0.5),targets,col="red",cex=1.5)

# ******************************
# PLOT THE MODELS USING A NETWORK 
# REPRESENTATION
# ****************************** 
# load library
library(qgraph)
# plot
layout(matrix(1:2,nr=1,nc=2))
	qgraph(m.markov.chain,edge.color="#4d7358")
		title("A) Emission matrix (Markov.Chain model)", line = 2.5)
	qgraph(m.uniform,edge.color ="#d64d4d")
		title("B) Emission matrix (uniform model)", line = 2.5)
