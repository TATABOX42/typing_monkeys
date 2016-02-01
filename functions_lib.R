
# ***************
# ***************
# ***************
# ***************
#   FUNCTIONS
# ***************
# ***************
# ***************
# ***************

get.emission.mat <- function(l,alphabet,training.file,Markov.Chain=TRUE,pseudocount=1) {
  if(Markov.Chain==FALSE){
    m.s <-  matrix((1/l),nr=l,nc=l,dimnames=list(alphabet,alphabet))
    return(m.s)
  }else{
    # create object
    m <- matrix(pseudocount,nr=l,nc=l,dimnames=list(alphabet,alphabet))
    # load the training file
    s <- scan(training.file, character(0))
    s.split <- unlist(strsplit(s,""))
    # train the matrix 
    for (i in 1:(length(s.split)-1)) {
      m[s.split[i],s.split[i+1]] <- m[s.split[i],s.split[i+1]] + 1
    }
    # scale the matrix (rowSums == 1)
    m.s <- m/rowSums(m)
    return(m.s)
  }
}

get.p <- function(emission.mat,Markov.Chain=TRUE,target) {
  # *************************************************
  # arguments description:
  # emission.mat: emission probability matrix, rowSums == 1
  # order: order of the Markov Chain (only order 0 and order 1 supported)
  # target: target string to compute the probability
  # *************************************************
  if(Markov.Chain==TRUE){
    # split the string into character()
    target.split <-  unlist(strsplit(target,""))
    # check if the target has length == 1
    if(length(target.split)==1){
      p <- 1/nrow(emission.mat)
    # target is not a single character
    } else {
      # set the order of the chain
      order <- 1
      # if target size is > 2
      if(length(target.split)>2){
        # compute the prob of the first character
        p <- emission.mat[target.split[1],target.split[1+order]]
        # compute for the next characters
        for (i in 2:(length(target.split)-order)) {
          p <- p * emission.mat[target.split[i],target.split[i+order]]
        }
        return(p)
      } else {
        p <- emission.mat[target.split[1],target.split[1+order]]
        return(p)
      }

    }
  # Markov.Chain == FALSE
  } else{
    # set the number of characters
    l <- nrow(emission.mat)
    # split the string into character()
    target.split <-  unlist(strsplit(target,""))    
    p <- (1/l)^length(target.split)
    return(p)   
  }
}

get.n.monkeys <- function(p) {
  n <- ceiling(1/p)
  return(n) 
}

