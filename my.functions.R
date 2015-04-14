###############
### hiyou() ###
###############
#Description: This function creates a personalize welcome message when you open R

#Inputs:
# none - relies on other functions to calculate information

#Outputs
# A welcome message to R

hiyou <- function(){
  #extracting some information
  myinfo <- Sys.info() ; rinfo <- R.Version()
  
  #making three statements
  hi <- paste("Hi",paste0(myinfo[7],"!"))
  welcome <- paste("Welcome to",paste0(rinfo$version.string,"!"))
  iam <- paste("My name is:",paste0(rinfo$nickname,"."),
               "Let's go have some fun!")
  
  #returning a cat statement
  return(cat(paste0(hi,"\n",welcome,"\n",iam)))
}
hiyou()

###################
### mysummary() ###
###################

#Description: This function calculates mean and standard deviation for a array of numeric values

#Inputs:
# x - an array of numeric values

#Outputs
# Options
# output = 1 : Returns a cat statment with mean (center) and standard deviation (spread)
# output = 2 : Returns the same information as output = 1, except in a data frame

mysummary <- function(x, output=1) {
  
  if(is.numeric(x)!=T){
    stop("x is not a vector of numeric values")
  }
  
  if(length(x)<30) {
    warning("Sample size is less than 30",call. = F)
  } 
  
  print("Calculating mean and standard deviation")
  #calculating stats of interest
  center <- mean(x) 
  spread <- sd(x)
  
  #Using output to determine how the information is
  #displayed to the user
  if (output == 1) {
    return(cat("Mean=", center, "\n", "SD=", spread, "\n"))
  } 
  
  if (output == 2) {
    df <- data.frame(statistic = c("mean","sd"),
                     value = c(center,spread))  
    return(df)
  }
}

#################
### gn_mean() ###
#################

#Description: This function calculates the geometric mean for a array of numeric values

#Inputs:
# x - an array of numeric values
# na.rm 
# Default = TRUE - removes NAs; FALSE does not

#Outputs
# Returns the geometric mean

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#############
### fib() ###
#############

#Description: This function returns the nth value in the Fibonacci sequence

#Inputs:
# n - a single numeric value

#Outputs
# Returns the nth value in the Fibonacci sequence

fib <- function(n){
  
  #base case
  if(n==1){
    return(1)
  } else if(n==2) {
    return(1)
    
    #inductive case
  } else {
    a = fib(n-1)
    b = fib(n-2)
    return(a+b)
  }
}
