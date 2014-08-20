############################################################
## Coursera -- R Programming 
## Program Assignment 2: Lexical Scoping
##
## Programmed by Wonoh Kim  08/20/2014
## wonoh.kim@flukenetworks.com
############################################################


############################################################
## Assignment: Caching the Inverse of a Matrix
############################################################


############################################################
## makeCacheMatrix(x = matrix()) 
## Creates a special "matrix" object that can cache its inverse
## A list contains
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
############################################################
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  
  
}

############################################################
## cacheSolve(x, ...)
## Computes the inverse of the "matrix" returned by makeCacheMatrix()
############################################################
cacheSolve <- function(x, ...) {
  
  ## first, check if the inverse has already been calculated 
  ## (and the matrix has not changed) 
  ## if so, gets the inverse from the cache and skips the computation
  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Warning: Getting cached data.")
    return(m)
  }
  
  ## then, retrieve the inverse from the cache
  ## calculates the mean and sets the value of the mean 
  ## in the cache via the setmean function  
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
  
  ## Return a matrix that is the inverse of 'x'
}


############################################################
## Example: Caching the Mean of a Vector
## <<- operator assigns a value to an object in an environment
## that is different from the current environment
############################################################


############################################################
## makeVector(x = numeric())
## Creates a special "vector"
## A list contains
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
############################################################
makeVector <- function(x = numeric()) {
  
  m1 <- NULL
  
  set <- function(y) {
    x <<- y
    m1 <<- NULL
  }
  
  get <- function() x
  
  setmean <- function(mean) m1 <<- mean
  getmean <- function() m1
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

############################################################
## cachemean(x, ...)
## Calculates the mean of "vector" defined by makeVector()
############################################################
cachemean <- function(x, ...) {
  
  ## first checks if the mean has already been calculated
  ## if so, gets the mean from the cache and skips the computation
  
  m1 <- x$getmean()
  if(!is.null(m1)) {
    message("getting cached data")
    return(m1)
  }
  
  ## calculates the mean and sets the value of the mean 
  ## in the cache via the setmean function  
  
  data <- x$get()
  m1 <- mean(data, ...)
  x$setmean(m1)
  m1
}

