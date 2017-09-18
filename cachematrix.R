## Programming Assignment 2: Lexical Scoping
## Coursera Data Science
## by David del Moral based on Coursera Examples makeVector and cachemean functions
## September 18, 2017
##
## Usage example:
## x<-c(1:20)
## v<-makeVector(x)
## m<-cachemean(v)
##
## x<-matrix(rnorm(16),4,4)
## m<-makeCacheMatrix(x)
## s<-cacheSolve(m)
##
## makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
##


makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(	set = set, 
        get = get,
        setmean = setmean,
        getmean = getmean)
}

## cachemean:  calculates the mean of the special "vector" created with the makeVector function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets 
## the mean from the cache and skips the computation. Otherwise, it calculates the mean of the
## data and sets the value of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cache data")
    return(m)
  }
  data<- x$get()
  m<-mean(data, ...)
  x$setmean(m)
  m
}

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(	set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting matrix from cache")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data, ...)
  x$setInverse(inv)
  inv    
}
