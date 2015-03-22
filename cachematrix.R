## Rj Programming Assignment2 Lexical Scoping
## 22 March 2015 Excercised by Jule On Bonsai

## Put comments here that give an overall description of what your
## functions do

## Exercise to cache the inverse of a matrix
## for use in special situation 
## where the (unchanged) inverse matrix is needed several times


## makeCacheMatrix creates a special "matrix" object  
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## in work
  data <-x$get()
  m <-  solve(data,...)
  ## in work
  m 
}


## cacheSolve computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
##    (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <-  solve(data,...)
  ## in work
  m
}
