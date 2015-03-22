## R Programming Assignment2 Lexical Scoping
## 22 March 2015 Excercised by Jule Bonsai

## Exercise to cache the inverse of a matrix
## for use in special situation 
## where the (unchanged) inverse matrix is needed several times

## Using example:
##   m <- matrix(c(4, 3, 3, 2), 2,2)
##   x <- makeCacheMatrix(m)
##   inv <- cacheSolve(x)
##   inv


## makeCacheMatrix creates a special "matrix" object  
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(invers) inv <<- invers
  
  getinv <- function () inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve computes the inverse of the special "matrix" 
##    returned by makeCacheMatrix above. 
## If the inverse has already been calculated 
##    (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv)) {
    message("INFO: getting cached data")
    return(inv) 
  }
  
  data <- x$get()
  
  inv <- solve(data)
  
  x$setinv(inv)
  
  inv
  
}
