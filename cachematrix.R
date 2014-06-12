## Put comments here that give an overall description of what your
## functions allow to calculate the inverse of a matrix as well as store it in and read it from the cache
## even if required multiple time, the time consuming calculation has to be done only once


## makeCacheMatrix checks, if a matrix (can but dont have to be the inverse) 
##      is already available in cache and if so returns it
##      the function as well allows to store a matrix in cache (the global environment)

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
  
}


## cacheSolve computes the inverse of the matrix object, returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## cacheSolve retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinverse()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
 
}
