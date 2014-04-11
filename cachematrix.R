## These two functions are used to calculate the inverse of matrices
## in an eficient manner. The first time the inverse is computed, it is
## stored in the cache. Further calls to the inverse function use this
## computed inverse instead of computing it again.

## This function is used to store/get the original matrix or to store/get
## the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the stored inverse matrix if previously calculated
## or it calculates and stores the inverse if no inverse in the cache is available.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}