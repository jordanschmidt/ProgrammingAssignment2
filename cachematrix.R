## These functions solve for the inverse of a matrix and cache the result. The cacheSolve
## function first checks if a solution exists in the cache before calculating a solution thus
## eliminating the need for recalculation.

## The makeCacheMatrix generates a list of 4 functions that set the value of the matrix,
## get the value of the matrix, set the value of the inverse and get the value of the inverse.

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


## This function returns the inverse of a matrix. The function first checks if an inverse
## has previously been calculated, if it has then the cached value is returned. If the inverse has
## not previously been calculated then it is calculated and cached.

cacheSolve <- function(x, ...) {
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
