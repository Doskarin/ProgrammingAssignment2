## following functions enable special object in order to store matrix and cache its inverse form

## makeCacheMatrix function creates matrix to: set value of matrix, get it, then set value of inverse
## and finally get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve function provides inverse of matrix generated from makeCacheMatrix function
## In case inverse matrix already exists cacheSolve gets it from cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
