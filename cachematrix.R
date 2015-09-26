## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## get returns the square matrix x stored in the main. set changes the matrix stored in the main
## setinverse stores the input value in the variable i into the main function  hwile getinverse returns the value. 

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



## Write a short comment describing this function
## cacheSolve first checks the value i in getinverse. 
## if the value is not null, it will return the cached data, 
## otherwise solve function will be invoked to commpute the inverse.

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
