## caching the inverse of a matrix

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse. It is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of a matrix
## 4. get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y=matrix()) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) im <<- solve
  getinversematrix <- function() im
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
##However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
##the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of 
##the inverse in the cache via the setinversematrix function.

cacheSolve <- function(x, ...) {
  im <- x$getinversematrix()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinversematrix(im)
  ## Return a matrix that is the inverse of 'x'
  im
}
