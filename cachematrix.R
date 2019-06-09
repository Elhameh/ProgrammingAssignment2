## Put comments here that give an overall description of what your
## functions do

## function for making a cache of the calculated inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(inversem) invm <<- inversem
  getinv <- function() invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## caculating the inverse of the matrix if there no cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data)
  x$setinv(invm)
  invm
  
}
