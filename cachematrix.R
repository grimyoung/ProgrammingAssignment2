##Takes in an invertible matrix 
## and creates a list with 4 functions
## which stores the value of matrix x and matrix inv
##set: sets the value of the matrix
##get: gets the value of the matrix
##setinverse: sets the matrix inv to the input matrix inverse
##getinverse: gets the matrix inv
makeCacheMatrix <- function(x = matrix()) {
  ##creates initial NULL inv
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


##Takes in the makeCacheMatrix list
## and looks to see if inv was calculated
## if so returns the inv
## else it calculates the inv, then stores it
## and returns the inv
cacheSolve <- function(x, ...) {  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}
