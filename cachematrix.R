## The first function makes a special matrix, the second function then
## solves for the inverse of that matrix. If a matrix which has been
## created by the first function is run a second time by the second 
## function, the second function "remembers" the inverse of that matrix
## and returns the inverse without having to do additional calculaitons.


## This funciton creates a special "matrix", or rather a list of funcitons 
## which; 1. sets the values of the matrix, 2. gets the values, 3. sets an
## inverse, and 4. gets the inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
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

## This function calculates the inverse of the matrix from the function
## above. Note that the matrix from the function above must be non-singular
## for this function to work. If the function below already has been used to 
## calculate the inverse of a matrix from the function above, it remembers
## the value and just reuses the value it calculated the first time.

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
