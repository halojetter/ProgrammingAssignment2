
## Create the capablity to cache the inverse of matrix i.e.
## Compute it for first call and store it. Return the stored
## for future calls
## Assumption: Input matrix is invertible

## Example usage:
## m <- makeCacheMatrix(matrix(rep(1:4), 2,2))
## cacheSolve(m)

## cacheSolve(m) will compute the inverse of the matrix for the first call and store it.
## For subsequent calls, it will retrive from cache and a message will be printed denoting this behavior

## makeCacheMatrix: Wrapper for matrix that can store inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  ##Set the value of the matrix and intialize inverse to NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }

  ## Get the value of the matrix
  get <- function() x

  ## Set the value of inverse
  setInverse <- function(inv) inverse <<- inv

  ##Get the value of inverse
  getInverse <- function() inverse

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()

  #Check if inverse if cached and return the cached value if it is present
  if(!is.null(inverse)){
    message("Getting cached value for inverse")
    return(inverse)
  }

  #Cached inverse does not exist. Compute the inverse and store it for future calls
  data <- x$get()
  inverse = solve(data)
  x$setInverse(inverse)
  inverse
}
