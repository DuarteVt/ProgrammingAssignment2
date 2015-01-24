## makeCacheMatrix takes as an argument a matrix and calculates its inverse.
## This function delivers the result as a list with the 4 parameters below
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(solve) inverseMatrix <<- solve
  getInverse <- function() inverseMatrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve checks whether this inverse has been calculated and, if not, calculates it
## and changes the parameters accordingly
cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)){
    message("Getting cached inverse matrix")
    return(inverseMatrix)
  } 
  y <- x$getMatrix()
  inverseMatrix <- solve(y, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
