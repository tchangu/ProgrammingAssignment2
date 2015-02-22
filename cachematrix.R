## Put comments here that give an overall description of what your
## functions do

## Returns a special "vector", essentially a list of function that provides
## the ability to (1) Set the value of the matrix
## (2) Get the value of the matrix
## (3) Set the value of the Inverse Matrix
## (4) Get the value of the Inverse Matrix
makeCacheMatrix <- function(matrix = matrix()) {
  ## Initialize the inverse matrix to null
  inverseMatrix <- NULL
  
  set <- function(y) {
    matrix <<- y
    inverseMatrix <<- NULL  ## Reset the inverse to null
  }
  
  get <- function() matrix
  
  setInverseMatrix <- function(inverseMatrixIn) inverseMatrix <<- inverseMatrixIn
  
  getInverseMatrix <- function() inverseMatrix
  
  # Return a list of functions
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
    
}


## This function calculates the inverse of the matrix using the 
## special matrix created using the above function.
## This functions first checks if the inverse is already calculated.
## If the inverse, which is an expensive operation, is already calculated
## then it skips the computation and returns the inverse from cache. If 
## it is not already calculated, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache using the setInverseMatrix
## function

cacheSolve <- function(x, ...) {
       
  ## Get the Inverse from cache. If it is not null, then return the cache
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  
  ## If the inverse is null then, get the matrix and perform the inverse 
  ## operation
  matrix <- x$get()
  inverseMatrix <- solve(matrix)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
