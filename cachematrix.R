## Put comments here that give an overall description of what your
## functions do


## This function can cache the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  ## Initialize  inverse property
  InverseMatrix <- NULL
  
  ## Set the matrix
  set <- function(y) {
    matrix <<- y
    InverseMatrix <<- NULL
  }
  
  ## Get the matrix
  get <- function() {
    ## return matrix
    matrix
  }
  
  
  ## Set the inverse matrix
  setInverseMatrix <- function(inverse){
    
    ## storing the inverse
    InverseMatrix <<- inverse
    
  }
  
  ## Get the inverse matrix
  getInverseMatrix <- function() {
    
    ## returns the inverse
    InverseMatrix
    
  }
  
  ## Returns the list of functions used
  list(set = set, get = get, 
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## This function computes the inverse of the special matrix returned by the "makeCacheMatrix" function. 
## If the inverse has already been calculated (and the matrix has not changed), then the "cacheSolve" should retrieve the inverse from the cache.
## If the inverse has not yet been calculated, the inverse will be calculated using function solve(x)



cacheSolve <- function(x, ...) {
  
  ## getting a matrix that is the inverse of x
  InverseMatrix <- x$getInverseMatrix()
  
  ## returns if the inverse has already been calculated 
  if(!is.null(InverseMatrix)) {
    message("getting cached inverse matrix")
    return(InverseMatrix)
  }
  
  ## if the inverse matrix has not yet been calculated
  data <- x$get()
  
  
  ## calculate inverse matrix
  InverseMatrix <- solve(data) %*% data
  
  ## storing the inverse 
  x$setInverseMatrix(InverseMatrix)
  
  
  ## return inverse matrix
  InverseMatrix
}
