## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(cachedMatrix = matrix()) {
  #set inverse matrix to null
  invMatrix <- NULL
  
  #set function for cached matrix
  set <- function(y) {
    cachedMatrix <<- y
    invMatrix <<- NULL  #reset invers of this matrix
  }
  #get function for cached matrix
  get <- function() cachedMatrix
  
  #set function for inverse of matrix
  setInvMatrix <- function(x) invMatrix <<- x
  
  #get function for inverse of matrix
  getInvMatrix <- function() invMatrix
  
  #create list to store all functions
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  #call get function for inverse matrix
  invMatrix <- x$getInvMatrix()
  
  #if cached inverse exist print message and return invMatrix
  if(!is.null(invMatrix)) {
    message("getting cached data")
    return(invMatrix)
  }
  
  #get matrix for each we calculate inverse
  data <- x$get()
  #calculate inverse
  invMatrix <- solve(data, ...)
  #store this value for feature
  x$setInvMatrix(invMatrix)
  #return invMatrix
  invMatrix
}
