## The function 'makeCacheMatrix' does the follwing:
## 1. It takes a matrix as input (it assumes a square 
##    matrix (nrow(x) = ncol(x)) that is not 
##    singular (determinant det(x) non-zero) )
## 2. It returns a list with the functions: 
##    "set", "get", "setinv" and "getinv".


  
makeCacheMatrix <- function(x = matrix()) {
  matrixinv <- NULL
  set <- function(y) {
          x <<- y
          matrixinv <<- NULL
          }
  get <- function() x
  setinv <- function(inv) matrixinv <<- inv
  getinv <- function() matrixinv
  list(set = set, get = get,
           setinv = setinv, getinv = getinv)
              }




## The function 'cacheSolve' does the following:
## 1. It takes a matrix as input
## 2. It puts the result of the function getinv() into the variable 'matrixinv'
## 3. If this result is NOT equal to NULL, then the result is returned while
##    the message "Getting cached data ..." is shown
## 4. The inverse of the matrix is calulated, and the result cached using,
##    from the list, the function setinv().



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  matrixinv <- x$getinv()
  if(!is.null(matrixinv)) {
          message("Getting cached data ...")
          return(matrixinv)
          }
  data <- x$getinv()
  matrixinv <- solve(x)
  x$setinv(matrixinv)
  matrixinv
  }


