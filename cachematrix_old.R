## The function 'makeCacheMatrix' does the follwing:
## 1. Check whether the input matrix is square, 
##    else message: "Not square, no inverse"
## 2. Check whether the inverse exists by calculating 
##    determinant. If det(x) is zero, then 
##    message: "Matrix is singular, no inverse"

## 3. source("/Users/tesaseverson/Desktop/R_astrid/ProgrammingAssignment2/cachematrix.R")
## 4. It returns a list with info on "set", "get", "setinv" and "getinv".


  
makeCacheMatrix <- function(x = matrix()) {
  matrixinv <- NULL
  sq <- ncol(x)==nrow(x)
  if (sq == FALSE) {
        message("The matrix is not square; no inverse.")
        } else {
        if (det(x)==0) {
              message("The determinant equals zero; no inverse.")
              } else {
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
        }
  }



## The function 'cacheSolve' does the following:

## 3. Else, get inverse from cache. If NULL, then 
## 4. ...
## 5. ...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  matrixinv <- x$getinv()
  if(!is.null(matrixinv)) {
          message("Getting cached data ...")
          return(matrixinv)
          }
##  data<-matrix(numeric(0), 0,0)
##  data <- x$getinv()
  matrixinv <- solve(x)
  x$setinv(matrixinv)
  matrixinv
  }


