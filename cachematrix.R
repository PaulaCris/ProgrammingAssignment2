
###################################################

## Is expected to set up two follow functions:
#1. makeCacheMatrix -- Creates a special matrix
#2. cacheSolve -- Gets the inverse of the special matrix by using function SOLVE

#
#1. makeCacheMatrix -- Creates a special matrix
makeCacheMatrix <- function(x = matrix()) { 
z <- NULL # z is the inverse of matrix x, setting it to NULL
set <- function(y) { #set the matrix
  x <<- y
  z <<- NULL
}
get <- function() x #get the matrix
setinverse <- function(solve) z <- solve # set the inverse, solve gets the inverse
getinverse <- function() z # get the inverse
list(set = set,
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


##2. cacheSolve -- Gets the inverse of the special matrix by using function SOLVE
 cacheSolve <- function(x, ...) {
    z <- x$getinverse() #return the matrix inverse of x
    if (!is.null(z)) {
      message("getting cached data")
      return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setinverse(z)
    z
  }
  
 
 
 # Setting my matrix(x)'s values
 matrix <-  (matrix(2:5, nrow = 2, ncol = 2)) 
 #Remember that if x is a square invertible matrix, then solve(X) returns its inverse.
 
 
 
 #Testing functions
matrix_1 <- makeCacheMatrix(matrix) #creating a object

cacheSolve(matrix_1) # getting the inverse

#################################Done
