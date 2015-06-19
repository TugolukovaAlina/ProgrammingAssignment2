## Put comments here that give an overall description of what your
## functions do

## creates a special matrix that can store its inverse matrix 

makeCacheMatrix <- function(M = matrix()) {

     # stores inverse matrix of M if it has been computed
     inverseM <- NULL

     set <- function(y) {
          M <<- y
          # set inverse matrix to null if matrix M has been modified
          inverseM <<- NULL
     }
     get <- function() M
     setInverseMatrix <- function(invM) inverseM <<- invM
     getInverseMatrix <- function() inverseM

     list(set = set, get = get,
          setInverseMatrix = setInverseMatrix,
          getInverseMatrix = getInverseMatrix)
}


## computes the inverse of the matrix returned by `makeCacheMatrix` above. 
## if inverse matrix has already been computed returns cache

cacheSolve <- function(M) {
     
     inverseM <- M$getInverseMatrix()
     
     # check if matrix has not changed and inverse matrix has already been counted
     # then return already computed inverse matrix
     if( !is.null(inverseM)) {
          message("getting cached data")
          return(inverseM)
     }
     
     # compute inverse matrix of M
     data <- M$get()
     inverseM <- solve(data)
     M$setInverseMatrix(inverseM)
     inverseM
}
