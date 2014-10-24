## This file contains the following functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the cachesolve
## retrieves the inverse from the cache.


## The makeCacheMatrix function takes a matrix and returns a list of
## functions. These functions allow the user to return or change the
## input matrix, as well as return and set the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
      
      ## Initialize a blank variable called inv
      inv <- NULL
      
      ## setMatrix is a function that can be used to assign a new matrix
      ## to variable x (without having to call makeCacheMatrix)
      ## and reset the inverse matrix inv
      setMtrx <- function(newMtrx) {
            x <<- newMtrx
            inv <<- NULL
      }
      
      ## getMatrix is a function that returns the matrix x      
      getMtrx <- function() {return(x)}
      
      ## setInverse is a function that assigns the calculated inverse matrix
      ## (calculated in the cacheSolve function) to variable inv
      setInv <- function (newInv) {inv <<- newInv}
      
      ## getInverse is a function that returns the current value of 
      ## the inverse matrix inv
      getInv <- function() {return(inv)}
      
      ## Return a list of the four functions above together with corresponding
      ## identifiers (this allows the functions to be called outside of the
      ## makeCacheMatrix function environment).
      return(list(setMatrix = setMtrx, getMatrix = getMtrx,
                  setInverse = setInv, getInverse = getInv))
}


## The cacheSolve function retrieves the inverse of the input matrix
## for makeCacheMatrix from the cache, if available. If the inverse
## is not in the cache, it calculates and caches the inverse matrix.

cacheSolve <- function(x, ...) {
      
      ## Obtain the value of the inverse matrix from the cache
      inv <- x$getInverse()
      
      ## Check if the inverse has already been calculated, and
      ## if so, retrieve it from the cache together with the
      ## message "getting cached data". If not, calculate the
      ## inverse and assign it to variable inv
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      InputMatrix <- x$getMatrix()
      calcInv <- solve(InputMatrix, ...)
      x$setInverse(calcInv)
      return(calcInv)
}