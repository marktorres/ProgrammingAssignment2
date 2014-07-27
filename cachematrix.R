## This file contains two functions that will cache the inverse of a matrix. 
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.



## The following function, makeCacheMatrix creates a special "matrix" object
## that can cache its inverse and a vector that contains a list which will: 
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix


makeCacheMatrix <- function(max = matrix()) {

     invmax <- NULL

     ## Define the four functions to maintain the cache
     setmax <- function(newmax) {
          ## Save the matrix into the cache
          max <<- newmax
          invmax <<- NULL
     }
     getmax <- function() {max}
     ## Save the inverse of the matrix into the cache
     setinvmax <- function(newinvmax) {invmax <<- newinvmax}
     getinvmax <- function() {invmax}

     ## Define the list containing the four functions
     list(setmax = setmax, 
          getmax = getmax,
          setinvmax = setinvmax,
          getinvmax = getinvmax)
}


## The second function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then the
## `cachesolve` will retrieve the inverse from the cache and skips 
## the computation of the inverse matrix.  Otherwise, it calculates 
## the inverse of the matrix and sets the value of the cache via the 
## `setinvmax` function.


cacheSolve <- function(max, ...) {
     ## Retrive the matix and inverse matrix from the cache
     curmax <- max$getmax()
     invmax <- max$getinvmax()

     ## Check to see if the matrix and matrix in the cache are identical     
     if(identical(curmax, max)) {
          message("getting cached inverse matrix")
          return(invmax)
     }
     ## The matrix and matrix in the cache are not identical
     message("calculating inverse matrix")
     invmax <- solve (max, ...)
     max$setinvmax(invmax)

     invmax
}