## Put comments here that give an overall description of what your
## functions do

## download and install MASS package for enabling the use of solve() function
## that is used in inverse matrix calculation. Here it is assumed that the matrix
## itself is a square matrix. Simply uncomment the following lines to install and
## and check the proper installation of the package

#install.packages ("MASS")
#find.package ("MASS")

## include MASS library for solve () function usage
library (MASS)

## makeCacheMatrix function returns a list of functions that are available.
##It aims to store the matrix and the cached value of the inverse with the 
##following functions:
## 1) setMatrix: set the matrix
## 2) getMatrix: get the matrix
## 3) cacheInverse: cache the invers and return to it
## 4) getInverse: get the inverse of the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  #stores the cached value or NULL.
  #initially it is set to NULL since nothing is cached
    
    cacheInverse <- NULL
    #store the matrix
    setMatrix <- function (Inv){
        x <<- Inv
        cache <<- NULL
    }
    
    #get the stored matrix
    getMatrix <- function (){
        x
    }
    
    #cache the given stuff
    cacheInverse <- function (solve){
      cache <<- solve
    }
    
    getInverse <- function (){
      cache
    }
    #list of functions
    list (setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse) 
}
## cacheSolve function returns to the inverse of the matrix given 
## by makeCacheMatrix function above. For inverse calculation,
## solve() function is used from MASS package of R.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  #if a cached value is not null, in other words, if it exists, return that.
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  #otherwise, get the matrix
  matrix_inv <- x$getMatrix()
  #calculate matrix inverse
  I <- solve(matrix_inv)
  #cache the inverse
  x$cacheInverse(I)
  
  #return the inverse
  I
}
