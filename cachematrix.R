# This file is done as required for the R Peer Assesments

# required:
## Put comments here that give an overall description of what your
## functions do ...

#   General description about the overall
#
#   Matrix inversion is usually a costly computation and their may be some benefit
#to caching the inverse of a matrix rather than compute it repeatedly
# 
#   This file has a pair of functions that cache the inverse of a matrix.
#
#   1. makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse.
#
#   2. cacheSolve: 
#   This function computes the inverse of the special "matrix" returned by 
#the makeCacheMatrix above. 
#   
#   If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache, and that's all.
#
#   NOTE: Computing the inverse of a square matrix can be done via solve() in R. 
#For example, if X is a square invertible matrix, then solve(X) gives its inverse.
#So, we are going to use it, as it in the second function.

# required:
## Write a short comment describing this function:

#   Description about the first function makeCacheMatrix
#
#The first function, makeCacheMatrix creates a special "matrix", here is the 
#list of what the function do:
#   1. set the value of the matrix
#   2. get the value of the matrix id already is in cache
#   3. set the inverse of the matrix
#   4. get the inverse of the matrix if it was already in cached and the matrix
#      has not change
#

makeCacheMatrix <- function(x = matrix()) {
  
  myinv <- NULL
  set <- function(y) {
    x <<- y
    myinv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) myinv <<- inverse
  getinverse <- function() myinv
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

m <- x$getinverse()
if(!is.null(m)) {
  message("getting cached data")
  return(myinv)
}
data <- x$get()
myinv <- solve(data, ...)
x$setinverse(myinv)
myinv

} # end of function makeCacheMatrix


# required:
## Write a short comment describing this function

#   Description about the second function cacheSolve
#
#The following function calculates the inverse of the special "matrix" created 
#with the above function. However, it first checks to see if the inverse has 
#already been calculated. 
#If so, 
#it gets the inverse from the cache and skips the computation. 
#Otherwise, 
#it calculates the mean of the data and sets the value of the mean in the cache 
#via the setinverse function.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  myinv <- x$getinverse()
  if(!is.null(myinv)) {
    message("getting cached data")
    return(myinv)
  }
  data <- x$get()
  myinv <- solve(data, ...)
  x$setinvers(myinv)
  myinv
  
}  # end of function cacheSolve
