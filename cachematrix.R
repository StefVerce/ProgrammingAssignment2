## ---------------------------------------------------------------------------##
## cachematrix.R                                                              ## 
## ---------------------------------------------------------------------------##
## contains 2 functions: makeCacheMatrix() + cacheSolve()                     ##
## ---------------------------------------------------------------------------##
## makeCacheMatrix builds a list containing functions used to handle matrix   ##
## cacheVector returns either the cached inverted matric or calculates the    ##
## inverted matrix and puts it into cache                                     ##
## ---------------------------------------------------------------------------##
## Data Science - R Programming - Coursera                                    ##
## Assignment 2 - Lexical Scoping                                             ##
## Nov.2014                                                                   ##
## ---------------------------------------------------------------------------##

## Given a matrix x, this function returns an object list containing functions to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. inverse the matrix
##  4. Get the inverse matrix

## 
makeCacheMatrix <- function(x = matrix()) {
 
  # set inverse matrix to NULL
  inv <- NULL
   
  # set the matrix using superassignment i.e outside environment of the function itself
  # initialize inverse matrix
  set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  
  # returns the value of x
  get <- function() x

  # sets the inverse matrix using "super assignment"
  setinv <- function(z) inv <<- z ## here s the trick: z is already the inverted matrix
  
  # returns inversed matrix
  getinv <- function() inv
  
  # special matrix list object
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
  

## 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## when the inverse matrix is already in the cache it returns it
  ## else it computes the inverse using "solve()" and push it into cache
  ## using setinv() 
  
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}