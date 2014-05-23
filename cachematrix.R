# This project is the second programming assignment 
# and first course project in the R Programming class
# completed by Glenn Dunmire. In this project we
# will be creating a matrix, and then computing its
# inverse. The inverse will be stored in a cache. 
# If the cacheSolve function is called again, 
# the function will return the inverse stored in the
# cache, rather than compute the inverse again. 
# The purpose of this is to prevent performing
# expensive computations multiple times. 

# The makeCacheMatrix function creates a special matrix
# which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ##initialize inverse matrix value  
  inv <- NULL 
    
    ##set value of matrix, note use of <<- operator
    ##is necessary, else would set the value just
    ##inside this function
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    ##return value of the matrix
    get <- function() x
    
    ##set the value of the inverse matrix
    setInv <- function(z) inv <<- z
    
    ##get the value of the inverse matrix
    getInv <- function() inv
    
    ##return a list of all the above functions
    list(set = set, get = get, setInv = setInv,
         getInv = getInv)
}


# The cacheSolve function calculates the inverse of
# a matrix. All matrices are assumed to be square and
# have an inverse. 
# First, the function checks to see if the inverse
# has already been calculated. If so, it gets the 
# inverse from the cache and returns it. 
# Else it calculates the inverse of the matrix and
# sets the inverse in the cache.

cacheSolve <- function(x, ...) {
  ##get inverse from cache (even if null)     
  sol <- x$getInv() ##sol stands for solution
       
  ##check if inverse exists. If it does
  ##return inverse
       if (!is.null(sol)) {
         message("getting cached inverse")
         return(sol)
       }
       
  ##else, get the original matrix
  ##data is the matrix
       data <- x$get() 
  
  ##compute inverse
       sol <- solve(data, ...)
  
  ##store inverse
       x$setInv(sol) 
  
  ##return inverse
       sol 
}
