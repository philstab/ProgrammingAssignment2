## makeCacheMatrix() creates an object for input into cacheSolve(). The 
## makeCacheMatrix holds the original matrix argument, the inverse of
## that matrix - if computed by cacheSolve(), and functions to mutate or 
## access those matrix objects. Those 4 functions are returned as a list

## cacheSolve() computes the inverse of the matrix passed to makeCacheMatrix, 
## and then accessing the functions in makeCacheMatrix(),
##  stores that inverse for fast retrieval later.


## takes input of a matrix, defines 4 functions for accessing or changing the 
## matrix. Stores the inverse of the matrix after cacheSolve() is called.
makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          setX<- function(passMatrix){
                      x <<- passMatrix
                      inverse <<- NULL
          }
          getX <- function() x
          setInverse <- function(inverseObj) inverse <<- inverseObj
          getInverse <- function()inverse
          
          list(setX = setX,
               getX = getX,
               setInverse = setInverse,
               getInverse = getInverse
               )
}


## cacheSolve takes MCM object as the argument, checks to see if the inverse is 
## stored in the makeCacheMatrix environment - if so, returns the inverse. If
## no inverse found, it computes the inverse and stores in makeCacheMatrix obj
cacheSolve <- function(MCM, ...) {
        inverse <- MCM$getInverse()
        if (!is.null(inverse)) {
             message("retrieving cached inverted matrix")
             return(inverse)
        }
        passThru <- MCM$getX()
        inverse <- solve(passThru)
        MCM$setInverse(inverse)
        return(inverse)
}

#test code
tm <- makeCacheMatrix(matrix(c(2,2,3,2), nrow = 2, ncol = 2))
cacheSolve(tm)

tm$setX(matrix(1:4, nrow=2, ncol=2))
tm$getX()
