## This function initializes the objects "x" and "inv", which will store a
## matrix and its inverse, respectively. It then defines functions to "get" and
## "set" the value of "x" and "inv", then outputs the four functions as a
## list. makeCacheMatrix also serves as the parent environment for these 
## functions. The "set" type functions can alter the values of "x" and "inv" 
## in the parent environment with the double arrow assignment, and the "get"
## functions, by default give the values of "x" and "inv" in the parent 
## environment, since these objects are not defined in the "get" functions
## themselves.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## the cacheSolve function takes as its argument an object produced by the 
## makeCacheMatrix function, which has a matrix "x" stored in it. Firstly it
## retreives the value of the inverted matrix, if there is one, then checks if
## it's NULL. If there is a stored value, the function returns the stored value
## along with a message to let the user know the cached value was used. 
## Otherwise, it retrieved the matrix "x" from the makeCacheMatrix environment,
## calculates the inverse matrix, stores it into said environment and returns
## it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                print("Getting Cached Data")
                return(inv)
        }
        matr <- x$get()
        inv_matr <- solve(matr, ...)
        x$setinv(inv_matr)
        inv_matr
}