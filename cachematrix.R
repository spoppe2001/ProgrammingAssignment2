##  This R script consists of two functions:
##  1.  'makeCacheMatrix', which creates and returns as a list 
##      four housekeeping functions.
##  2.  'cacheSolve, which returns the inverse of a matrix that has been
##      supplied to 'makeCacheMatrix'.
##
##  ** Important note on usage:  Invoke 'makeCacheMatrix' so it can 
##  ** return a list of functions, such as 'a <- makeCacheMatrix(b),'
##  ** where 'b' is the matrix to be inverted.

makeCacheMatrix <- function(x = matrix()) {
    ##  This function returns a list of four housekeeping functions.
    m <- NULL
    set <- function(y) {
    ##  The 'set' function makes a copy of y, the matrix to be inverted, 
    ##  in the closure environment and initializes the inverse to NULL.
        x <<- y  # Store a copy of matrix in the closure environment.
        m <<- NULL  # Initialize the inverse matrix.
    }
    get <- function() x
    ##  The 'get' function returns the matrix to be inverted.
    setinv <- function(matinv) { 
    ##  The 'setinv' function stores the supplied inverse to the cache.
        m <<- matinv
           }
    getinv <- function() {
    ##  The 'getinv' function returns the inverse, or NULL, from the cache.
              m
    }
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
  
}




cacheSolve <- function(x, ...) {
    ## Returns the inverse of a square matrix.  
    ##   If the inverse of the matrix has not been cached, 
    ##   computes the inverse and caches it.  
    ##   Otherwise retrieves the inverse from the cache and returns it.
    ##
    ##  Args:
    ##      x, a list of housekeeping functions
    ##
    ##  Returns:
    ##      m, the inverse of the matrix
    ##
    ##  Assumptions:
    ##  1.  The matrix in the argument is square and invertible.
    ##      If not, an error will be returned.
    ##  2.  The matrix to be inverted has been stored in the closure
    ##      environment.  It is retrieved by x$getinv().

     m <- x$getinv()  # Retrieve the inverse or NULL from the cache.
       if(!is.null(m)) {  #If the inverse is in the cache ... 
        message("getting cached inverse")
        return(m)  # Return the inverse.
    }  # If the cache is empty ...
    data <- x$get()  # Get the matrix to be inverted.
    m <- solve(data, ...)  #Invert the matrix
      x$setinv(m)  # Put the inverse into the cache.
      return(m)  # Return the inverse
}

