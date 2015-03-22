## An R function that caches the inverse of a matrix 
## to save time in computations

## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to set the value of the 
## matrix ,get the value of the matrix, set the value of the inverse
## of the matrix, and get the value of the inverse of the matrix
## one has to create a non-singular square matrix first
## Example: y<-matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow=3, ncol=3) and 
## have it as the argument of this function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
               x <<- y
               m <<- NULL
        }
        get <- function() x
        setinv <- function(m_inverse) m <<- m_inverse
        getinv <- function() m
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)

}


## After executing the makeCacheMatrix() function with the input y 
## matrix, one feeds the list output of the function into the 
## cacheSolve() function.  If the inverse of that matrix is already 
## computed then that value is returned, else it will compute the 
## inverse and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()

        m<-solve(data, ...)

        x$setinv(m)

        m

}
