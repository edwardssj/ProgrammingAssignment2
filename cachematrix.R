## The makeCacheMatrix and cacheSolve functions demonstrate a method by which
## the result of a calculation, in this case finding the inverse of a matrix, 
## can be stored so that the value does not have to be calculated repeatedly.

## The function receives a matrix as a parameter and returns a list object.
## The list allows the sub-functions to be accessed by name by cacheSolve. 

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The first time this function is called x$getinv() returns NULL, so the 
## inverse is calculated. The result is 'cached' using the superassignment 
## operator so that when the function is called again the calculation is 
## not repeated, instead the cached value is returned.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv

}
