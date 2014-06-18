## These two functions will be used to create a special matrix object and a 	
## method to calculate its inverse that checks if the inverse has been 
## calculated before, so it doesn't has to re-do it.

## IMPORTANT: These functions doesn't check if the matrix submitted is invertible
## so it asumes that it is always invertible.

## makeCacheMatrix creates a special matrix object with 4 functions
## so it can set, get the matrix, and set and get the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

## cacheSolve uses the special matrix created by makeCacheMatrix as argument
## and checks if there is a value for its inverse in its environment
## if it does, it returns "getting cached inverse matrix" and returns the value
## if it doesn't, it computes it

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
            message("getting cached inverse matrix")
            return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
