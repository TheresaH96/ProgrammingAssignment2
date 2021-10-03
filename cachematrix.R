## The following two functions calculate the inverse of a matrix and 
## cache the value. Before calulating the inverse again, it checks to
## see if the result is already saved in cache; if it is, this value 
## is returned; if it isn't, the inverse is calculated.

## The first function creates a matrix object that is a list of 4 functions:
## a). set the matrix
## b). get the matrix
## c). set the inverse of the matrix
## d). get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function calculates the inverse of the matrix but first checks
## if the inverse matrix already exists in cache. If it does it returns the 
## cached value, otherwise the inverse matrix is calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
