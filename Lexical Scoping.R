makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y = matrix()) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse = matrix()) inv <<- inverse
        getInverse <- function() inv
        
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (is.null(inv)) {
                matrix <- x$get()
                inv <- solve(matrix, ...)
                x$setInverse(inv)
        } else {
                message("Getting cached inverse...")
        }
        inv
}

