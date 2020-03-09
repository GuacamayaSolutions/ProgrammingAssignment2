makeCacheMatrix <- function(x = matrix()) {
        z_ <- NULL
        set <- function(y) {
                x_ <<- y
                z_ <<- NULL
        }
        get <- function() x_
        setInverse <- function(inverse_) z_ <<- inverse_
        getInverse <- function() z_
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
        z_ <- x$getInverse()
        if(!is.null(z_)) {
                message("getting inversed matrix")
                return(z_)
        }
        data <- x$get()
        z_ <- solve(data, ...)
        x$setInverse(z_)
        z_
}