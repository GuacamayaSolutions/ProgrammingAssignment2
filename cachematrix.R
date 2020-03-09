##`makeCacheMatrix`: This function creates a special "matrix" object
##that can cache its inverse. Steps include:
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

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


##cacheSolve: This function computes the inverse of the special
##matrix returned by `makeCacheMatrix` above. If the inverse has
##already been calculated (and the matrix has not changed), then
##cacheSolve should retrieve the inverse from the cache.


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