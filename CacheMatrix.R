#Caching inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        A <- NULL
        set <- function(y) {
                x <<- y
                A <<- NULL
        }
        get <- function() x
        setinv <- function(inv) 
	  A <<- inv
        getinv <- function() A
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSol <- function(x, ...) {
       
        A <- x$getinv()
        if (!is.null(A)) {
                message("getting cached data")
                return(A)
        }
        mat <- x$get()
        A <- solve(mat, ...)
        x$setinv(A)
        A
}



# For verifying
> matrix1 <- makeCacheMatrix(matrix(1:8, 2,2))
> matrix1$getinv()
NULL
> cacheSol(matrix1)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
