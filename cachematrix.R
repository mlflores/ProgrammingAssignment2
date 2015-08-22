## Function that cache values of a matrix and its inverse to  

## Function that creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
		invmx <- NULL
		setmx = function(y) {
			x <<- y
			invmx <<- NULL
		}
		getmx <- function() x
		setinv <- function (inverse) invmx <<- inverse
		getinv <- function() invmx
		list(set=setmx, get=getmx, setinv=setinv, getinv=getinv)
}

## Makes inverse of the original matrix created in makeCacheMatrix()

cacheSolve <- function(x, ...) {
		invmx = x$getinv()
        if (!is.null(invmx)){
                message("getting cached data")
                return(invmx)
        }
        mx.data = x$get()
        invmx = solve(mx.data, ...)
        x$setinv(invmx)
        return(invmx)	
}