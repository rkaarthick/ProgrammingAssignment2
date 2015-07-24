## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        setMatrix <- function(y) {
                x <<- y
                im <<- NULL
        }
        getMatrix <- function() x
        setInverseMatrix <- function(solve) im <<- solve
        getInverseMatrix <- function() im
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        im <- x$getInverseMatrix()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$getMatrix()
	  
	  tryCatch({
    		im <- solve(data, ...)
		x$setInverseMatrix(im)
        	return(im)
	  }, error = function(e) {
    		message("Given input is not a square invertible matrix")		
	  })
}
