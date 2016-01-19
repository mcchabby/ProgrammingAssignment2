## Put comments here that give an overall description of what your
## functions do

## This function creates a "special" matrix object that can cache its inverse.
## 1. Set the value of the matrix. set()
## 2. Get the value of the matrix. get()
##??3. Set the value of the inverse. setInverse()
## 4. Get the value of the inverse. getInverse()

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) i <<- inverse
	getInverse <- function() i
	list (set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, and the matrix hasn't changed, then cache solve
## should retrieve the inverse from cache.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
		i <- x$getInverse()
		if (!is.null(i)) {
			message("Getting cached data.")
			return(i)
		}
		data <- x$get()
		## To get the inverse of a matrix, use solve().
		i <- solve(data)
		x$setInverse(i)
		i
}

## Sample execution:
## > m <- makeCacheMatrix(rbind(c(1, 0), c(0, 1)))
## > m$get()
## 		[,1] [,2]
## [1,]   -1    2
## [2,]   -2    1
## > cacheSolve(m)
##            [,1]       [,2]
## [1,] 0.3333333 -0.6666667
## [2,] 0.6666667 -0.3333333
## > cacheSolve(m)
## Getting cached data.
##            [,1]       [,2]
## [1,] 0.3333333 -0.6666667
## [2,] 0.6666667 -0.3333333