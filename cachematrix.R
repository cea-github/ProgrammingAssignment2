## The function makeCacheMatrix() constructs a custom "cache matrix". The function
## "cacheSolve()" operates on a "cache matrix", returning its inverse but only
## performing the calculations when necessary. Instead, the value of the inverse
## is stored in the "cache matrix" object.

## The values can be set at initialization as "makeCackeMatrix(x)" identical to
## a standard matrix "x" or with set(). The values, i.e. a matrix, can be retrieved
## with get(). The inverse is set and retrieved with setSolve and getSolve().
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	get <- function(){
		x
	}
	
	setSolve <- function(inverse){
		inv <<- inverse
	}
	
	getSolve <- function(){
		inv
	}
	
	list(set=set,get=get,setSolve=setSolve,getSolve=getSolve)
}


## If we have previously found the inverse of the "cache matrix" we retrieve the
## value from getSolve() without calculations. Otherwise, we calculate the
## function and set the inverse in the "cache matrix"
cacheSolve <- function(x, ...) {
        inv <- x$getSolve()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
	inv <- solve(x$get())
	x$setSolve(inv)
	inv
}
