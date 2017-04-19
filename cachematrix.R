## MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL					## sets the value of inv to NULL
  set <- function(y) {				## sets the value of the matrix
    x <<- y						## caches the inputted matrix so that cacheSolve can check whether it has changed
    inv <<- NULL					## sets the value of inv to NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) ## calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...){
	inv<- x$getinverse ()  			## Return a matrix that is the inverse of 'x'
	if(!is.null(inv)){			 
	   message("Getting cached data")
	   return(inv)		
}
	mat <-x$get()
	inv <-solve(mat, ...)
	x$setinverse(inv)
	inv
}
