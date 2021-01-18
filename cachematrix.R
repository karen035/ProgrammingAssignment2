## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Note: the 1st function should
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		set_inverse <- function(inv_input) inv <<- inv_input
		get_inverse <- function() inv
		
		list(set = set, get = get,
			 set_inverse = set_inverse,
			 get_inverse = get_inverse)

}


## Write a short comment describing this function
##
## 1. creates a special 'matrix' object that can cache its inverse.
## 2. cacheSolve: computes the inverse of the special matrix
## returned by the function above. 

cacheSolve <- function(x, ...) {
		inv <- x$get_inverse()
		if(!is.null(inv)) {
			message("getting cached inverse")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$set_inverse(inv)
		inv
       
}
