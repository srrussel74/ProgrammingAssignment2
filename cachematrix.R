###########################################################################################
# The functions create an object "special" Matrix able to ontaining cached inverse of the matrix, 
# after it is caclulated. This with help of functions below
# 

####
# makeCacheMatrix() with a (invertable) matrix 'x' as argument returns
# "special" matrix, containing matrix variable x, list of (4) functions,
# and variable m, as a cache for value inverse of matrix.
# The function  get() is used to get a value of matrix 'x'
# from "special" matrix. 
# With the last two functions, the value of the inverse of 'x' can
# be cached (setsolve()) and be obtained (getsolve()). 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
		
	get <-function() x

	setsolve <- function(inverse) m <<- inverse 
	getsolve <-function() m

	list(
	     set=set, 
	     get=get, 
	     setsolve=setsolve,
	     getsolve=getsolve
	    )
}

####
# The function cacheSolve() returns matrix that is the inverse  
# of matrix (z$get()) from the "special" matrix which is the function argument here.
# The function checks if a value of the inverse is cached or not, 
# to save a second computation loop.
# The function makes use of functions from list of "special" matrix 'z' ,
# except the function set().
# Be aware, there is no suitability test for a matrix its invertibly,
# So apply it to a square matrix with non-zero determinant.

cacheSolve <- function(z, ...) {
	m <- z$getsolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data<-z$get()
	m<-solve(data,...)
	z$setsolve(m)
	m
}
