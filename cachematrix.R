## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Cache the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
	i<- NULL


	set<-function(matrix){
		m<<-matrix
		i<<-NULL
	}
	get<-function() m

	setInverse<-function(inverse){
		i<<-inverse
	}
	getInverse<-function() i

	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## Write a short comment describing this function
## Compute the inverse of a matrix returned by the function above, 
##if it's already stored in memory, this function would retrieve the result
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <-x$getInverse()

	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	data<-x$get()
	
	m<-solve(data)%*%data

	x$setInverse(m)

	m
}
