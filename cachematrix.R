## makeCacheMatrix is a function that retuns a list of functions 
## it is used to store a martix and a cached value of the inverse
## of the matrix. It contains-
## setMatrix set the value of a matrix
## getMatix  get the value of a matrix
## cacheInverse get the cached value
## getInverse get the cached value



makeCacheMatrix <- function(x = matrix()) {
	cache<-NULL
	setMatrix <-function(newValue){
		x<<-newValue
		cache<<-NULL
	}
	getMatrix<-function(){
		x
	}
	cacheInverse <-function(solve){
		cache<<-solve
	}
	getInverse <-function(){
		cache
	}
	list(setMatrix =setMatrix, getMatrix=getMatrix, cacheInverse=cacheinverse, getInverse=getInverse)
}




## the cacheSolve part cacluates the inverse of the special "matrix"
## it will check to see if the inverse has already been calculated
## if yes, it will get the inverse from the cache and skip the calculation
## if no, it will calculate the inverse of the matrix and set the value of it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$getinverse()
        if(!is.null(inverse)){
        	message("getting cached data")
        	return(inverse)
        }
        data<-x$getMatrix()
        inverse <-solve(data)
        x$cacheInverse(inverse)
        inverse
}
