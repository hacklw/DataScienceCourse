<<<<<<< HEAD
## This is my solution of the  second programming assignment (week 3) 
## of the Coursera Course "R Programming"
## The assignment was: Caching the Inverse of a Matrix

###############################################################

## The function makeCacheMatrix creates a "matrix" object which can cache 
## its inverse.  The function returns a list of 4 funtions that set and get
## a matrix as well as set and get inverse matrix.  Call this function
## first on a matrix object
makeCacheMatrix <- function(x = matrix()) {
	x.inv <- NULL
	set <-function(y){
		x<<-y
		x.inv<<-NULL
	}
	get <- function() x
    set.inv <- function(inverse) x.inv <<- inverse
    get.inv <- function() x.inv
    list(set=set, get=get,
		 set.inv=set.inv, get.inv=get.inv)
}


## The function cacheSolve computes and returns the inverse of the matrix
## special "matrix" created by the function makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix did not change), the function  
## will retrieve the inverse from the cache (console output: "Getting cached
## data:"
## Assumtion as defined in the assignement: x is always invertible
cacheSolve <- function(x, ...) {
	x.inv <- x$get.inv()
    if(!is.null(x.inv)) {
        message("Getting cached data:")
        return(x.inv)
    }
    data <- x$get()
    x.inv <- solve(data)
    x$set.inv(x.inv)
    x.inv
        ## Return a matrix that is the inverse of 'x'
}

###############################################################
## Test the function
###############################################################
## Create "matrix"
### x = rbind(c(2,1,1), c(9,6,4), c(2,3,1)) 
### m = makeCacheMatrix(x)

## first run: empty cache>create inverse and return
### cacheSolve(m)
####[,1] [,2] [,3]
####[1,] -3.0    1 -1.0
####[2,] -0.5    0  0.5
####[3,]  7.5   -2  1.5

## second run: cache existing>get inverse from cache and return
### cacheSolve(m)
####[Getting cached data:
####[        [,1] [,2] [,3]
####[[1,] -3.0    1 -1.0
####[[2,] -0.5    0  0.5
####[[3,]  7.5   -2  1.5

## then change "matrix" and retest
### x = rbind(c(2,1,3), c(9,6,4), c(2,3,1)) 
### m = makeCacheMatrix(x)
### cacheSolve(m)
### cacheSolve(m)
=======
## This is my solution of the  second programming assignment (week 3) 
## of the Coursera Course "R Programming"
## The assignment was: Caching the Inverse of a Matrix

###############################################################

## The function makeCacheMatrix creates a "matrix" object which can cache 
## its inverse.  The function returns a list of 4 funtions that set and get
## a matrix as well as set and get inverse matrix.  Call this function
## first on a matrix object
makeCacheMatrix <- function(x = matrix()) {
	x.inv <- NULL
	set <-function(y){
		x<<-y
		x.inv<<-NULL
	}
	get <- function() x
    set.inv <- function(inverse) x.inv <<- inverse
    get.inv <- function() x.inv
    list(set=set, get=get,
		 set.inv=set.inv, get.inv=get.inv)
}


## The function cacheSolve computes and returns the inverse of the matrix
## special "matrix" created by the function makeCacheMatrix. If the inverse 
## has already been calculated (and the matrix did not change), the function  
## will retrieve the inverse from the cache (console output: "Getting cached
## data:"
## Assumtion as defined in the assignement: x is always invertible
cacheSolve <- function(x, ...) {
	x.inv <- x$get.inv()
    if(!is.null(x.inv)) {
        message("Getting cached data:")
        return(x.inv)
    }
    data <- x$get()
    x.inv <- solve(data)
    x$set.inv(x.inv)
    x.inv
        ## Return a matrix that is the inverse of 'x'
}

###############################################################
## Test the function
###############################################################
## Create "matrix"
### x = rbind(c(2,1,1), c(9,6,4), c(2,3,1)) 
### m = makeCacheMatrix(x)

## first run: empty cache>create inverse and return
### cacheSolve(m)
####[,1] [,2] [,3]
####[1,] -3.0    1 -1.0
####[2,] -0.5    0  0.5
####[3,]  7.5   -2  1.5

## second run: cache existing>get inverse from cache and return
### cacheSolve(m)
####[Getting cached data:
####[        [,1] [,2] [,3]
####[[1,] -3.0    1 -1.0
####[[2,] -0.5    0  0.5
####[[3,]  7.5   -2  1.5

## then change "matrix" and retest
### x = rbind(c(2,1,3), c(9,6,4), c(2,3,1)) 
### m = makeCacheMatrix(x)
### cacheSolve(m)
### cacheSolve(m)
>>>>>>> b6d4d1578660681a17363bfcec14954344db3c7d
