## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(m = matrix()) {

## Setting the inverse attribute 
inv <- NULL

## Set matrix
set <- function(x) {
m <<- x
inv <<- NULL
}
## Get matrix
get <- function() m

##Set inverse of the matrix 
setInverse <- function(inverse) inv <<- inverse

##Get the inverse of the matrix getInverse
getInverse <- function() inv

##Create a list of the methods
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...) {
        
## Return a matrix that is the inverse of 'm'
        
inv <- m$getInverse()
if (!is.null(inv)) {
message("getting cached data")
return(inv)
}

## Get the matrix from the object 
mat <- m$get()
inv <- solve(mat, ...)

##Set the invers to the object
m$setInverse(inv)

inv
}