### Assignment: Caching the Inverse of a Matrix

  ## Matrix inversion is usually a costly computation and there may be some
  ## benefit to caching the inverse of a matrix rather than computing it
  ## repeatedly. 

## `makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.  
## It holds a per-instance value (cached) inverse as 'cachedInverse"
## flow would be something like
## ----console:
## z<- makeCachMatrix()   ## set a new object 'z' to this special matrix object, returning a list of the 4 functions
## z$set(matrix(c(2,-1,0,-1,2,-1,0,-1,2),nrow=3,ncol=3))  ## load a 3x3 known to have an inverse
## z2 <- z$get()  ## set 'z2' to the original matrix 
## z2  ## show the created matrix!
## z$setInverse(z2) ## set the inverse over the original matrix
## z3 <- z$getInverse() ## retrieve the generated inverse
## z3 ## show the inverse!
## ----end console-------continue below

makeCacheMatrix <- function(invertibleMtx = matrix()) {
  ## for assignment, we always expect a square matrix
  ## and to be invertible, its determinant !=0 
  cachedInverse <- NULL
  set <- function(primeMtx) { ## primeMtx is 'assumed' to be a square invertible matrix
    invertibleMtx <<- primeMtx  ## set the outer return matrix to this originating matrix
    cachedInverse <<- NULL  ## initialize the outer stored matrix inverse 
  }
  get <- function() invertibleMtx
  setInverse <- function(toInvert) cachedInverse <<- solve(toInvert)
#  setInverse <- function() cachedInverse <<- solve(invertibleMtx)
  getInverse <- function() cachedInverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}
    
## `cacheSolve`: This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.
## per instructions, we assume the matrix is 
## square, with a non-zero determinant (invertible)
## ----continuing above flow:
## after the inverse had been set, a call over z should give a message "returning cached inverse"
## ----console:
##  cacheSolve(z)
## 'returning cached inverse'
##       [,1] [,2] [,3]
##  [1,] 0.75  0.5 0.25
##  [2,] 0.50  1.0 0.50
##  [3,] 0.25  0.5 0.75
## ---end console
## now we can create another matrix, but get the non-cached inverse using cacheSolve:
## q <- makeCacheMatrix()
## q$set(matrix(c(3,-1,0,-1,3,-1,0,-1,3),nrow=3,ncol=3))
## q2 <- q$get()
## cacheSolve(q)
##            [,1]      [,2]       [,3]
## [1,] 0.38095238 0.1428571 0.04761905
## [2,] 0.14285714 0.4285714 0.14285714
## [3,] 0.04761905 0.1428571 0.38095238
## ----and try it again, we should get a cached version:
## cacheSolve(q) ## again
## "returning cached inverse"
## ....

cacheSolve <- function(invertibleMtx, ...) {
  ## Return a matrix that is the inverse of 'invertibleMtx'
  ## Computing the inverse of a square matrix can be done with the `solve`
  ## function in R. For example, if `X` is a square invertible matrix, then
  ## `solve(X)` returns its inverse.
  
  inverseCached <- invertibleMtx$getInverse()  ## use the special matrix object's getInverse
  if(!is.null(inverseCached)) {             ## if it had not yet been set it will be NULL     
    message("returning cached inverse")     ## it WAS found
    return(inverseCached)                   ## return the cached inverse
  }
  copyToInvert <- invertibleMtx$get()        ## grab a copy of the special matrix' values
  inverseCached <- solve(copyToInvert, ...) ## get the inverse   
  invertibleMtx$setInverse(inverseCached)    ## set the matrix object's cache value
  inverseCached                             ## return the inverse
}
