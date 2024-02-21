## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## condition the matrix to cacheSolve() so it can figure out if the inverse is already stored in 
## the matrix object (such as 'my.matrix.A' below)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

### use the entries in the named object (e.g., my.matrix.A) to determine if the inverse of 
### the matrix is already cached; and if it is, just use the cached result. If not, 
### calculate and cache the new result

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x' 
  
  ### pull the inverse from my.matrix$getinverse()
  
  inv <- x$getinverse()
  
  ### if there's a value in 'inv' from the cache (aka, it's not empty), then return inv
  if(!is.null(inv)) {
    
    ### let the user know I am retrieving cached data
    message("getting a cached matrix (this was not freshly calculated)")
    
    ### provide the already-calculated-inverse
    return(inv)
    
    ###if there is no already-calculated inverse, then calculate one and print it 
    ### (with a helpful message for someone learning this)
  } else {
    
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  message("Here's a brand new inverse")
  inv
  }
}


### some toy examples

a1 <- c(3, 2, 2) 
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4) 

A <- rbind(a1, a2, a3) 

b1 <- c(3, 2, 5) 
b2 <- c(2, 3, 2) 
b3 <- c(5, 2, 4) 

B <- rbind(b1, b2, b3) 

c1 <- c(3, 2, 5) 
c2 <- c(2, 3, 2) 
c3 <- c(5, 2, 4) 

C <- rbind(c1, c2, c3) 


my.matrix.A<-makeCacheMatrix(A)

my.matrix.B<-makeCacheMatrix(B)

my.matrix.C<-makeCacheMatrix(B)

cacheSolve(my.matrix.A)

cacheSolve(my.matrix.B)

cacheSolve(my.matrix.A)

cacheSolve(my.matrix.C)



