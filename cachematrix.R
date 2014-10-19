# Copyright coursera10062014 2014.  All Rights Reserved.
#
# CacheMatrix provides a matrix wrapper that can efficiently repeatedly
# compute the inverse by caching the answer.
#
# Example Usage:
#
# m <- makeCacheMatrix(matrix(c(1, 2, 3, 4), ncol=2, nrow=2))
# inverse <- cacheSolve(m)
# # future calls to cacheSolve are O(1).

## makeCacheMatrix wraps matrix x to enable efficient repeated calls
## to cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
  m <- x
  inverse <- NULL  
  cacheHits <- -1  # for unit testing.  -1 = never fetched, 0 = fetched once.
  
  ## set replaces the current matrix and clears the cache and testing counters.
  set <- function(replacement) {
    m <<- replacement
    inverse <<- NULL
    cacheHits <<- -1
  }
  
  ## get returns the stored matrix.
  get <- function() {
    m
  }
  
  ## setinverse stores the computed inverse.
  setinverse <- function(computedInverse) {
    if (!is.null(inverse)) {
      stop("inverse already stored.")
    }
    cacheHits <<- cacheHits + 1
    inverse <<- computedInverse
  }
  
  ## getinverse retrieves the cached inverse.
  getinverse <- function() {
    if (is.null(inverse)) {
      NULL
    } else {
      cacheHits <<- cacheHits + 1
      inverse
    }
  }
  
  hits <- function() cacheHits  # for unit testing
  
  # The examples provides a set method that can be used to change the
  # (in the example case) vector that has its mean being computed.
  # However, the problem specification for the assignment doesn't specify
  # any set method.  Thus one is not provided here.
  list(matrix=x, setinverse=setinverse, getinverse=getinverse, hits=hits,
      set=set, get=get)
}

## cacheSolve takes a a CacheMatrix previously created using makeCacheMatrrix.
## It will return the inverse matrix.  Note that it returns a matrix, not
## a CacheMatrix.  Subsequent calls to cacheSolve for the same x will return
## a cached value.

cacheSolve <- function(x, ...) {
  # The example in the assignment uses environment caching to carry around the
  # cache.  That's not necessary.  We can store it in the CacheMatrix itself.
  if (class(x) != "list") {
    stop("cacheSolve only accepts arguments created by makeCacheMatrix().")
  }
  i <- x$getinverse()
  if (is.null(i)) {    
    i <- solve(x$get())
    x$setinverse(i)
  }
  i
}

## assertMatrixEqual will stop() if got and want do not match

assertMatrixEqual <- function(what, got, want) {
  if (!is.matrix(got)) {
    stop(paste(what, " got a ", class(want), ", want a matrix"))
  }
  assertEqual(paste(what, " dimensions"), dim(got), dim(want))
  assertEqual(paste(what, " values"), as.vector(got), as.vector(want))
}

## assertEqual will stop() if got and want do not match.

assertEqual <- function(what, got, want) {
  if (is.matrix(want)) {
    assertMatrixEqual(what, got, want)
  } else {
    delta = all.equal(got, want)
    if (!isTRUE(delta)) {
      stop(paste(what, ": got ", got, ", want ", want, " delta ", delta), "\n")
    }
  }
}

## testCacheMatrix is a unit test for the other functions in this file.

testCacheMatrix <- function() {
  m <- matrix(c(2, 1, 2, 2, 4, 2, 2, 2, 1), 3, 3, byrow=TRUE)
  
  m2 <- makeCacheMatrix(m)
  assertEqual("initial hits", m2$hits(), -1)
  
  cacheSolve(m2)
  assertEqual("first computation hits", m2$hits(), 0)

  cacheSolve(m2)
  assertEqual("first cache hit", m2$hits(), 1)
    
  cacheSolve(m2)
  assertEqual("second cache hit", m2$hits(), 2)
  
  m <- matrix(c(1,2,3,4), nrow=2, ncol=2)
  m2$set(m)
  assertEqual("after set, hits", m2$hits(), -1)

  inverse <- cacheSolve(m2)
  assertEqual("cacheSolve", inverse, 
              matrix(c(-2, 1, 1.5, -0.5), nrow=2, ncol=2))
  
  reverse <- cacheSolve(makeCacheMatrix(inverse))
  assertEqual("reverse", reverse, m)

  print("testCacheMatrix: PASS")
}
