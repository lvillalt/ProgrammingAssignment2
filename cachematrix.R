makeCacheMatrix <- function(x = matrix(), nr, nc, br) {

  ## The problem setup asks to do this if the matrix has not changed, to this 
  ## effect we will calculate column and row sum vectors. If BOTH are identical 
  ## to the cached ones, we will use the cached inversion.
  ##
  ## If any of the vectors is different, we will calculate a new inverse, and
  ## cache the new inverse, and the new column and row vectors

       i <- NULL
        set <- function(y, nr, nc, br) {
                x <<- matrix(y, nrow=nr, ncol=nc, byrow=br)
				rS <<- rowSums(x)
				cS <<- colSums(x)
                i <<- NULL
        }
        get <- function() matrix(x, nrow=nr, ncol=nc, byrow=br)
        setinverse <- function(solve) i <<- solve
		setrowsums <- function() rS <<- rowSums(matrix(x, nrow=nr, ncol=nc, byrow=br))
		setcolsums <- function() cS <<- colSums(matrix(x, nrow=nr, ncol=nc, byrow=br))
        getinverse <- function() i
		getrowsums <- function() rS
		getcolsums <- function() cS
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
			 setrowsums = setrowsums,
			 setcolsums = setcolsums,
			 getrowsums = getrowsums,
			 getcolsums = getcolsums)
}

cacheSolve <- function(x, ...) {

  ## return a matrix that is the inverse of x
  ## Since non square matrices (e.g. An n x m matrix where n != m) do not have
  ## an inverse, and the assumption given is that the matrix, x, has an inverse,
  ## then x is both SQUARE (n x n) and INVERTIBLE. 
  ##
  ## This function will then calculate a matrix Y, such that XY = YX = I_n
  ## where I_n is the n x n identity matrix. Then Y = X^-1

  ## The problem setup asks to do this if the matrix has not changed, to this 
  ## effect we will calculate column and row sum vectors. If BOTH are identical 
  ## to the cached ones, we will use the cached inversion.
  ##
  ## If any of the vectors is different, we will calculate a new inverse, and
  ## cache the new inverse, and the new column and row vectors
  
         i <- x$getinverse()
		 locRsum <- rowSums(x$get())
		 locCsum <- colSums(x$get())
        if(!is.null(i)&&identical(locRsum, x$getrowsums())&&identical(locCsum, x$getcolsums())) {
                message("getting cached data")
                return(i)
        }
        data <- as.matrix(x$get(), nrow=nrow(x$get()), ncol=ncol(x$get()))
        i <- solve(data, ...)
        x$setinverse(i)
		x$setrowsums()
		x$setcolsums()
        i
}
