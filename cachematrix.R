## The module contains the implementation of caching computing of matrix inversion.
## (you can find this text in Russian in the end of file)
## 
## The algorithm of module using as follows:
## 1. Create a square matrix, for example: 
##    > a <- matrix(c(3,1,5,2,3,3,2,1,4),3,3)
## 2. Create an object that will be responsible for caching inverted matrix,
## for example:  
##    > a_cache <- makeCacheMatrix(a)
##    Function makeCacheMatrix(x) creates environment, which contains:
##      two variables (you can't access to it directly, it looks like private fields
##      in object-oriented languages):
##        x - matrix to be inverse (it is parameter);
##        m - cache of inverted matrix;
##      and four functions:
##        set(y) - to set matrix to be inverse (when you use this function, cache  
##                 value is cleaned);
##        get() - to get original matrix as a matrix :) 
##        set_inverse(inverse) - to set inverted matrix to cache (do not use this 
##                  function directly, it is bad solution. Actually we need  
##                  to include all functionality of makeCacheMatrix(x)  
##                  in set_inverse(inverse) for more safety);
##        get_inverse() - to get inverse matrix (be carefull, if you change matrix 
##                 and don't call function cacheSolve, this function returned NULL).
## 3. Now you can calculate inverse matrix of a:
##    > cacheSolve(a_cache)
##    [,1] [,2] [,3]
##    [1,]  1.8 -0.4 -0.8
##    [2,]  0.2  0.4 -0.2
##    [3,] -2.4  0.2  1.4
## 4. And again (repeated call does not cause recalculation, it takes inverted
## matrix from cache variable m):
##    > cacheSolve(a_cache)
##    getting cached data
##    [,1] [,2] [,3]
##    [1,]  1.8 -0.4 -0.8
##    [2,]  0.2  0.4 -0.2
##    [3,] -2.4  0.2  1.4
## 5. You can change matrix in object (it is more efficient then cteate new object):
##    > a_cache$set(matrix(1:4,2,2))
##    > cacheSolve(a_cache)
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    > cacheSolve(a_cache)
##    getting cached data
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
## 6. You can also use cacheSolve() for solving a system of equations, 
## as a usual solve() due to parameter "...". Solution is also cached,  
## but does not consider the vector b, it can give the wrong answer:
##    > a_cache$set(matrix(c(1,0,0,1),2,2))
##    > cacheSolve(a_cache,b = c(3,4))
##    [1] 3 4
##    > cacheSolve(a_cache,c(1,2))
##    getting cached data
##    [1] 3 4

## Function makeCacheMatrix(x) creates environment for caching
## x - matrix to be inverted
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) { m <<- inverse }
        get_inverse <- function() {m}
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse
        )
}


## Special function to calculate inverted matrix using cache
## x - an object created by function makeCacheMatrix
cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}

## Этот модуль реализует вычисление обратной матрицы с кэшированием (то есть, 
## если для одной матрицы вызвать функцию несколько раз, то вычисления будут
## производиться при первом вызове, а потом результат сохранится в памяти и будет
## браться оттуда).
## 
## Алгоритм использования (и работы) модуля следующий:
## 1. Создаем квадратную матрицу, например: 
##    > a <- matrix(c(3,1,5,2,3,3,2,1,4),3,3)
## 2. Создаем объект, который будет отвечать за кэширование инвертированной матрицы,
## например: 
##    > a_cache <- makeCacheMatrix(a)
##    Функция makeCacheMatrix(x) создает среду, которая содержит:
##      две переменные (вы не можете получить к ним доступ напрямую, это похоже на
##      ptivate-поля в объектно-ориентированных языках):
##        x - матрица, которую будем инвертировать (это параметр);
##        m - сохраненное значение инвертированной матрицы;
##      и четыре функции:
##        set(y) - установить матрицу для инвертирования (при использовании этой функции  
##                 вычисленное значение инвертированной матрицы удаляется из кэша);
##        get() - получить исходную матрицу в виде матрицы :) 
##        set_inverse(inverse) - записать инвертированную матрицу в кэш (не используйте 
##                  эту функцию непосредственно, это плохое решение. По-нормальному  
##                  надо запихатьвсю функциональность функции makeCacheMatrix(x)  
##                  в set_inverse(inverse) для большей безопасности);
##        get_inverse() - получить инвертированную матрицу (остророжно,если вы меняли 
##                 исходную матрицу и не вызвали cacheSolve, то эта функция вернет NULL).
## 3. Теперь можно вычислять инвертированную матрицу для a:
##    > cacheSolve(a_cache)
##    [,1] [,2] [,3]
##    [1,]  1.8 -0.4 -0.8
##    [2,]  0.2  0.4 -0.2
##    [3,] -2.4  0.2  1.4
## 4. И снова (при повторном вызове вычисления не повторяются, инвертированная матрица 
## достается из переменной m):
##    > cacheSolve(a_cache)
##    getting cached data
##    [,1] [,2] [,3]
##    [1,]  1.8 -0.4 -0.8
##    [2,]  0.2  0.4 -0.2
##    [3,] -2.4  0.2  1.4
## 5. Можно поменять исходную матрицу в объекте (это эффективнее, чем создавать 
## новый объект):
##    > a_cache$set(matrix(1:4,2,2))
##    > cacheSolve(a_cache)
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
##    > cacheSolve(a_cache)
##    getting cached data
##    [,1] [,2]
##    [1,]   -2  1.5
##    [2,]    1 -0.5
## 6. Можно использовать cacheSolve() для решения систем линейных уравнений, 
## как и обычную функцию solve() благодаря параметру "...". Решение тоже кэшируется, 
## но при этом не учитывается вектор b, так что ответ может быть неверным:
##    > a_cache$set(matrix(c(1,0,0,1),2,2))
##    > cacheSolve(a_cache,b = c(3,4))
##    [1] 3 4
##    > cacheSolve(a_cache,c(1,2))
##    getting cached data
##    [1] 3 4
