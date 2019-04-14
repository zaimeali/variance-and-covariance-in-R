dataset = matrix(
  c(9, 39, 15, 56, 25, 93, 14, 61, 10, 50, 18, 75, 0, 32, 16, 85, 5, 42, 19, 70, 16, 66, 20, 80),
  ncol = 2,
  byrow = TRUE
)

colnames(dataset, do.NULL = FALSE)
colnames(dataset) <- c("X","Y")


dataset <- as.data.frame(dataset)


TotalXY = colSums(dataset)

MeanXY = colMeans(dataset)

plot(dataset)

summary(dataset)

var(dataset)

cov(dataset)

X = matrix(
  dataset$X,
  ncol = 1,
  byrow = TRUE
)

Y = matrix(
  dataset$Y,
  ncol = 1,
  byrow = TRUE
)

Xlen = length(X)

Ylen = length(Y)

meanM <- function(arr , len){
  return( sum(arr)/len )
}

matCov = matrix(
  ncol = 2,
  nrow = 2,
  byrow = TRUE
)

covM <- function(arr1, arr2, len){
  sum = 0
  for(i in 1: len){
    sum = sum + (arr1[i] - meanM(arr1, len)) * (arr2[i] - meanM(arr2, len))
  }
  return(sum / (len - 1))
}


for(i in 1: 3){
  for(j in 1: 3){
    if(i == 1 & j == 1){
      matCov[i, j] = covM(X, X, Xlen)
    }
    else if(i == 1 & j == 2){
      matCov[i, j] = covM(X, Y, Xlen)
    }
    else if(i == 2 & j == 1){
      matCov[i, j] = covM(Y, X, Xlen)
    }
    else if(i == 2 & j == 2){
      matCov[i, j] = covM(Y, Y, Xlen)
    }
  }
}


cov(dataset)

print(matCov)
