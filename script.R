setwd('C:/Users/lenda/Desktop/VUT_Ing/1_semestr/MPA-PRG/exercises/MPA_PRG_exercise_04')


GetChange1 <- function(M){ # czech crowns
  r <- M
  pd <- floor(r/50)
  r <- r - pd*50
  dc <- floor(r/20)
  r <- r - dc*20
  ds <- floor(r/10)
  r <- r - ds*10
  p <- floor(r/5)
  r <- r - p*5
  d <- floor(r/2)
  j <- r - d*2

  return (c(pd, dc, ds, p, d, j))
}
GetChange1(77)

GetChange2_universal <- function(M, list_of_coins){ # universal currency
  list
  r <- M
  len_of_list <- length(list_of_coins)
  output <- 1:len_of_list
  for (i in 1:(len_of_list)){
    output[i] <- floor(r/list_of_coins[i])
    r <- r - output[i] * list_of_coins[i]
  }
  #output <- append(output, r)
  return (output)
}
GetChange2_universal(295, c(100,50,30,20,10, 5))
GetChange2_universal(53.5, c(5,3,1))


MostChocolatePath <- function (M, row=1, col=1){
  if (row == nrow(M)){
    return (M[row, col])
  } else{
    bars <- M[row, col]
    down <- MostChocolatePath(M, row+1, col)
    diagonal <- MostChocolatePath(M, row+1, col+1)
    return (max(down, diagonal) + bars)
  }
}
MostChocolatePath(matrix(c(3,1,5,1,0,4,3,2,0,0,0,6,0,0,0,7), nrow = 4, ncol = 4)) # matrix written by columns

HanoiTowers <- function(n, fromPeg, toPeg){
  if (n == 1){
    return (print(paste0('Move disk from peg ', fromPeg, ' to ', toPeg)))
  }
  unusedPeg <- 6 - fromPeg - toPeg
  HanoiTowers(n-1, fromPeg, unusedPeg)
  print(paste0('Move disk from peg ', fromPeg, ' to ', toPeg))
  HanoiTowers(n-1, unusedPeg, toPeg)
}

number_of_disks <- 3
HanoiTowers(number_of_disks, 1,3)
HanoiTowers(5, 1,3)