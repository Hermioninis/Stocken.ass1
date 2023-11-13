## MCMC for snakes and Ladders










## simulation for snakes and ladders

place_1 <- function(){
  result <- sample(c(3, 3, 4, 7), 1, replace = TRUE)
  return(result)
}

place_3or4 <- function(){
  result <- sample(c(3, 4, 6, 7), 1, replace = TRUE)
  return(result)
}


place_6 <- function(){
  result <- sample(c(4, 7, 9, 9), 1, replace = TRUE)
  return(result)
}

place_7 <- function(){
  result <- sample(c(4, 9, 9, 9), 1, replace = TRUE)
  return(result)
}

countList <- c() #counting the amount of dices thrown per game
count6List <- c()
count6to3List <- c()
simulation <- 10000

for(i in 1:simulation){
  result = place_1() #first dice thrown, cannot win the game
  counter = 1 
  place6Counter = 0
  place6to3Counter = 0
  
  while(result != 9){
    
    if(result == 3){ #same alternatives with the same probability
      if(place6Counter != 0){
        place6to3Counter <- place6to3Counter + 1
        #result = place_3or4()
      }
      result = place_3or4()
    }
    
    else if(result == 4){
      result = place_3or4()
    }
    
    else if(result == 6){
      place6Counter = place6Counter + 1
      result = place_6()
    }
    
    else{
      result = place_7()
    }
    counter = counter + 1 
  }

  if(place6Counter != 0){
    count6List[i] <- place6Counter
  }
  
  if(place6to3Counter != 0){
    count6to3List[i] <- place6to3Counter
  }
  countList[i] <- counter

}
count6List <- na.omit(count6List)
count6to3List <- na.omit(count6to3List)

cat('The expected value  for throws is', mean(countList))
cat('The probability to land on 6 before the end is', length(count6List)/length(countList))
cat('The probability to land 3 after 6 before the end is', length(count6to3List)/length(countList))

