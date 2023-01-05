myPlayer="o"
oppPlayer="x"
#N=4

isMovesLeft = function(g) {
  if(sum(is.na(g))>0) {
    return(TRUE)
  } else {
    return(TRUE)
  }
}

playerMove = function(game,N) {
  print(game)
  while(TRUE) {
    x = readline(paste0("Enter move [1-",N,",1-",N,"]"))
    pattern=paste0("^[1-",N,"],[1-",N,"]$")
    #print(pattern)
    if(!grepl(pattern,x)) {
      print("Invalid Coordinates")
    } else {
      y = as.integer(unlist(strsplit(x,",")))
      if(!is.na(game[y[1],y[2]])) {
        print("Position already taken")
      } else {
        game[y[1],y[2]]=oppPlayer
        break
      }
    }
  }
  return(game)
}

evaluateN = function(N,g) {
  score = 0
  for(row in 1:N) {
    myCount = sum(grepl(myPlayer,g[row,]))
    oppCount = sum(grepl(oppPlayer,g[row,]))
    if(oppCount==0 & myCount>0) {
      score = score + 10^(myCount-1)
    } else if(myCount==0 & oppCount > 0) {
      score = score + (-1 * 10^(oppCount-1))
    } 
  }
  for(col in 1:N) {
    myCount = sum(grepl(myPlayer,g[,col]))
    oppCount = sum(grepl(oppPlayer,g[,col]))
    if(oppCount==0 & myCount>0) {
      score = score + 10^(myCount-1)
    } else if(myCount==0 & oppCount > 0) {
      score = score + (-1 * 10^(oppCount-1))
    } 
  }
  d1 = NA
  d2 = NA
  for(i in 1:N) {
    d1 = c(d1,g[i,i])
    d2 = c(d2,g[i,N-i+1])
  }
  myCount = sum(grepl(myPlayer,d1))
  oppCount = sum(grepl(oppPlayer,d1))
  if(oppCount==0 & myCount>0) {
    score = score + 10^(myCount-1)
  } else if(myCount==0 & oppCount > 0) {
    score = score + (-1 * 10^(oppCount-1))
  }
  
  myCount = sum(grepl(myPlayer,d2))
  oppCount = sum(grepl(oppPlayer,d2))
  if(oppCount==0 & myCount>0) {
    score = score + 10^(myCount-1)
  } else if(myCount==0 & oppCount > 0) {
    score = score + (-1 * 10^(oppCount-1))
  }
  return(score)
}

minimax = function(N,g,depth,player) {
  bestX = NA
  bestY = NA
  bestScore = ifelse(player==myPlayer,-Inf,Inf)
  if(isGameOver(g) | depth == 0) {
    bestScore = evaluateN(N,g)
  } else {
    for(i in 1:N) {
      for(j in 1:N) {
        if(is.na(g[i,j])) {
          g[i,j]=player
          if(player == myPlayer) {
            currentScore = minimax(N,g,depth - 1,oppPlayer)[1]
            if(currentScore > bestScore) {
              bestScore = currentScore
              bestX = i
              bestY = j
            }
          } else {
            currentScore = minimax(N,g,depth - 1,myPlayer)[1]
            if(currentScore < bestScore) {
              bestScore = currentScore
              bestX = i
              bestY = j
            }
          }
          g[i,j]=NA
        }
      }
    }
  }
  return(c(bestScore,bestX,bestY))
}

whoPlaysFirst = function() {
  x = NA
  while(TRUE) {
    x = readline("Do you want to play first? (y/n) :")
    if(!grepl("^[yn]$",x)) {
      print("Invalid Input")
    } else {
      break
    }
  }
  if(x == "y") {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

whatSize = function() {
  x = NA
  while(TRUE) {
    x = readline("Dimension? [3-9] :")
    if(!grepl("^[3-9]$",x)) {
      print("Invalid Input")
    } else {
      break
    }
  }
  return(as.integer(x))
}

play = function() {
  print("You are x and I am o")
  N = whatSize()
  g = newGame(N)
  
  playerFirst = whoPlaysFirst()
  while(TRUE){
    if(didPlayerWin(N,g,oppPlayer)) {
      print(g)
      print("Oh No, You Won. Congratulations!!!")
      break
    } else if (didPlayerWin(N,g,myPlayer)) {
      print(g)
      print("He He. I won. LOOOOSER")
      break
    } else if(isGameOver(g)) {
      print(g)
      print("Game Drawn.")
      break
    }
    
    if(playerFirst) {
      g=playerMove(g,N)
    }  
    playerFirst = TRUE
    move=minimax(N,g,2,myPlayer)
    g[move[2],move[3]]=myPlayer
  }
}

didPlayerWin = function(N,g,player) {
  #print(paste0("In didPlayerWin ",player))
  d1=NA
  d2=NA
  for(i in 1:N){
    d1 = c(d1,g[i,i])
    d2 = c(d2,g[i,N-i+1])
  }
  for(i in 1:N){
    if(sum(grepl(player,g[i,]))==N){
      return(TRUE)
    }
  }
  for(i in 1:N){
    if(sum(grepl(player,g[,i]))==N){
      return(TRUE)
    }
  }
  if(sum(grepl(player,d1))==N) {
    return(TRUE)
  }
  if(sum(grepl(player,d2))==N) {
    return(TRUE)
  }
  return(FALSE)
}

isGameOver = function(g) {
  if(sum(is.na(g))==0){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

newGame = function(N) {
  #return(as.matrix(data.frame("1"=c(NA,NA,NA),"2"=c(NA,NA,NA),"3"=c(NA,NA,NA))))
  return(matrix(nrow=N,ncol=N))
}