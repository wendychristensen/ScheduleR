# Enter data

ta1 <- cbind ("Amy Z", 0, 1, 1, 1, 0)
ta2 <- cbind ("Brian Y", 0, 0, 1, 1, 1)
ta3 <- cbind ("Carol X", 1, 0, 1, 0, 1)
ta4 <- cbind ("Daniel W", 0, 0, 1, 1, 0)
ta5 <- cbind ("Elaine V", 0, 1, 1, 1, 0)

data = rbind(ta1,ta2,ta3,ta4,ta5)

# Turn data into data frame and give names to section columns

data.df1 <- as.data.frame(data)
names(data.df1) <- c("Name","A","B","C", "D", "E")
data.df1

# Determine how many sections need to be assigned

sectnum = nrow(data.df1)
sectnum

# For loop to assign sections to TAs

for (i in 1:sectnum) {
  
  data.df1$A <- as.numeric(as.character(data.df1$A))
  data.df1$B <- as.numeric(as.character(data.df1$B))
  data.df1$C <- as.numeric(as.character(data.df1$C))
  data.df1$D <- as.numeric(as.character(data.df1$D))
  data.df1$E <- as.numeric(as.character(data.df1$E))
  
  sapply(data.df1, class) #Checks that column 1 (name) is character and columns 2-6 (sections) are numeric
  
  #### Loop starts here
  
  # Sums each section column and appends the sums to the schedule data frame
  
  countA = sum(data.df1$A,na.rm=T)    
  countB = sum(data.df1$B,na.rm=T)
  countC = sum(data.df1$C,na.rm=T)
  countD = sum(data.df1$D,na.rm=T)
  countE = sum(data.df1$E,na.rm=T)
  
  countAll = cbind("Sum",countA, countB, countC, countD, countE) #Creates a vector of counts
  countAll.df <- as.data.frame(countAll)
  names(countAll.df) <- c("Name","A","B","C", "D", "E")
  countAll.df
  
  data.df1 = rbind(data.df1,countAll.df)
  data.df1
  
  # Make section columns numeric again
  
  data.df1$A <- as.numeric(as.character(data.df1$A))
  data.df1$B <- as.numeric(as.character(data.df1$B))
  data.df1$C <- as.numeric(as.character(data.df1$C))
  data.df1$D <- as.numeric(as.character(data.df1$D))
  data.df1$E <- as.numeric(as.character(data.df1$E))
  
  sapply(data.df1, class) 
  
  # Determine which section has the fewest endorsements
  
  sumorder = data.df1[6,][order(data.df1[6,])] # Orders the sum row endorsements from lowest to highest
  sumorder
  low = sumorder[[1]]  # Isolates the first value of sumorder, which will be a number (i.e. the lowest current sum)
  low
  
  a = which((data.df1[6,] == low) == TRUE) # Creates a vector identifying which column/s have sums equal to the lowest current sum
  a
  b=a[[1]] # Isolates the first value of a, identifying the section which has the lowest current sum  b

  data.df1[,b]
  
  # Determine which TA is assigned to the section with fewest endorsements
  
  c = which(data.df1[,b]==1) # Creates a vector identifying which rows (i.e., TAs) have availability in the section with the fewest endorsements
  c
  d=c[1] # Isolates the first value of c, which will be the number of the row of the first/only TA who endorsed having availability for that section
  d
  
  # Assign a TA to a section
  
  data.df1[d,b] = 99
  data.df1
  
  # Remove the TA and the assigned section from the schedule
  
  data.df1[,b][data.df1[,b] < 99] <-NA #turned all non-99 values in column to NA
  data.df1[d,][data.df1[d,] < 99] <-NA #turned all non-99 values in column to NA, thorws a warning but works
  data.df1
  
  # Return the schedule to its orginal state for looping
  
  data.df1 = data.df1[-nrow(data.df1),] # Removes sum row
  data.df1
  
}

# Create a vector that displays the name of the column where each row has a 99

label = names(data.df1)

section = c(label[(which(data.df1[1,]=="99"))],label[(which(data.df1[2,]=="99"))],label[(which(data.df1[3,]=="99"))],label[(which(data.df1[4,]=="99"))], label[(which(data.df1[5,]=="99"))])

# Create new data set that binds the schedule and the vector of assignments

schedule = cbind(data.df1, section)
schedule

paste("Assign", schedule$Name, "to Section", schedule$section)


