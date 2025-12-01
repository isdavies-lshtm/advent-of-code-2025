
rotations <- read.table("day1-input.txt")

names(rotations)[1] <- "code"
rotations$code <- as.character(rotations$code)

rotations$dir <- substr(rotations$code, 1, 1)
rotations$num <- as.integer(substr(rotations$code, 2, nchar(rotations$code)))


initial_position <- 50
zero_counter <- 0

get_new_position <- function(start_position, dir, num) {
  if (dir == "R") {
    new_position <- start_position + num
  } else if (dir == "L") {
    new_position <- start_position - num
  }
  return(new_position)
}

check_position <- function(position) {position %% 100 == 0} 


# Part 1
start_position <- initial_position
for (i in 1:nrow(rotations)) {
  dir <- rotations$dir[i]
  num <- rotations$num[i]
  position <- get_new_position(start_position, dir, num)
  zero_counter <- zero_counter + check_position(position)
  start_position <- position
}

print(zero_counter)


# Part 2 

check_transition <- function(dir, current_position, previous_position) {
  # get upper and lower bounds of numbers passed through, excluding the start and end position
  if (dir == "R") {
    high <- current_position - 1
    low <- previous_position + 1
  } else if (dir == "L") {
    high <- previous_position - 1
    low <- current_position + 1
  }
  # count how many multiples of 100 appear in this range
  zeros_passed <- floor(high/100) - ceiling(low/100) + 1
  return(zeros_passed)
}

zero_counter_p2 <- 0

start_position <- initial_position
for (i in 1:nrow(rotations)) {
  dir <- rotations$dir[i]
  num <- rotations$num[i]
  position <- get_new_position(start_position, dir, num)
  zero_counter_p2 <- zero_counter_p2 + check_position(position) + check_transition(dir, position, start_position)
  start_position <- position
}

print(zero_counter_p2)









