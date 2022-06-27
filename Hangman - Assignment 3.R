#' Assignment 3 - 2011
#' Crystal Valdez
#' Hangman Assignment

setwd("~/Documents/MBiotech/MSC2011/Assignments/PeerReviews/Hangman_Crystal_Valdez/")
getwd()

#Retriving text file of words and putting them into a list
inlist <-
  strsplit(readLines("dictionary_of_words.txt"), "[[:space:]]+")
pathways <- lapply(inlist, tail, n = -1)
names(pathways) <- lapply(inlist, head, n = 1)

#Choose a random word from the dictionary list
word <- sample(inlist, 1)

# print word for debugging purposes 
print(word)

#Provide instructions to the user
cat(
  "Your instructions are to guess one letter at a time to make up the secret word. \nYou win if you guess the word right within 10 tries. If you use up your tries, you lose.\nBest of luck!\n"
)

#Tell user the number of letters in word
print(paste0("Your word has ", nchar(word), " letters."))
print(strrep('_ ', nchar(word)))
tries <- 10

#Creating vector for the word
wordpiece <- replicate(nchar(word),"_")


while (tries > 0) {
 
  letter <- tolower(readline("Please guess a letter: "))

  # checking user input is valid 
  while (nchar(letter) != 1 | !is.na(as.numeric(letter)) | grepl("[a-z]", letter) == F) {
    print("Invalid letter entry.")
    letter <- readline("Please guess a single letter: ")
  } # end when entry is valid 
  
  # checking if letter given from user is in the secret word
  if (grepl(letter, word, ignore.case = T)) {
    
    letters <- strsplit(as.character(word),"")[[1]]
    indices <- which(letters == letter)
    wordpiece[indices] <- letter
    print(
      paste("Correct! This letter is in the word"))
    
    # print visual clue for how user is progressing
    print(wordpiece)
  } else {
    print("This letter is not in the word")
    tries <- tries - 1
    print(sprintf("You have %d tries remaining.",tries))
  }

  if (tries == 0) {
    print("You have ran out of tries. Sorry!")
  }
  
  # checking if user has guessed all letters in the word
  if (paste(paste(wordpiece, collapse="" ) == word)) {
    print(paste("Congratulations you have guessed the word: ", word))
    break
  } # break out of while loop if user has guessed all the letters in the word

}

#' Ashley review comment: The variable names were readable so it made the code easier 
#' to follow. It may be good to include a sample dictionary_of_words.txt file so the user
#' can have a dictionary to use right away. Running the code from source there were 
#' some errors like unmatched bracket for example. Meanwhile, some errors only occurred when 
#' certain conditions were met. It is good to test the code using all possible inputs from the user
#' to ensure that if any of the conditions are met at any point in time, there won't be any 
#' errors when that piece of code runs and also to ensure the code runs as expected with each
#' possible input. Lastly, the code before going into the while loop was well commented
#' and was easy to follow. I felt that having more comments within the while loop or 
#' perhaps a block of code before the while loop explaining the code inside the loop
#' could have been helpful for following the code within the while loops. I felt that
#' the code within the while loops was harder to follow without comments.  



