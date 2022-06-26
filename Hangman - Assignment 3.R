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
wordpiece <- vector()


while (tries > 0) {
  letter <- tolower(readline("Please guess a letter: "))

  # checking user input is valid 
  while (nchar(letter) != 1 | !is.na(as.numeric(letter))) {
    print("Invalid letter entry.")
    letter <- readline("Please guess a single letter: ")
  } # end when entry is valid 
  
  # checking if letter given from user is in the secret word
  if (grepl(letter, word, ignore.case = T)) {
    wordpiece <- append(wordpiece, letter)
    correctplace <- which(word == letter)
    
    for (i in correctplace) {
      wordpiece[i] <- letter
    }
    print(
      paste("Correct! This letter is in the word"))
  } else {
    print("This letter is not in the word")
    tries <- tries - 1
    print(sprintf("You have %d tries remaining.",tries))
  }

  if (tries == 0) {
    print("You have ran out of tries. Sorry!")
  }
  if (paste(wordpiece == word)) {
    print(paste("Congratulations you have guessed the word: ", word))
  }

}



