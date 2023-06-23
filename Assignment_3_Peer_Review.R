##ASSIGNMENT 3
# AO: Peer Review

# AO: Overall, the code follows good programming protocol and the Hangman game does function as intended. The code also has sufficient comments to make reviewing simpler.

#First we are going to read the textfile from the working directory
word_list <- readLines("words.txt")

# AO: It would be a good idea to implement error handling in the line above in the case that the txt file does not have the appropriate data within it. This can be done by checking the return value of readlines().
# AO: Example of the error catching code: 
# AO: 
# if (length(word_list) == 0) {
# stop("Word list is empty. Please populate the 'words.txt' file with words.")
# }

# We want to choose a random word from the list, and want to announce how long it is 
secret_word <- tolower(sample(word_list, 1))
word_length <- nchar(secret_word)
#We will create a vector to store the guessed letters
guessed_letters <- vector(mode = "character", length = 0)
#As for the rules, there are 8 guesses allowed so we will define the maximum number of incorrect guesses
incorrrect_limit <- 8
game_state <- list(secret_word = secret_word, guessed_letters = guessed_letters, incorrrect_limit = incorrrect_limit)
#Welcome the user to the game and inform them about the secret word's length
print(paste("Welcome to Hangman! The secret word is", word_length, "letters long."))

# AO: The code above works well and is concise for establishing the parameters of the game.

# I will use a while loop so that it continues to ask for a user input until either the word is guessed or there has been more than 8 incorrect guesses
while (TRUE) { 
  # Display the secret word with dashes for unguessed letters
  # Initialize an empty string to hold the word display
  word_display <- ""
  #Iterate over each letter in the secret word, separating by nothing so it splits the word by each letter individually
  for (letter in strsplit(secret_word, "")[[1]]) {
    #Check if the letter is already guessed from the list of previously guessed letters
    if (letter %in% guessed_letters) {
      #If the letter is guessed we will add it to the word display
      word_display <- paste(word_display, letter, sep = "")
    } else {
      #If the letter is not guessed we will add a placeholder underscore to the word display
      word_display <- paste(word_display, "_", sep = "")
    }
  }
  # Print the word display to show the current state of the guessed word
  print(word_display)
  
  # AO: The code above does not fully stop a user from entering the same letter multiple times. I would add a line of defensive coding to prevent this error from occuring.
  
  # Display the guessed letters so that the user can keep count and this will ensure that the game is efficient
  guessed_letters_str <- paste(guessed_letters, collapse = ", ")
  print(paste("Guessed letters:", guessed_letters_str))
  # Ask the user for a guess
  guess <- readline("Enter a letter: ")
  # We need to make sure that the guess meets the requirements 
  if (nchar(guess) != 1) { #it must only be one character long
    print("Invalid input. You must enter one letter only. Try again.") #if it is longer then it will tell the user what their input error was
    next
  }
  
  # AO: The code above works correctly and also has good defensive code to ensure that users can only input 1 letter at a time.
  
  # We will convert the guess to lowercase to make identification easier 
  guess <- tolower(guess)
  
  # We also want to check if the input is a letter, it shouldn't be a number or character. 
  if (!grepl("^[a-z]$", guess)) {
    print("Invalid input. You must enter a letter. Please try again.")
    next
  }
  
  # AO: Good defensive coding again to ensure that users must input a character.
  
  #After a valid input has been entered, it will be added to the list of guessed letters form before to update as the while loop continues to loop
  guessed_letters <- c(guessed_letters, guess)
  
  #In each loop, we will check if the conditions of the game have been met and if the game can be over. 
  
  #We will check if the player has guessed the entire secret word correctly
  if (all(strsplit(secret_word, "")[[1]] %in% guessed_letters)) {
    print(paste0("WINNER! You guessed the secret word! It was ", secret_word, "!!"))
    return(TRUE)  #Return TRUE to indicate the game is over
  }
  
  #Then we will calculate the number of incorrect guesses by counting letters in guessed_letters not present in the secret word
  incorrect_guesses <- sum(!(guessed_letters %in% strsplit(secret_word, "")[[1]]))
  #If the player has exceeded the maximum allowed incorrect guesses, the game will end 
  if (incorrect_guesses >= incorrrect_limit) {
    print("Game over! You lost):")
    print(paste("The secret word was:", secret_word))
    return(TRUE)  # Return TRUE to indicate the game is over
  }
  
}#closing the while loop 

# AO: The user isn't able to guess the full word, rather they are only able to guess word for word. I would consider adding lines of code to allow the user to guess the full word.
# AO: An example of this is:
# AO:
# AO: ...

# AO: After a valid input has been entered, it will be added to the list of guessed letters from before to update as the while loop continues to loop
# AO: guessed_letters <- c(guessed_letters, guess)

# AO: Check if the guess matches the secret word
# AO: if (guess == secret_word) {
# AO: print(paste0("WINNER! You guessed the secret word! It was ", secret_word, "!!"))
# AO: return(TRUE)  # Return TRUE to indicate the game is over
# AO: }

# AO: ...


# AO: It would also be a good idea to add a line of code to prompt the user to "Play Again". 
# AO: An example of this is:
# AO: play_again <- TRUE
# while (play_again) {
# Existing code for the game
# After the game ends, ask the user if they want to play again
# play_again_input <- tolower(readline("Do you want to play again? (yes/no): "))

# if (play_again_input %in% c("no", "n")) {
# play_again <- FALSE
# }
# }