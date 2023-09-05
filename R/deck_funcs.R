#' Generate a Standard Deck of Playing Cards
#'
#' This function creates a standard deck of playing cards represented as a data
#' frame. The deck includes suits, ranks, and values for each card.
#'
#' @param suits A character vector specifying the suits in the deck. Default is
#' c('C', 'D', 'H', 'S') for Clubs, Diamonds, Hearts, and Spades.
#' @param ranks A character vector specifying the ranks in the deck. Default is
#' c('2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A') for ranks
#' 2 to Ace.
#' @param values A numeric vector specifying the values assigned to each card in
#' the deck. Default is a sequence from 2 to 14.75 incremented by 0.25.
#'
#' @return A data frame representing the deck of cards. The data frame has four
#' columns: `rank`, `suit`, `card`, and `value`. The data frame also has class
#' attributes "StandardDeck" and "data.frame".
#'
#' @examples
#' deck <- standard_deck()
#' head(deck)
#' tail(deck)
#'
#' @export
standard_deck <- function(suits = c('C', 'D', 'H', 'S'),
                          ranks = c('2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A'),
                          values = seq(2, 14.75, by = 0.25)){

  # Check if suits and ranks are character vectors
  if (!is.character(suits)) {
    stop("The 'suits' argument must be a character vector.")
  }

  if (!is.character(ranks)) {
    stop("The 'ranks' argument must be a character vector.")
  }

  # Check if the length of suits is 4
  if (length(suits) != 4) {
    stop("The 'suits' argument must have a length of 4.")
  }

  # Check if values is a numeric vector
  if (!is.numeric(values)) {
    stop("The 'values' argument must be a numeric vector.")
  }

  # Check if the length of values matches the number of cards in the deck
  if (length(values) != length(suits) * length(ranks)) {
    stop("The length of the 'values' argument must match the number of cards in the deck (length of 'suits' * length of 'ranks').")
  }

  # Create the deck
  deck <- expand.grid(rank = ranks, suit = suits)
  deck$card <- paste0(deck$rank, deck$suit)

  # Turn suit to factor
  deck$suit <- factor(deck$suit,
                      levels = suits,
                      labels = suits)

  # Order deck
  deck <- deck[order(deck$rank, deck$suit), ]

  # Add values to cards
  deck$value <- values

  # Turn card to factor using the current order
  deck$card <- factor(deck$card, levels = deck$card)

  #Set class
  class(deck) <- c("StandardDeck", "data.frame")

  return(deck)
}


#' Shuffle a Deck of Cards
#'
#' This function shuffles a deck of cards and returns the shuffled deck.
#' The function can handle standard decks, anonymous decks, and interleaved decks.
#' For interleaved decks, an option to pair shuffle is also available.
#'
#' @param deck_of_cards An anonymous function that returns a deck of cards as
#' either a data frame or a list of two numeric vectors for interleaved decks.
#' Default is `function(x){standard_deck()}`.
#' @param seed An optional seed for reproducibility. Default is NULL.
#' @param paired Logical flag to indicate if the interleaved deck should be pair
#' shuffled. Default is FALSE.
#'
#' @return A data frame representing the shuffled deck of cards. The data frame
#' inherits various classes based on its type. All shuffled decks will have the
#' classes "ShuffledDeck" and "data.frame". Additional class inheritance depends
#' on the `deck_of_cards` parameter:
#' - "StandardDeck" if `deck_of_cards` returns a standard deck (default)
#' - "AnonymousDeck" if `deck_of_cards` returns a single vector
#' - "InterleavedDeck" if `deck_of_cards` returns a list of two vectors.
#' If the `paired` parameter is set to TRUE, an interleaved deck will also
#' inherit the class "PairedDeck".
#'
#' @examples
#' # Standard deck
#' std_deck <- shuffle_deck()
#' head(std_deck)
#'
#' # Anonymous deck
#' anon_deck <- shuffle_deck(deck_of_cards = function(x){runif(52, 1, 10)})
#' head(anon_deck)
#'
#' # Interleaved deck
#' interleaved_deck <- shuffle_deck(
#'                         deck_of_cards = function(x){list(runif(26, 1, 5),
#'                                                          runif(26, 6, 10))})
#' head(interleaved_deck)
#'
#' # Paired interleaved deck
#' paired_deck <- shuffle_deck(
#'                   deck_of_cards = function(x){list(runif(26, 1, 5),
#'                                                    runif(26, 6, 10))},
#'                   paired = TRUE)
#' head(paired_deck)
#'
#' @export
shuffle_deck <- function(deck_of_cards = function(x){standard_deck()}, seed = NULL,
                         paired = FALSE) {

  # Check if 'seed' is a valid single integer
  if (!is.null(seed) && (!is.numeric(seed) || length(seed) != 1)) {
    stop("The 'seed' parameter must be a single integer.")
  }

  # Check if 'paired' is a logical value
  if (!is.logical(paired) || length(paired) != 1) {
    stop("The 'paired' parameter must be a single logical value.")
  }

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate the values based on the anonymous function
  decks <- deck_of_cards(NULL)

  # Check if the result from 'deck_of_cards' is valid
  if (!(inherits(decks, "StandardDeck") ||
        (is.numeric(decks) && is.vector(decks)) ||
        (is.list(decks) && length(decks) == 2 &&
         is.numeric(decks[[1]]) && is.numeric(decks[[2]])))) {
    stop("The 'deck_of_cards' function must return either a 'StandardDeck', a numeric vector, or a list of two numeric vectors.")
  }

  # If the result is a list with two elements, treat them as separate decks
  if (is.list(decks) && length(decks) == 2) {

    # Check if the two decks are of the same length
    if (length(decks[[1]]) != length(decks[[2]])) {
      stop("Both decks must be of the same length for interleaving!")
      }

    # Create separate decks for player and computer
    A_deck <- data.frame(card = paste("A", 1:length(decks[[1]]), sep = "_"), value = decks[[1]])
    B_deck <- data.frame(card = paste("B", 1:length(decks[[2]]), sep = "_"), value = decks[[2]])

    # Shuffle each deck
    if (paired == TRUE){
      index <- sample(nrow(A_deck))

      A_deck <- A_deck[index, ]
      B_deck <- B_deck[index, ]
    } else {
      A_deck <- A_deck[sample(nrow(A_deck)), ]
      B_deck <- B_deck[sample(nrow(B_deck)), ]
    }

    # Interleave the decks
    interleaved_deck <- rbind(A_deck, B_deck)[order(rep(1:nrow(A_deck), 2)), ]

    # Add classes and return deck
    class(interleaved_deck) <- c("InterleavedDeck", "ShuffledDeck", "data.frame")
    if (paired == TRUE){
      class(interleaved_deck) <- append("PairedDeck", class(interleaved_deck))
    }

    return(interleaved_deck)

    } else if (!inherits(decks, "StandardDeck")) {

      # If it's just one deck, treat it as before
      values <- decks
      ordered_indices <- order(values, decreasing = FALSE)
      ordered_deck <- data.frame(card = 1:length(values), value = values[ordered_indices])

      # Shuffle the ordered deck
      shuffled_deck <- ordered_deck[sample(nrow(ordered_deck)), ]

      # Add classes and return deck
      class(shuffled_deck) <- c("AnonymousDeck", "ShuffledDeck", "data.frame")
      return(shuffled_deck)

      } else {

        # Shuffle the provided deck
        shuffled_deck <- decks[sample(nrow(decks)), ]

        # Add classes and return deck
        class(shuffled_deck) <- c("StandardDeck", "ShuffledDeck", "data.frame")
        return(shuffled_deck)
      }
}

#' Deal a Card from the Deck
#'
#' This function deals the top card from a given deck and returns the dealt card
#' along with the updated deck.
#'
#' @param current_deck A data frame representing the current deck of cards. This
#' can either be a standard deck, an anonymous deck, or an interleaved deck.
#' The function also accepts an object of class "UpDeck" which contains an
#' updated deck and the last dealt card.
#'
#' @return A list containing two elements: `dealt_card`, a data frame representing
#' the card that was dealt, and `updated_deck`, a data frame representing the
#' remaining cards in the deck. The list has the class attribute "UpDeck".
#'
#' @examples
#' # Using a standard deck
#' std_deck <- standard_deck()
#' result <- deal_card(std_deck)
#' result$dealt_card
#' result$updated_deck
#'
#' # Using an "UpDeck" object
#' result2 <- deal_card(result)
#' result2$dealt_card
#' result2$updated_deck
#'
#' @export
deal_card <- function(current_deck) {

  # Check if 'current_deck' inherits from one of the expected classes
  if (!inherits(current_deck, c("StandardDeck", "ShuffledDeck", "UpDeck"))) {
    stop("The 'current_deck' must inherit from 'StandardDeck', 'ShuffledDeck', or 'UpDeck'.")
  }


  if (!inherits(current_deck, "UpDeck")){

    if (nrow(current_deck) == 0) {
      stop("No more cards in the deck!")
    }

    card_to_deal <- current_deck[1, ]
    updated_deck <- current_deck[-1, ]  # Remove the top card

  } else {

    if (nrow(current_deck$updated_deck) == 0) {
      stop("No more cards in the deck!")
    }

    card_to_deal <- current_deck$updated_deck[1, ]
    updated_deck <- current_deck$updated_deck[-1, ]  # Remove the top card

  }

  up_deck <- list(dealt_card = card_to_deal, updated_deck = updated_deck)
  class(up_deck) <- "UpDeck"

  return(up_deck)
}
