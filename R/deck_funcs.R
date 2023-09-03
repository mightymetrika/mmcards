standard_deck <- function(suits = c('C', 'D', 'H', 'S'),
                          ranks = c('2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A'),
                          values = seq(2, 14.75, by = 0.25)){
  # Create the deck
  deck <- expand.grid(rank = ranks, suit = suits)
  deck$card <- paste0(deck$rank, deck$suit)

  # Turn suit to factor
  deck$suit <- factor(deck$suit,
                      levels = c("C", "D", "H", "S"),
                      labels = c("C", "D", "H", "S"))

  # Order deck
  deck <- deck[order(deck$rank, deck$suit), ]

  # Add values to cards
  deck$value <- values

  # Turn card to factor using the current order
  deck$card <- factor(deck$card, levels = deck$card)

  #Set class
  class(deck) <- c("data.frame", "deck")

  return(deck)
}


shuffle_deck <- function(deck_of_cards = function(x){standard_deck()}, seed,
                         paired = FALSE) {

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
  }

  # Generate the values based on the anonymous function
  decks <- deck_of_cards(NULL)

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

    class(interleaved_deck) <- c("data.frame", "shuffled deck", "interleaved deck")
    return(interleaved_deck)

    } else if (!inherits(decks, "deck")) {

      # If it's just one deck, treat it as before
      values <- decks
      ordered_indices <- order(values, decreasing = FALSE)
      ordered_deck <- data.frame(card = 1:length(values), value = values[ordered_indices])

      # Shuffle the ordered deck
      shuffled_deck <- ordered_deck[sample(nrow(ordered_deck)), ]
      class(shuffled_deck) <- c("data.frame", "shuffled deck", "anonymous deck")
      return(shuffled_deck)

      } else {

        # Shuffle the provided deck
        shuffled_deck <- decks[sample(nrow(decks)), ]
        class(shuffled_deck) <- c("data.frame", "shuffled deck", "deck")
        return(shuffled_deck)
      }
}


deal_card <- function(current_deck) {

  if (!inherits(current_deck, "up_deck")){

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
  class(up_deck) <- "up_deck"

  return(up_deck)
}
