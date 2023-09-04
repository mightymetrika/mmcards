#' Add image paths to a deck of cards
#'
#' This function takes a deck of cards and adds image paths to each card.
#' It produces a new deck that includes the original information along with the
#' image paths. The function is designed to work with various types of decks and
#' allows for customization of image paths.
#'
#' @param deck A data frame representing a deck of cards. Default is standard_deck(),
#' but the function is also optimized to work with shuffled_deck().
#' @param i_path The file path to the folder containing the card images.
#' @param cards A vector of card names corresponding to the images. Default includes
#' all cards based on a deck created with standard_deck()
#' @param i_names A vector of image file names corresponding to the cards.
#' Should be in the same order as `cards` the default is vector_playing_cards
#' which works when `deck` is set to standard_deck() and `cards` uses the default
#' vector.
#' @param i_type The file extension of the image files (e.g., "png", "jpg").
#'
#' @return A data frame that includes the original deck along with image paths for
#' each card. Inherits the class "ImgDeck" in addition to the original deck's classes.
#'
#' @examples
#' \dontrun{
#'   image_deck <- i_deck(deck = standard_deck(),
#'                        i_path = paste0(gsub("mmcards", "", getwd()),
#'                                             "PNG-cards-1.3/PNG-cards-1.3"))
#' }
#'
#' @seealso \code{\link{standard_deck}}, \code{\link{shuffle_deck}}, \code{\link{deal_card}}
#'
#' @export
i_deck <- function(deck,
                    i_path,
                    cards = c("2C", "2D", "2H", "2S", "3C", "3D", "3H", "3S",
                              "4C", "4D", "4H", "4S", "5C", "5D", "5H", "5S",
                              "6C", "6D", "6H", "6S", "7C", "7D", "7H", "7S",
                              "8C", "8D", "8H", "8S", "9C", "9D", "9H", "9S",
                              "10C", "10D", "10H", "10S", "JC", "JD", "JH", "JS",
                              "QC", "QD", "QH", "QS", "KC", "KD", "KH", "KS",
                              "AC", "AD", "AH", "AS"),
                    i_names = vector_playing_cards,
                    i_type = "png"){

  # Get folder with images
  image_paths <- paste0(i_path, "/", i_names, ".", i_type)

  # Create image data frame
  image_frame <- data.frame(card = cards,
                            icard = image_paths)
  # Store the classes of deck
  deck_classes <- class(deck)

  # Merge deck and image_frame without sorting
  ideck <- merge(deck, image_frame, by = "card", sort = FALSE)

  # To ensure the order is exactly the same as the original deck
  ideck <- ideck[match(deck$card, ideck$card),]

  #Add class "ideck"
  class(ideck) <- append("ImgDeck", deck_classes)

  # Return
  return(ideck)
}
