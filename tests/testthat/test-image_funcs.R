test_that("i_deck adds images to a deck of cards", {

  ic <- i_deck(deck = standard_deck(),
               i_path = getwd())

  expect_s3_class(ic, "ImgDeck")
  expect_s3_class(ic, "StandardDeck")

})

# test_that("i_deck adds images to a deck of cards", {
#
#   ic <- i_deck(deck = standard_deck(),
#                 i_path = paste0(gsub("mmcards", "", getwd()),
#                                 "PNG-cards-1.3/PNG-cards-1.3"))
#
#   expect_s3_class(ic, "ImgDeck")
#   expect_equal(2 * 2, 4)
# })
#
# test_that("i_deck works with a Shiny application", {
#
#     library(shiny)
#
#     # Initialize a shuffled and image-embedded deck
#     ic <- i_deck(deck = shuffle_deck(seed = 42),
#                  i_path = paste0(gsub("mmcards", "", getwd()),
#                                  "PNG-cards-1.3/PNG-cards-1.3"))
#
#     # UI
#     ui <- fluidPage(
#       titlePanel("Card Deck Shiny App"),
#
#       fluidRow(
#         column(6,
#                # Image output
#                imageOutput("cardImage", height = "auto")
#         ),
#         column(6,
#                # Text output
#                textOutput("cardValue"),
#
#                # Deal card button
#                actionButton("deal_card", "Deal Card")
#         )
#       )
#     )
#
#     # Server logic
#     server <- function(input, output, session) {
#
#       # Initialize reactive variable to store current deck
#       current_deck <- reactiveVal(ic)
#
#       # Output for card image
#       output$cardImage <- renderImage({
#         # Initialize or get the current deck
#         deck <- current_deck()
#
#         # Return image path
#         list(src = deck$icard[1],
#              width = 250,
#              height = 350)
#       }, deleteFile = FALSE)
#
#       # Output for card value
#       output$cardValue <- renderText({
#         # Initialize or get the current deck
#         deck <- current_deck()
#
#         # Return card value
#         return(deck$value[1])
#       })
#
#       # Deal card logic
#       observeEvent(input$deal_card, {
#         # Get the current deck
#         deck <- current_deck()
#
#         # Deal a card
#         new_deck <- deal_card(deck)
#
#         # Update the current deck
#         current_deck(new_deck$updated_deck)
#       })
#     }
#
#     # Run the app
#     app <- shinyApp(ui, server)
#
#     expect_s3_class(app, "shiny.appobj")
# })
