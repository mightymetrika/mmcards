
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmcards

Games of chance serve as essential learning tools for understanding
probability and statistics. The ‘mmcards’ package, short for Mighty
Metrika cards, offers utility functions for handling playing cards,
streamlining the development of educational Shiny applications related
to games of chance.

## Installation

To install the development version of ‘mmcards’, you can use the
[devtools](https://devtools.r-lib.org/) package:

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/mmcards")
```

## Who Has the Highest Card: A Basic Example

This section demonstrates a simple game called “Who Has the Highest
Card,” which shows you how to use mmcards to create a shuffled standard
deck and deal cards to players.

``` r
library(mmcards)

# Define the Players
p1 <- NULL
p2 <- NULL

# Create a shuffled standard deck of 52 cards using a random number seed for reproducibility
game_deck <- shuffle_deck(seed = 147)

# Deal a card to each player
game_deck <- deal_card(game_deck)
p1 <- game_deck$dealt_card

game_deck <- deal_card(game_deck)
p2 <- game_deck$dealt_card

# Display the cards and determine the winner
paste("p1 has:", p1$card, "with a value of", p1$value)
#> [1] "p1 has: 5C with a value of 5"
paste("p2 has:", p2$card, "with a value of", p2$value)
#> [1] "p2 has: KH with a value of 13.5"
paste0("The winner is: ", ifelse(p1$value > p2$value, "p1", "p2"))
#> [1] "The winner is: p2"
```

## Alternative Decks

To accommodate various use-cases, shuffle_deck() allows users to create
specialized decks like AnonymousDeck, InterleavedDeck, or PairedDeck.
You can learn more about these decks in the “alternative-decks”
vignette.

## Adding Card Images

Use the i_deck() function to add images to your card decks. You’ll need
to have the images stored on your computer.

The following example was used to create a Shiny application which flips
through a deck of shuffled cards showing the card image and the card
value.

In this example, my working directory was set to
“C:/Users/Administrator/Desktop/mmcards” and a deck of PNG
[vector-playing-cards](https://code.google.com/archive/p/vector-playing-cards/downloads)
was stored at
“C:/Users/Administrator/Desktop/PNG-cards-1.3/PNG-cards-1.3”.

``` r
#install.packages("shiny")
library(shiny)

    # Initialize a shuffled and image-embedded deck
    ic <- i_deck(deck = shuffle_deck(seed = 42),
                 i_path = paste0(gsub("mmcards", "", getwd()),
                                 "PNG-cards-1.3/PNG-cards-1.3"))

    # UI
    ui <- fluidPage(
      titlePanel("Card Deck Shiny App"),

      fluidRow(
        column(6,
               # Image output
               imageOutput("cardImage", height = "auto")
        ),
        column(6,
               # Text output
               textOutput("cardValue"),

               # Deal card button
               actionButton("deal_card", "Deal Card")
        )
      )
    )

    # Server logic
    server <- function(input, output, session) {

      # Initialize reactive variable to store current deck
      current_deck <- reactiveVal(ic)

      # Output for card image
      output$cardImage <- renderImage({
        # Initialize or get the current deck
        deck <- current_deck()

        # Return image path
        list(src = deck$icard[1],
             width = 250,
             height = 350)
      }, deleteFile = FALSE)

      # Output for card value
      output$cardValue <- renderText({
        # Initialize or get the current deck
        deck <- current_deck()

        # Return card value
        return(deck$value[1])
      })

      # Deal card logic
      observeEvent(input$deal_card, {
        # Get the current deck
        deck <- current_deck()

        # Deal a card
        new_deck <- deal_card(deck)

        # Update the current deck
        current_deck(new_deck$updated_deck)
      })
    }

    # Run the app
    shinyApp(ui, server)
```
