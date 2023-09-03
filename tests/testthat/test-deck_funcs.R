test_that("standard_deck works with defaults", {
  deck <- standard_deck()

  expect_equal(nrow(deck), 52)
  expect_s3_class(deck, "data.frame")
  expect_s3_class(deck, "deck")
})

test_that("shuffle_deck works with standard_deck", {
  #Get deck
  sdeck <- shuffle_deck(seed = 100)

  # Test expectations
  expect_equal(nrow(sdeck), 52)
  expect_s3_class(sdeck, "data.frame")
  expect_s3_class(sdeck, "shuffled deck")
  expect_s3_class(sdeck, "deck")
})

test_that("shuffle_deck works with anonymous function", {

  #Get deck
  sdeck <- shuffle_deck(deck_of_cards = function(x){stats::runif(52, 1, 52)},
                        seed = 100)

  # Test expectations
  expect_equal(nrow(sdeck), 52)
  expect_s3_class(sdeck, "data.frame")
  expect_s3_class(sdeck, "shuffled deck")
  expect_s3_class(sdeck, "anonymous deck")
})

test_that("shuffle_deck works for interleaved decks", {

  #Get deck
  sdeck <- shuffle_deck(deck_of_cards = function(x) {list(stats::rnorm(26, 1, 2),
                                                          stats::rnorm(26, 1.5, 1.5))},
                        seed = 100)

  # Test expectations
  expect_equal(nrow(sdeck), 52)
  expect_s3_class(sdeck, "data.frame")
  expect_s3_class(sdeck, "shuffled deck")
  expect_s3_class(sdeck, "interleaved deck")
})

test_that("shuffle_deck works for interleaved decks with paired = TRUE", {

  #Get deck
  sdeck <- shuffle_deck(deck_of_cards = function(x) {list(c(1:26),
                                                          c(1:26))},
                        seed = 100,
                        paired = TRUE)

  # Test expectations
  expect_equal(nrow(sdeck), 52)
  expect_s3_class(sdeck, "data.frame")
  expect_s3_class(sdeck, "shuffled deck")
  expect_s3_class(sdeck, "interleaved deck")

  # Test that pairing worked
  expect_equal(sdeck$value[[1]],sdeck$value[[2]])
  expect_equal(sdeck$value[[11]],sdeck$value[[12]])
  expect_equal(sdeck$value[[21]],sdeck$value[[22]])
  expect_equal(sdeck$value[[31]],sdeck$value[[32]])
  expect_equal(sdeck$value[[41]],sdeck$value[[42]])
  expect_equal(sdeck$value[[51]],sdeck$value[[52]])
})

test_that("deal_card works with standard deck", {

  # Get deck
  sdeck <- shuffle_deck(seed = 100)

  # Deal one card
  udeck <- deal_card(sdeck)

  # Test expectations
  expect_equal(nrow(udeck$dealt_card), 1)
  expect_equal(nrow(udeck$updated_deck), 51)
  expect_s3_class(udeck, "up_deck")

  # Deal second card
  udeck <- deal_card(udeck$updated_deck)

  # Test expectations
  expect_equal(nrow(udeck$dealt_card), 1)
  expect_equal(nrow(udeck$updated_deck), 50)
  expect_s3_class(udeck, "up_deck")

})

test_that("deal_card works with standard deck and up_deck class", {

  # Get deck
  sdeck <- shuffle_deck(seed = 100)

  # Deal one card
  udeck <- deal_card(sdeck)

  # Deal second card
  udeck <- deal_card(udeck)

  # Test expectations
  expect_equal(nrow(udeck$dealt_card), 1)
  expect_equal(nrow(udeck$updated_deck), 50)
  expect_s3_class(udeck, "up_deck")

})

