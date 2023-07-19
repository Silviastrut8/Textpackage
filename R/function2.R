#' Generate a word cloud visualization
#'
#' This function generates a word cloud visualization based on the frequency of words in a given text document.
#'
#' @param text A character vector or a file path to the text document.
#' @param max_words The maximum number of words to include in the word cloud (default: 100).
#' @param stopwords A character vector of words to exclude from the word cloud (default: NULL).
#'
#' @examples
#' # Generate a word cloud from a character vector
#' text <- c("This is a sample text.", "It contains multiple sentences.")
#' word_cloud(text)
#'
#' # Generate a word cloud from a text file with custom options
#' file_path <- "path/to/text_file.txt"
#' stopwords <- c("is", "a", "it", "multiple")
#' word_cloud(file_path, max_words = 50, stopwords = stopwords)
#'
#' @import wordcloud
#' @importFrom tm Corpus
#' @importFrom tm DocumentTermMatrix
#' @importFrom tm removeWords
#' @importFrom tm stopwords
#' @importFrom RColorBrewer brewer.pal
#'
#' @export
word_cloud <- function(text, max_words = 100, stopwords = NULL) {
  if (is.character(text)) {
    corpus <- Corpus(VectorSource(text))
  } else if (is.character(text) && file.exists(text)) {
    corpus <- Corpus(DirSource(dirname(text)), pattern = basename(text))
  } else {
    stop("Invalid input. Please provide a character vector or a valid file path.")
  }

  # Preprocess text
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords)

  # Create document-term matrix
  dtm <- DocumentTermMatrix(corpus)

  # Get word frequencies
  freq <- rowSums(as.matrix(dtm))

  # Order words by frequency
  freq_ordered <- order(freq, decreasing = TRUE)

  # Select top words
  top_words <- names(freq_ordered)[seq_len(min(max_words, length(freq_ordered)))]

  # Generate word cloud
  wordcloud(top_words, freq[freq_ordered], scale = c(5, 0.5), random.order = FALSE, colors = brewer.pal(8, "Dark2"))
}
