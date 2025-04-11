library(tidyverse)

analyze_text_file <- function(filename = "hw2_data.txt") {
  tryCatch({
    lines <- readLines(filename)
  }, error = function(e) {
    stop(paste("Error: File '", filename, "' not found.", sep=""))
  })
  
  # 確保至少有一行，並檢查最後一行是否有換行符
  if (length(lines) > 0) {
    last_line <- lines[length(lines)]
    if (!grepl("(\\r?\\n)$", last_line)) {
      lines[length(lines)] <- paste0(last_line, "\n")
    }
  }
  
  words <- lines
  
  # 使用 named list 統計字詞頻率，達到類似Hash/Dictionary的效果
  word_counts <- list()
  for (word in words) {
    if (word %in% names(word_counts)) {
      word_counts[[word]] <- word_counts[[word]] + 1
    } else {
      word_counts[[word]] <- 1
    }
  }
  
  # 2. 顯示不重複字詞總數
  unique_word_count <- length(word_counts)
  print(paste("Total number of unique words:", unique_word_count))
  
  # 3. 顯示每個字詞出現次數
  print("\nWord Counts:")
  for (word in names(word_counts)) {
    print(paste(word, ":", word_counts[[word]]))
  }
  
  # 將 named list 轉換為 data.frame 以便繪圖
  word_counts_df <- data.frame(Word = names(word_counts), Count = unlist(word_counts), stringsAsFactors = FALSE)
  word_counts_df <- word_counts_df %>%
    arrange(desc(Count))
  
  # 4. 繪製直方圖
  ggplot(word_counts_df, aes(x = Word, y = Count)) +
    geom_bar(stat = "identity") +
    labs(title = "Word Frequency Histogram", x = "Words", y = "Frequency") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# 執行分析
analyze_text_file()