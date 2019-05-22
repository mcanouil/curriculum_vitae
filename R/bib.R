clean_field <- function(pattern, x) {
  gsub(
    pattern = paste0("^", pattern, " = "), 
    replacement = "", 
    x = gsub(
      pattern = ",$", 
      replacement = "", 
      x = gsub(
        pattern = "[{}]", 
        replacement = "", 
        x = grep(paste0("^", pattern), x, value = TRUE)
     )
    )
  )
}

read_article <- function(.x) {
  authors <- do.call("rbind", strsplit(unlist(strsplit(clean_field("author", .x), " and ")), ", "))
  authors <- gsub(" ", "&nbsp;", paste(authors[, 2], authors[, 1]))
  authors <- paste(paste(authors[-length(authors)], collapse = ", "), authors[length(authors)], sep = " and ")
  data.frame(
    title = clean_field("title", .x),
    month = gsub("May.", "May", paste0(Hmisc::capitalize(clean_field("month", .x)), ".")),
    year = clean_field("year", .x),
    doi = clean_field("doi", .x),
    authors = authors, 
    journal = clean_field("journal", .x),
    first = clean_field("first", .x),
    stringsAsFactors = FALSE
  )
}

read_bib <- function(path) {
  big_file <- paste(readLines(path), collapse = "")
  big_file <- unlist(strsplit(x = big_file, split = "@", fixed = TRUE))
  big_file <- big_file[nchar(big_file)!=0]
  
  all_bib <- lapply(strsplit(x = big_file, split = "\t"), read_article)
  all_bib <- do.call("rbind.data.frame", all_bib)
  all_bib[["month"]] <- factor(
    x = all_bib[["month"]],
    levels = gsub("May.", "May", paste0(c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    ), "."))
  )
  all_bib[["doi"]] <- ifelse(
    test = grepl("^http", all_bib[["doi"]]), 
    yes = all_bib[["doi"]], 
    no = paste0("https://www.doi.org/", all_bib[["doi"]])
  )
  
  all_bib[order(all_bib[["year"]], all_bib[["month"]], decreasing = TRUE), ]
}

format_author <- function(bib, author, max) {
  split_authors <- unlist(strsplit(strsplit(bib[["authors"]], ", ")[[1]], " and "))
  split_authors <- gsub(
    pattern = author, 
    replacement = paste0("<u>", author, "</u>", if (bib[["first"]]) "<sup>†</sup>" else ""), 
    x = split_authors
  )
  pos_author <- grep(author, split_authors)
  if (length(split_authors) > max) {
    if (pos_author <= max) {
      cat(
        paste(split_authors[1:max], collapse = ", "), 
        "*et&nbsp;al.*"
      )
    } else {
      cat(
        paste(c(split_authors[1:(max - 1)], "*...*, "), collapse = ", "), 
        paste0(grep(pattern = author, x = split_authors, value = TRUE), "<sup>", pos_author, "</sup>"),
        "*et&nbsp;al.*"
      )
    }
  } else {
    cat(
      paste(split_authors[-length(split_authors)], collapse = ", "), 
      paste0(split_authors[length(split_authors)], "."), 
      sep = " and "
    )
  }
}

format_bib_resume <- function(bib, author, max = 10) {
  author <- gsub(" ", "&nbsp;", author)
  for (ipub in seq_len(nrow(bib))) {
    cat("\n")
    cat("###", bib[ipub, "title"], "\n\n")
    cat("\n")
    format_author(bib = bib[ipub, ], author = author, max = max)
    cat("\n")
    cat("\n")
    cat("N/A\n\n")
    cat("\n")
    cat(paste(bib[ipub, "month"], bib[ipub, "year"]), "\n\n")
    cat("\n")
    cat("::: aside\n")
    cat("*[", bib[ipub, "journal"],"](",  bib[ipub, "doi"], ")*\n", sep = "")
    if (bib[["first"]][ipub]) {
      cat("\n")
      cat("<sup>†</sup> As first or co-first author.\n")
      cat("\n")
    }
    cat(":::\n")
    cat("\n")
  }
}

format_bib_website <- function(bib, author, max = 25) {
  author <- gsub(" ", "&nbsp;", author)
  for (ipub in seq_len(nrow(bib))) {
    cat("\n")
    cat("###", paste0("[", (nrow(bib) + 1) - ipub, "]"), bib[ipub, "title"], "\n\n")
    cat("\n")
    cat("*[", bib[ipub, "journal"],"](",  bib[ipub, "doi"], ")* (", paste0(bib[ipub, "month"], " ", bib[ipub, "year"]), ")\n\n", sep = "")
    cat("\n")
    format_author(bib = bib[ipub, ], author = author, max = max)
    cat("\n")
    if (bib[["first"]][ipub]) {
      cat("\n")
      cat('<p style="font-size: 75%;"><sup>†</sup> As first or co-first author.</p>\n')
      cat("\n")
    }
    cat("\n")
  }
}
