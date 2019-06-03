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
    first = if (any(grepl("annote", .x))) {
      grepl("first", clean_field("annote", .x))
    } else {
      FALSE
    },
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

format_bib_author <- function(bib, author, max) {
  split_authors <- unlist(strsplit(strsplit(bib[["authors"]], ", ")[[1]], " and "))
  split_authors <- gsub(
    pattern = author, 
    replacement = paste0("<u>", author, "</u>", if (bib[["first"]]) "<sup>†</sup>" else ""), 
    x = split_authors
  )
  pos_author <- grep(author, split_authors)
  if (length(split_authors) > max) {
    if (pos_author <= max) {
      paste(
        paste(split_authors[1:max], collapse = ", "), 
        "*et&nbsp;al.*"
      )
    } else {
      paste(
        paste(c(split_authors[1:(max - 1)], "*...*, "), collapse = ", "), 
        paste0(grep(pattern = author, x = split_authors, value = TRUE), "<sup>", pos_author, "</sup>"),
        "*et&nbsp;al.*"
      )
    }
  } else {
    paste(
      paste(split_authors[-length(split_authors)], collapse = ", "), 
      split_authors[length(split_authors)], 
      sep = " and "
    )
  }
}

format_bib_resume <- function(bib, author, max = 10) {
  author <- gsub(" ", "&nbsp;", author)
  for (ipub in seq_len(nrow(bib))) {
    cat("###", bib[ipub, "title"], "\n")
    cat("\n")
    cat(format_bib_author(bib = bib[ipub, ], author = author, max = max), "\n")
    cat("\n")
    cat("N/A\n")
    cat("\n")
    cat(paste(bib[ipub, "month"], bib[ipub, "year"]), "\n")
    cat("\n")
    cat("::: aside\n")
    cat("*[", bib[ipub, "journal"],"](",  bib[ipub, "doi"], ")*\n", sep = "")
    if (bib[["first"]][ipub]) {
      cat("\n")
      cat('<p style="font-size: 75%;"><sup>†</sup> As first or co-first author.</p>')
      cat("\n")
    }
    cat(":::\n")
    cat("\n")
  }
  
  invisible()
}

format_bib_website <- function(bib, author, max = 25) {
  author <- gsub(" ", "&nbsp;", author)
  for (ipub in seq_len(nrow(bib))) {
    cat("###", paste0("[", (nrow(bib) + 1) - ipub, "]"), bib[ipub, "title"], "\n")
    cat("\n")
    month_names <- c(
      "January", "February", "March", "April", "May", "June", 
      "July", "August", "September", "October", "November", "December"
    )
    names(month_names) <- gsub("May.", "May", paste0(c(
      "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
    ), "."))
    cat(
      "*[", bib[ipub, "journal"],"](",  bib[ipub, "doi"], ")*",
      " *(", paste0(month_names[bib[ipub, "month"]], " ", bib[ipub, "year"]), ")*\n", 
      sep = ""
    )
    cat("\n")
    cat(format_bib_author(bib = bib[ipub, ], author = author, max = max))
    cat("\n")
    if (bib[["first"]][ipub]) {
      cat("\n")
      cat('<p style="font-size: 75%;"><sup>†</sup> As first or co-first author.</p>\n')
      cat("\n")
    }
    cat("\n")
  }
  
  invisible()
}

format_bib <- function(bib, author, max = 25, output = NULL) {
  switch(
    EXPR = output,
    "resume" = format_bib_resume(bib = bib, author = author, max = max),
    "website" = format_bib_website(bib = bib, author = author, max = max)
  )
  
  invisible()
}

format_package_author <- function(package, author, max = 55) {
  split_authors <- unlist(strsplit(strsplit(package[["authors"]], ", ")[[1]], " and "))
  split_authors <- gsub(
    pattern = author, 
    replacement = paste0("<u>", author, "</u>"), 
    x = split_authors
  )
  split_authors <- gsub(" ", "&nbsp;", split_authors)
  authors <- paste(
    paste(split_authors[-length(split_authors)], collapse = ", "), 
    split_authors[length(split_authors)], 
    sep = " and "
  )
  max <- max + length(gregexpr("&nbsp;", authors)[[1]]) * 5
  if (nchar(authors) > max) {
    regmatches(
      x = authors, 
      m = structure(
        gregexpr(" ", authors)[[1]][max(which(gregexpr(" ", authors)[[1]] < max))], 
        match.length = 1L
      )
    ) <- "  \n"
    
  }
  
  authors
}

format_packages_resume <- function(package, author, max) {
  for (irow in seq_len(nrow(package))) {
    ipackage <- package[irow, ]
    cat("### ", ipackage[["name"]], ": ", ipackage[["title"]], "\n", sep = "")
    cat("\n")
    cat(format_package_author(package = ipackage, author = author, max = max), "\n")
    cat("\n")
    cat(ipackage[["where"]], "\n")
    cat("\n")
    clean_since <- gsub(
      pattern = "May.", 
      replacement = "May", 
      x = format(
        as.Date(paste0(ipackage[["since"]], "-01"), format = "%Y-%m-%d"), 
        format = "%b. %Y"
      )
    )
    cat(clean_since, "\n")
    cat("\n")
    switch(
      EXPR = ipackage[["where"]],
      "GitHub" = {
        cat(
          "[https://github.com/", ipackage[["user"]], "/", ipackage[["name"]], "/]",
          "(https://github.com/", ipackage[["user"]], "/", ipackage[["name"]], "/)\n", 
          sep = ""
        )
        cat("\n")
        cat("::: aside\n")
        cat(
          "[![GitHub_tag](https://img.shields.io/github/tag/mcanouil/", ipackage[["name"]], ".svg?label=Github&color=4169e1)]",
          "(https://github.com/", ipackage[["user"]], "/", ipackage[["name"]], "/)\n",
          sep = ""
        )
        cat(":::\n")
      },
      "CRAN" = {
        cat(
          "[https://cran.r-project.org/package=", ipackage[["name"]], "]",
          "(https://cran.r-project.org/package=", ipackage[["name"]], ")\n", 
          sep = ""
        )
        cat("\n")
        cat("::: aside\n")
        cat(
          "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/", ipackage[["name"]], "?color=4169e1)]",
          "(https://cran.r-project.org/package=", ipackage[["name"]], ")\n", 
          sep = ""
        )
        cat(":::\n")
      }
    )
    cat("\n")
  }

  invisible()
}

format_packages_website <- function(package, author, max) {
  for (irow in seq_len(nrow(package))) {
    ipackage <- package[irow, ]
    cat("### ", ipackage[["name"]], ": ", ipackage[["title"]], "\n", sep = "")
    cat("\n")
    switch(
      EXPR = ipackage[["where"]],
      "GitHub" = {
        cat(
          "[![GitHub_tag](https://img.shields.io/github/tag/mcanouil/", ipackage[["name"]], ".svg?label=Github&color=4169e1)]",
          "(https://github.com/", ipackage[["user"]], "/", ipackage[["name"]], "/)\n",
          sep = ""
        )
      },
      "CRAN" = {
        cat(
          "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/", ipackage[["name"]], "?color=4169e1)]",
          "(https://cran.r-project.org/package=", ipackage[["name"]], ")\n", 
          sep = ""
        )
      }
    )
    cat("\n")
    cat(format_package_author(package = ipackage, author = author, max = max), "\n")
    cat("\n")
    clean_since <- format(
      as.Date(paste0(ipackage[["since"]], "-01"), format = "%Y-%m-%d"), 
      format = "%B %Y"
    )
    switch(
      EXPR = ipackage[["where"]],
      "GitHub" = {
        cat(
          "Created on ", clean_since,
          " and available at ",
          "[https://github.com/", ipackage[["user"]], "/", ipackage[["name"]], "/]",
          "(https://github.com/", ipackage[["user"]], "/", ipackage[["name"]], "/)\n", 
          sep = ""
        )
      },
      "CRAN" = {
        cat(
          "Created on ", clean_since,
          " and available at ",
          "[https://cran.r-project.org/package=", ipackage[["name"]], "]",
          "(https://cran.r-project.org/package=", ipackage[["name"]], ")\n", 
          sep = ""
        )
      }
    )
    cat("\n")
  }
  
  invisible()
}

format_packages <- function(package, author, max, output = NULL) {
  switch(
    EXPR = output,
    "resume" = format_packages_resume(package = package, author = author, max = max),
    "website" = format_packages_website(package = package, author = author, max = max)
  )

  invisible()
}
