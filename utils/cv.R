`%>%` <- magrittr::`%>%`

fa <- fontawesome::fa

main_colour <- "#333333"

add_github_logo <- function(url) {
  sub("[GitHub]", paste0('[', fa("github", fill = main_colour), ' GitHub]'), gsub(
    pattern = "(.*)https://github.com/(.*)", 
    replacement = glue::glue('\\1[GitHub](https://github.com/\\2)'), 
    x = url
  ), fixed = TRUE) 
}

profil_section <- function(xlsx = "data/cv.xlsx", sheet = "profil") {
  readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::filter(show == 1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    dplyr::mutate(
      level = purrr::map_chr(
        .x = level, 
        .f = ~ paste(rep("#", each = as.numeric(.x) + 2), collapse = "")
      )
    ) %>% 
    glue::glue_data('{level} {title}\n\n{paragraph}\n\n')
}

contact_section <- function(xlsx = "data/cv.xlsx", sheet = "contact") {
  readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(
      '## Contact Info {{#contact}}',
      '\n\n',
      '- {fa("user", fill = main_colour)} {position}',
      '\n',
      '- {fa("university", fill = main_colour)} {institute}',
      '\n',
      '- {fa("map-marker", fill = main_colour)} {city}',
      '\n',
      '- {fa("envelope", fill = main_colour)} [{gsub("@", " [at] ", email)}](mailto:{email})',
      '\n',
      '- {fa("phone", fill = main_colour)} {phone}',
      '\n',
      '- {fa("orcid", fill = main_colour)} [{orcid}](https://orcid.org/{orcid})',
      '\n',
      '- {fa("linkedin", fill = main_colour)} [{linkedin}](https://www.linkedin.com/in/{linkedin})',
      '\n',
      '- {fa("github", fill = main_colour)} [{github}](https://github.com/{github})',
      '\n',
      '- {fa("twitter", fill = main_colour)} [{twitter}](https://twitter.com/{twitter})',
      '\n',
      '- {fa("r-project", fill = main_colour)} {rgroup}',
      '\n\n'
    )
}

skills_section <- function(xlsx = "data/cv.xlsx", sheet = "skills") {
  readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    dplyr::group_by(level) %>% 
    dplyr::summarise(
      what = as.character(glue::glue_collapse(what, sep = ", ", last = " and ")), 
      .groups = "drop"
    ) %>% 
    tidyr::pivot_wider(names_from = level, values_from = what) %>% 
    glue::glue_data(
      '## Computer Skills {{#skills}}',
      "\n\n",
      '- <u style="color: var(--main-color);">*Advanced:*</u> {advanced}',
      '\n',
      '- <u style="color: var(--main-color);">*Intermediate:*</u> {intermediate}',
      '\n',
      '- <u style="color: var(--main-color);">*Basic:*</u> {basic}',
      '\n\n'
    )
}

disclaimer_section <- function(text = NULL) {
  glue::glue(
    '## Disclaimer {{#disclaimer}}',
    if (is.null(text)) '\n\n' else '\n\n{text}\n\n',
    'Last updated on {Sys.Date()}.\n\n'
  )
}

sidebar <- function(
  png = "pictures/cv.png", 
  contact = contact_section(), 
  skills = skills_section(), 
  disclaimer = disclaimer_section()
) {
  cat(
    '# Aside\n',
    '```{{r, out.extra = \'style="width=226px;" id="picture"\'}}',
    'knitr::include_graphics({png})',
     '```',
    contact,
    skills, 
    disclaimer,
    sep = "\n\n"
  )
}

title_section <- function(author = NULL) {
  c(
    "# Main",
    glue::glue('## {author} {{#title}}')
  )
}

education_section <- function(xlsx = "data/cv.xlsx", sheet = "education", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n',
      '### {degree}',
      '{university}',
      '{city}',
      '{start} - {end}',
      '{description}',
      '\n\n'
    )
  
  if (page_break_after) {
    c("## Education {data-icon=graduation-cap data-concise=true .break-after-me}", text)
  } else {
    c("## Education {data-icon=graduation-cap data-concise=true}", text)
  }
}

experience_section <- function(xlsx = "data/cv.xlsx", sheet = "experience", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n',
      '### {position}',
      '{institute}',
      '{city}',
      '{start} - {end}',
      'Activities: *{activities}*',
      '\n\n'
    )
  
  if (page_break_after) {
    c("## Professional Experience {data-icon=laptop .break-after-me}", text)
  } else {
    c("## Professional Experience {data-icon=laptop}", text)
  }
}

workshop_section <- function(xlsx = "data/cv.xlsx", sheet = "workshop", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n', 
      '### {title}',
      '{type}',
      '{city}',
      '{date}',
      '::: aside\n{add_github_logo(url)}\n:::',
      '\n\n'
    )
  
  if (page_break_after) {
    c(glue::glue("## Workshop Experience ({length(text)}) {{data-icon=chalkboard-teacher .break-after-me}}"), text)
  } else {
    c(glue::glue("## Workshop Experience ({length(text)}) {{data-icon=chalkboard-teacher}}"), text)
  }
}

packages_section <- function(xlsx = "data/cv.xlsx", sheet = "packages", author = NULL, page_break_after = FALSE) {
  format_package_author <- function(authors, author, max = 57) {
    purrr::map(authors, function(iauthors) {
      split_authors <- unlist(strsplit(strsplit(iauthors, ", ")[[1]], " and "))
      split_authors <- gsub(
        pattern = author, 
        replacement = paste0("<u>", author, "</u>"), 
        x = split_authors
      )
      split_authors <- gsub(" ", "&nbsp;", split_authors)
      list_authors <- paste(
        paste(split_authors[-length(split_authors)], collapse = ", "), 
        split_authors[length(split_authors)], 
        sep = " and "
      )
      max <- max + length(gregexpr("&nbsp;", list_authors)[[1]]) * 5
      if (nchar(list_authors) > max) {
        regmatches(
          x = list_authors, 
          m = structure(
            gregexpr(" ", list_authors)[[1]][max(which(gregexpr(" ", list_authors)[[1]] < max))], 
            match.length = 1L
          )
        ) <- "  \n"
      }
      
      list_authors
    })
  }
  
  format_package_date <- function(date) {
    purrr::map(date, function(idate) {
      gsub(
        pattern = "May.", 
        replacement = "May", 
        x = format(
          as.Date(paste0(idate, "-01"), format = "%Y-%m-%d"), 
          format = "%b. %Y"
        )
      )
    })
  }
  
  format_package_url <- function(repo_user, repo_name, where) {
    purrr::pmap(
      .l = list(repo_user, repo_name, where), 
      .f =  function(repo_user, repo_name, where) {
        mc <- sub("#", "", main_colour)
        switch(
          EXPR = where,
          "GitHub" = {
            paste0(
              "[https://github.com/", repo_user, "/", repo_name, "/](https://github.com/", repo_user, "/", repo_name, "/)", 
              "\n\n",
              "::: aside",
              "\n",
              "[![GitHub_tag](https://img.shields.io/github/tag/", repo_user, "/", repo_name, ".svg?label=Github&color=", mc, ")](https://github.com/", repo_user, "/", repo_name, "/)",
              "\n",
              ":::",
              "\n"
            )
          },
          "CRAN" = {
            paste0(
              "[https://cran.r-project.org/package=", repo_name, "](https://cran.r-project.org/package=", repo_name, ")", 
              "\n\n",
              "::: aside",
              "\n",
              "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/", repo_name, "?color=", mc, ")](https://cran.r-project.org/package=", repo_name, ")",
              "\n",
              ":::",
              "\n"
            )
          },
          "BOTH" = {
            paste0(
              "[https://cran.r-project.org/package=", repo_name, "](https://cran.r-project.org/package=", repo_name, ")  ", 
              "\n",
              "[https://github.com/", repo_user, "/", repo_name, "/](https://github.com/", repo_user, "/", repo_name, "/)", 
              "\n\n",
              "::: aside",
              "\n",
              "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version-ago/", repo_name, "?color=", mc, ")](https://cran.r-project.org/package=", repo_name, ")  ", 
              "\n",
              "[![GitHub_tag](https://img.shields.io/github/tag/", repo_user,"/", repo_name, ".svg?label=Github&color=", mc, ")](https://github.com/", repo_user, "/", repo_name, "/)",
              "\n",
              ":::",
              "\n"
            )
          }
        )
      }
    )
  }
  
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n',
      '### {name}: {title}',
      # '{format_package_author(authors, author)}',
      '(As *{tolower(type)}*)',
      '{purrr::map(where, ~ switch(EXPR = .x, "GitHub" = "GitHub", "CRAN" = "CRAN", "BOTH" = "CRAN"))}',
      '{format_package_date(since)}',
      '{format_package_url(user, name, where)}'
    )
  

  
  if (page_break_after) {
    c(
      glue::glue("## R Packages ({length(text)}) {{data-icon=code .break-after-me}}"), 
      text
    )
  } else {
    c(
      glue::glue("## R Packages ({length(text)}) {{data-icon=code}}"), 
      text
    )
  }
}

awards_section <- function(xlsx = "data/cv.xlsx", sheet = "awards", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n', 
      '### {name}',
      '{institute}',
      '{city}',
      '{date}',
      '{description}',
      '::: aside\n{add_github_logo(url)}\n:::',
      '\n\n'
    )
  
  if (page_break_after) {
    c(glue::glue("## Awards ({length(text)}) {{data-icon=trophy .break-after-me}}"), text)
  } else {
    c(glue::glue("## Awards ({length(text)}) {{data-icon=trophy}}"), text)
  }
}

oral_section <- function(xlsx = "data/cv.xlsx", sheet = "oral", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n', 
      '### {title}',
      '{organiser}',
      '{city}',
      '{date}',
      '::: aside\n{add_github_logo(url)}\n:::',
      '\n\n'
    )

  if (page_break_after) {
    c(glue::glue("## Oral communications ({length(text)}) {{data-icon=comment-dots .break-after-me}}"), text)
  } else {
    c(glue::glue("## Oral communications ({length(text)}) {{data-icon=comment-dots}}"), text)
  }
}

poster_section <- function(xlsx = "data/cv.xlsx", sheet = "poster", page_break_after = FALSE) {
  text <- readxl::read_xlsx(xlsx, sheet) %>% 
    dplyr::slice(dplyr::n():1) %>% 
    dplyr::mutate_all(.funs = ~ tidyr::replace_na(.x, "")) %>% 
    glue::glue_data(.sep = '\n\n', 
      '### {title}',
      '{organiser}',
      '{city}',
      '{date}',
      '::: aside\n{add_github_logo(url)}\n:::',
      '\n\n'
    )

  if (page_break_after) {
    c(glue::glue('## Poster communications ({length(text)}) {{data-icon=file .break-after-me}}'), text)
  } else {
    c(glue::glue('## Poster communications ({length(text)}) {{data-icon=file}}'), text)
  }
}

articles_section <- function(bib = "data/cv.bib", author = NULL, page_break_after = FALSE) {
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
    authors <- apply(X = authors[, c(2, 1)], MARGIN = 1, FUN = function(irow) {
      gsub(" ", "&nbsp;", paste(unique(irow), collapse = " "))
    })
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
    big_file <- big_file[nchar(big_file) != 0]
    
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

  format_bib_author <- function(authors, first, author, max = 10) {
    purrr::pmap(list(authors, first), function(iauthors, ifirst) {
      split_authors <- unlist(strsplit(strsplit(iauthors, ", ")[[1]], " and "))
      split_authors <- gsub(
        pattern = author, 
        replacement = paste0("<u>", author, "</u>", if (ifirst) "<sup>&dagger;</sup>" else ""), 
        x = split_authors
      )
      pos_author <- grep(author, split_authors)
      if (length(split_authors) <= max) {
        paste(
          paste(split_authors[-length(split_authors)], collapse = ", "), 
          split_authors[length(split_authors)], 
          sep = " and "
        )
      } else {
        switch(
          EXPR = paste(abs(c(0, length(split_authors)) - pos_author) > ceiling(max / 2), collapse = "--"),
          "TRUE--TRUE" = {
            split_authors[pos_author] <- paste0(
              split_authors[pos_author], "<sup>", pos_author, "/", length(split_authors), "</sup>"
            )
            paste0(
              paste(
                c(
                  split_authors[1:ceiling((max - 1) / 2)],
                  "*[...]*",
                  split_authors[pos_author],
                  "*[...]*",
                  split_authors[(length(split_authors) - (max - 1 - ceiling((max - 1) / 2))):(length(split_authors) - 1)]
                ),
                collapse = ", "
              ),
              " and ", 
              split_authors[length(split_authors)]
            )
          },
          "TRUE--FALSE" = {
            split_authors[pos_author] <- paste0(
              split_authors[pos_author], "<sup>", pos_author, "/", length(split_authors), "</sup>"
            )
            paste0(
              paste(
                c(
                  split_authors[1:ceiling(max / 2)],
                  "*[...]*",
                  split_authors[(length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)]
                ),
                collapse = ", "
              ),
              " and ", 
              split_authors[length(split_authors)]
            )
          },
          "FALSE--TRUE" = {
            split_authors[pos_author] <- paste0(
              split_authors[pos_author], "<sup>", pos_author, "/", length(split_authors), "</sup>"
            )
            paste0(
              paste(
                c(
                  split_authors[1:ceiling(max / 2)],
                  "*[...]*",
                  split_authors[(length(split_authors) - (max - 1 - ceiling(max / 2))):(length(split_authors) - 1)]
                ),
                collapse = ", "
              ),
              " and ", 
              split_authors[length(split_authors)]
            )
          },
          "FALSE--FALSE" = {
            paste(
              paste(split_authors[-length(split_authors)], collapse = ", "), 
              split_authors[length(split_authors)], 
              sep = " and "
            )
          }
        )
      }
    })
  }
  
  author <- gsub(" ", "&nbsp;", author)
  text <- glue::glue_data(.x = read_bib(bib), .sep = '\n\n', 
      '### {title}',
      '{format_bib_author(authors, first, author)}',
      'N/A',
      '{month} {year}',
      '::: aside',
      '*[{journal}]({doi})*\n{ifelse(first, \'<p style="font-size: 75%;"><sup>&dagger;</sup> As first or co-first author.</p>\', \'\')}\n:::',
    )
  
  if (page_break_after) {
    c(glue::glue("## Publications ({length(text)}) {{data-icon=newspaper .break-after-me}}"), text)
  } else {
    c(glue::glue("## Publications ({length(text)}) {{data-icon=newspaper}}"), text)
  }
}
