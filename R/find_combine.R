datapackage <- function(){
  fromJSON("https://www.opendatani.gov.uk/api/3/action/package_show?id=a7b76920-bc0a-48fd-9abf-dc5ad0999886")
}

resources <- function(){
  datapackage()$result$resources
}

rx_metadata <- function() {

    return(datapackage()$result)
}

rx_months <- function(month_range = NULL) {

    if (is.null(month_range)) {
        df <- data.frame(dataset.month = resources()$name,
                         published.date = as.Date(resources()$created),
                         row.names = 1:length(resources()$name))
        df
    } else {
        df <- data.frame(dataset.month = resources()$name[month_range],
                         published.date = as.Date(resources()$created[month_range]),
                         row.names = month_range)
        df
    }
}

import_rx <- function(month_range = 1:1) {

    new_cols <- as.character(c("practice_id", "year",
        "month", "vtm_nm", "vmp_nm", "amp_nm", "presentation",
        "strength", "total_items", "total_quantity",
        "gross_cost", "actual_cost", "bnf_code", "bnf_chapter",
        "bnf_section", "bnf_paragraph", "bnf_subparagraph"))

    combined <- c()
    combined <- data.table(combined)

    print(paste("Downloading and combining files between",
        resources()$name[max(month_range)], "and",
        resources()$name[min(month_range)]))

    ## Download the files defined in range. Then, we can
    ## stitch the files together (it doesn't matter on
    ## the order, the month number is a field that can
    ## be used to sort later):
    for (i in month_range) {
        temp <- tempfile()
        download.file(resources()$url[i], temp, method = "libcurl")
        print(paste(resources()$name[i], "downloaded"))

        temp_dataset <- read_csv(temp, col_names = TRUE,
            col_types = cols("i","i","i","c","c","c","c","c","i","i","d","d","c","i","i","i","i","c","c"),
            na = c("-", "NA", ""))

        if (length(temp_dataset) > 17) {
            temp_dataset <- temp_dataset[1:17]
        }

        temp_dataset <- temp_dataset[rowSums(is.na(temp_dataset)) !=
            length(temp_dataset), ]

        colnames(temp_dataset) <- new_cols
        combined <- bind_rows(combined, temp_dataset)
        rm(temp_dataset)
    }

    colnames(combined) <- tolower(colnames(combined))

    ## Now, let's sort by the medicine type using the
    ## British National Formulary (BNF) coding system,
    ## then month:
    attach(combined)

    combined <- combined[order(combined$bnf_chapter,
        combined$bnf_section, combined$bnf_paragraph,
        combined$bnf_subparagraph, combined$bnf_code,
        -combined$year, -combined$month), ]

    detach(combined)

    print(paste("Combined monthly prescribing datasets between start",
        format(as.Date(paste(min(combined$year, na.rm = TRUE),
            min(combined$month, na.rm = TRUE), "01",
            sep = "-")), "%B %Y"), "and end",
        format(as.Date(paste(max(combined$year,
            na.rm = TRUE), max(combined$month, na.rm = TRUE),
            "01", sep = "-")), "%B %Y"), "with",
        prettyNum(nrow(combined), big.mark=","), "rows."))

    combined <- combined %>% mutate(
      date = ymd(paste(year, month, 1))
      )
    
    combined
}
