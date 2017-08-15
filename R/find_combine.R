library(jsonlite, quietly = TRUE)
library(plyr, quietly = TRUE)
library(readr, quietly = TRUE)
library(data.table, quietly = TRUE)

## Use the OpenDataNI package API for the GP
## prescribing dataset (providing the id doesn't
## change)
datapackage <- fromJSON("https://www.opendatani.gov.uk/api/3/action/package_show?id=a7b76920-bc0a-48fd-9abf-dc5ad0999886")
resources <- datapackage$result$resources

rx_metadata <- function() {
    
    return(datapackage$result)
}

rx_months <- function(month_range = NULL) {
    
    if (is.null(month_range)) {
      df <- data.frame(dataset.month = resources$name, row.names = 1:length(resources$name))
      df
    } else {
      df <- data.frame(dataset.month = resources$name[month_range], row.names = month_range)
      df
    }
}

find_combine <- function(range = 1:1) {
    
    newCols <- as.character(c("practice", "year", "month", 
        "vtm_nm", "vmp_nm", "amp_nm", "presentation", 
        "strength", "total_items", "total_quantity", 
        "gross_cost", "actual_cost", "bnf_code", "bnf_chapter", 
        "bnf_section", "bnf_paragraph", "bnf_subparagraph"))
    
    ## Create a list of the files that we actually want
    ## (the range)
    filenames <- c()
    for (i in range) {
        name <- paste(resources$name[i], ".csv", sep = "")
        filenames <- append(filenames, as.character(name))
    }
    
    combined <- c()
    combined <- data.table(combined)
    
    print(paste("Downloading and combining files between", 
        resources$name[max(range)], "and", resources$name[min(range)]))
    
    ## Download the actual files Then, we can stitch the
    ## files together (it doesn't matter on the order,
    ## the month number is a field that can be used to
    ## sort later):
    
    for (i in range) {
        temp <- tempfile()
        download.file(resources$url[i], temp, method = "libcurl")
        print(paste("Now reading:", resources$name[i]))
        temp_dataset <- read_csv(temp, col_names = TRUE, 
            na = c("-", "NA", ""))
        if (length(temp_dataset) > 17) {
            temp_dataset <- temp_dataset[1:17]
        }
        colnames(temp_dataset) <- newCols
        combined <- rbind.fill(combined, temp_dataset)
        rm(temp_dataset)
    }
    
    colnames(combined) <- tolower(colnames(combined))
    
    ## Now, let's sort by the medicine type using the
    ## British National Formulary (BNF) coding system,
    ## then month:
    attach(combined)
    combined <- combined[order(bnf_chapter, bnf_section, 
        bnf_paragraph, bnf_subparagraph, bnf_code, 
        -year, -month), ]
    detach(combined)
    
    prescriptions <<- combined
}
