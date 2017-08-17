grep_bnf_code <- function(bnf_code, field) {
    grepl(paste("^", bnf_code, ".*", sep = ""), field)
}

analyse_medicine <- function(x, bnf_code_a, bnf_code_b){
  
  df <- x %>% group_by(practice_id,date) %>%
    summarise(type_a = sum(total_items[which(grep_bnf_code(bnf_code_a, bnf_code))]),
              type_b = sum(total_items[which(grep_bnf_code(bnf_code_b, bnf_code))]),
              type_a_spend = sum(actual_cost[which(grep_bnf_code(bnf_code_a, bnf_code))]),
              type_b_spend = sum(actual_cost[which(grep_bnf_code(bnf_code_b, bnf_code))])) %>%
    mutate(ratio = round(type_a / type_b *
                             100, 3)) %>% arrange(date)
  
  colnames(df)[3:6] <- c(paste(bnf_code_a, "_items", sep=""), paste(bnf_code_b, "_items", sep=""),
                         paste(bnf_code_a, "_spend", sep=""), paste(bnf_code_b, "_spend", sep=""))
  
  df
}

##analysis <- merge(analysis, practices[c(1:3, 7)], by.x = "practice",
  ##  by.y = "PracNo")
