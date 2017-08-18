grep_bnf_code <- function(bnf_code, field) {
    grepl(paste("^", bnf_code, ".*", sep = ""), field)
}

compare_bnf <- function(x, bnf_code_a, bnf_code_b){

  df <- x %>% group_by(practice_id, date) %>%
    summarise(type_a_items = sum(total_items[which(grep_bnf_code(bnf_code_a, bnf_code))]),
              type_b_items = sum(total_items[which(grep_bnf_code(bnf_code_b, bnf_code))]),
              type_a_spend = sum(actual_cost[which(grep_bnf_code(bnf_code_a, bnf_code))]),
              type_b_spend = sum(actual_cost[which(grep_bnf_code(bnf_code_b, bnf_code))])) %>%
    mutate(ratio = round(type_a_items / type_b_items *
                             100, 3)) %>% arrange(date)

#  colnames(df)[3:6] <- c(paste("items_", bnf_code_a, sep=""), paste("items_", bnf_code_b, sep=""),
#                         paste("spend_", bnf_code_a, sep=""), paste("spend_", bnf_code_b, sep=""))

  df
}

compare_rx <- function(x, bnf_code_a, bnf_code_b, name_a = NULL, name_b = NULL, spending = FALSE, plot = FALSE){

  df <- compare_bnf(x, bnf_code_a, bnf_code_b)

  if(!spending){
    df <- df %>%
      group_by(date) %>%
      summarise(type_a_items = sum(type_a_items),
                type_b_items = sum(type_b_items)) %>%
      gather(stat, "Items", type_a_items:type_b_items)
    if(!plot){
      return(df)
    } else if (plot){
      df %>%
        ggplot()+
        geom_line(aes(date, Items, group=stat, colour=stat))
    }
  } else if(spending) {
    df <- df %>%
      group_by(date) %>%
      summarise(type_a_spend = sum(type_a_spend),
                type_b_spend = sum(type_b_spend)) %>%
      gather(stat, "Spend", type_a_spend:type_b_spend)
    if(!plot){
      return(df)
    } else if(plot){
      df %>%
      ggplot()+
      geom_line(aes(date, Spend, group=stat, colour=stat))
    }
  }
}

##analysis <- merge(analysis, practices[c(1:3, 7)], by.x = "practice",
  ##  by.y = "PracNo")
