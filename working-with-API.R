library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyr)

# The header that includes the auth token
headers <- c('Authorization' = paste("Token", 'XX'))


# ideal workflow: 
# 1. get list of questions
# 2. create a question csv by looping through the list of questions
# 3. Create a df with all predictions for a question

groups <- c("19899", "19901", "19907", "19912", "19954", "19961", "19970",
            "19975", "19978", "19997", "20333", "20334", "20335", "20336",
            "20342") |> sort()

sub_questions <- my_vector <- c(
  20283, 20284, 20286, 20287, 20288, 20289, 20290, 20292, 20285, 20291,
  20236, 20281, 20282, 20298, 20299, 20300, 20301, 20302, 20303, 20305,
  20306, 20307, 20304, 20239, 20296, 20297, 20310, 20311, 20312, 20313,
  20314, 20315, 20316, 20317, 20318, 20319, 20308, 20309, 20238, 20320,
  20321, 20326, 20329, 20331, 20322, 20323, 20324, 20325, 20327, 20328,
  20330, 20237, 19957, 19956, 19958, 19955, 19965, 19964, 19962, 19963,
  19971, 19972, NA, 19979, 19980, NA, NA, NA, NA, NA, 20343, 20344
) |> sort()

data_list <- list()

# Get question ids for comparisons
project_id <- 2723
base_url <- paste0('https://www.metaculus.com/api2/questions/?project=', project_id, '&')
# this way of fetching the data for example doesn't give us the description? 
# and also we're not getting the subquestions directly, but rather just in 
# a nested weird way

loopcounter <- 0
offset <- 0  # Initialize offset
limit <- 100  # Number of records per request

# Fetch API data
repeat {
  if (loopcounter == 0) {
    url <- paste0(base_url, "limit=", limit)
    print("Fetching data with no offset")
  } else {
    url <- paste0(base_url, "limit=", limit, "&offset=", offset)
    print(paste("Fetching data with offset", offset))
  }
  
  response <- httr::GET(url, httr::add_headers(.headers=headers))
  if (status_code(response) == 200) {
    data <- fromJSON(rawToChar(response$content))
    if (length(data$results) == 0) {
      print("No more results.")
      break  # Exit the loop if no more results are returned
    }
    selected_results <- data$results |> 
      as_tibble() 
    
    l <- list()
    for (i in 1:nrow(selected_results)) {
      l[[i]] <- unnest_longer(selected_results[i, ], sub_questions, keep_empty = TRUE)
      
      # have to do some converting of data types to make it possible to 
      # to combine everything into a single data.frame
      if (is.logical(l[[i]]$sub_questions$possibilities$low)) {
        l[[i]]$sub_questions$possibilities$low <- as.character(l[[i]]$sub_questions$possibilities$low)
        l[[i]]$sub_questions$possibilities$high <- as.character(l[[i]]$sub_questions$possibilities$high)
      }
      if (is.numeric(l[[i]]$sub_questions$possibilities$scale$max)) {
        l[[i]]$sub_questions$possibilities$scale$max <- as.character(l[[i]]$sub_questions$possibilities$low)
        l[[i]]$sub_questions$possibilities$scale$min <- as.character(l[[i]]$sub_questions$possibilities$high)
      }
    }
    data <- bind_rows(l)
    
    # further unnesting to get the sub_questions data into the main data.frame
    data <- data |>
      unnest(sub_questions, names_sep = "_")
    
    # extract the latest median prediction which is either in the community
    # prediction or the sub_questions_community_prediction
    data <- data |>
      mutate(median = community_prediction$full$q2) |>
      mutate(median = ifelse(is.na(median), sub_questions_community_prediction$full$q2, median))

    offset <- offset + limit  # Increment the offset for the next batch
    loopcounter <- loopcounter + 1
  }
  
  data_list[[loopcounter]] <- data
}

data <- bind_rows(data_list)

# question data
question_data <- data |>
  select(id, sub_questions_id, 
         active_state, sub_questions_active_state, 
         condition, 
         cp_reveal_time, sub_questions_cp_reveal_time,
         created_time, 
         edited_time, 
         effected_close_time, sub_questions_effected_close_time, 
         close_time, sub_questions_close_time,
         publish_time, sub_questions_publish_time,
         sub_questions_conditioned_on_resolution,
         possibilities, sub_questions_possibilities,
         resolution, sub_questions_resolution,
         projects, group, group_label, 
         resolve_time, 
         status, 
         title, 
         type,
         url)

# prediction data
prediction_data <- data |>
  select(id, sub_questions_id, 
         community_prediction, sub_questions_community_prediction) |>
  unnest(community_prediction, names_sep = "_") |>
  unnest(sub_questions_community_prediction, names_sep = "_") 





rbind(prediction_data$community_prediction_full, 
      prediction_data$sub_questions_community_prediction_full) 

names(prediction_data)

question_data

data$possibilities

# Unclear columns:
# what is data$activity?

data <- data |>
  select(page_url, title, id, activity, sub_questions, 
         active_state, community_prediction, resolution)

d <- data |>
  

# Write to CSV - currently not working since we have nested data
# csv_filename <- "questions_list.csv"
# write_csv(csv_data, csv_filename)
# 
# print("CSV file has been written.")