# PM102 Assessment in R
# student Number 2260092
# 29.11.2023

# Firstly we get PostgreSQL working in R and load the required libraries
library(RPostgreSQL) # we do this to access the database.
library(GetoptLong) # we do this to substitute variables into strings.
library(ggplot2)
library(tidyverse)
library(dplyr)
library(DBI)

drv <- dbDriver('PostgreSQL')  # i use this to connect R and PostgreSQL
con <- dbConnect(drv,dbname='gp_practice_data', host='localhost',
                 port=5432, user='postgres',
                 password="")

# to see the tables in our database
tables<- dbListTables(con)
print(tables)

# if our datasets is working properly,we can test with a simple query
data_gp <- dbGetQuery(con,"SELECT * FROM gp_data_up_to_2015")
glimpse(data_gp)

data_qof <- dbGetQuery(con,"SELECT * FROM qof_achievement")
glimpse(data_qof)

data_adr <- dbGetQuery(con,"SELECT * FROM address")
glimpse(data_adr)

data_qof_ind <- dbGetQuery(con,"SELECT * FROM qof_indicator")
glimpse(data_qof_ind)

data_bnf <- dbGetQuery(con,"SELECT * FROM bnf")
glimpse(data_bnf)    

data_chem <- dbGetQuery(con,"SELECT * FROM chemsubstance")
glimpse(data_chem)


#Solution Starts here,lets first take a look at what practiceid looks like

# we have 619 practices in the gp data
practiceid_in_gp <- dbGetQuery(con,"SELECT DISTINCT(practiceid) 
                               FROM gp_data_up_to_2015")

practiceid_count <- dbGetQuery(con,
                      "SELECT practiceid, COUNT(*)as count_num 
                      FROM gp_data_up_to_2015 
                      GROUP BY practiceid 
                      ORDER BY count_num")

# we have 460 orgcode including WAL
orgcode_in_qof <-dbGetQuery(con,"SELECT DISTINCT(orgcode) FROM qof_achievement")

# practices in gp data that are NOT in QOF table(we have 160 of them) 
note_practiceid <- dbGetQuery(con,"SELECT DISTINCT(practiceid)
FROM gp_data_up_to_2015  
WHERE practiceid 
NOT IN (SELECT DISTINCT(orgcode) FROM qof_achievement)")

#orgcode in the QOF table that is NOT in gp data(we have just 'WAL')
note_orgcode<-dbGetQuery(con,"SELECT DISTINCT(orgcode) FROM qof_achievement 
           WHERE orgcode NOT IN 
           (sELECT DISTINCT(practiceid) FROM gp_data_up_to_2015)")


#Question 1
# we create a function to check if medication and QOF data is available for any 
# practiceid selected by the user.we also use trycatch and also handle error

check <- function(practice_id) {
  tryCatch({
    practice_med_available <- practice_id %in% as.character(data_gp$practiceid)
    practice_qof_available <- practice_id %in% as.character(data_qof$orgcode)
    return(list(med_available = practice_med_available, qof_available = 
                  practice_qof_available))
  }, error = function(e) {
    cat("An error occurred:", e$message, "\n")
    return(list(med_available = FALSE, qof_available = FALSE))
  })
}

while (TRUE) {
  # We allow user input to select a GP practice (any practiceid from above)
  # practiceid is case sensitive(the W should be in capital letter)  also note 
  #that practiceid in the gp table is the same as orgcode in the data_qof table
  practice_id <- readline(prompt = "Enter practiceid: ")
  
  #We now check medication and QOF availability for the practice_id 
  availability <- check(practice_id)
  med_available <- availability$med_available
  qof_available <- availability$qof_available
  
  # if statement to check if both medication and QOF data are available
  if (med_available && qof_available) {
    cat("Both Medication information and QOF data are available for your 
        practice.\n")
    break  # if both medication & QOF data are available we break the loop
    
    # Question 1(a) to check if medication is available
  } else if (med_available && !qof_available) {
    cat("Medication information is available but 
        QOF data is not available for your practice.\n")
    
    # Question 1(b) to check if QOF data is available
  } else if (!med_available && qof_available) {
    cat("QOF data is available but Medication information is not available 
        for your practice.\n")
  } else {
    cat("Both Medication information and QOF data are not available
        for your practice.\n")
  }
}

# Question 1(c)
# If there is both medication and QOF information available for the practice,
#show:
#The number of patients at the practice; and
#The average spend per month on medication

if (med_available && qof_available) {
  
    # Filter medication data for the selected practice
    gp_practice_table <- data_gp %>%
      filter(practiceid == practice_id)%>%
      filter(period>="201501")
  
    qof_practice_table <- data_qof %>%
      filter(orgcode==practice_id)
    
    # Question 1(ci) Calculate number of patients at the practice
    practice_number_patients <- max(qof_practice_table$field4)
    cat("Number of patients at the practice:", practice_number_patients, "\n")
    
    
    #Question 1(cii) average spent per month in the practice
    practice_mont_spt <- aggregate(actcost ~ period, 
                                   data = gp_practice_table, FUN = sum)
    
    # we get the number of unique periods
    practice_num_p <- length(unique(gp_practice_table$period))
    
    # I now calculate average spent per month in the practice
    practice_avg_spend_per_month <- 
      sum(practice_mont_spt$actcost) / practice_num_p
    cat("Average spend per month on medication:", 
        practice_avg_spend_per_month,"\n")
    
      # Question 1(ciii):
    # Create a visualisation showing the spend on medication per patient,
    #compared to other practices within the postcode area

    # we now get the postcode based on the practiceid
    practice_postcode_query <- paste("SELECT postcode FROM address 
                                 WHERE practiceid = '", practice_id, "'"
                                     , sep = "")
      
    practice_postcode <- dbGetQuery(con, practice_postcode_query)
   
    # Extract similar postcode data by using the first 4 characters 
    # of the practice postcode
    similar_postcode_query <- paste("SELECT * FROM address 
                                WHERE postcode LIKE '", 
                                    substr(practice_postcode$postcode, 1,4), 
                                    "%'", sep = "")
    
    similar_postcode_data <- dbGetQuery(con, similar_postcode_query)
    
    
    similar_postcode_med_table <- left_join(similar_postcode_data,data_gp,
                                             by = "practiceid")
    
    per_patient_med_cost <- similar_postcode_med_table %>%
      group_by(practiceid, postcode) %>%
      summarise(cost = 
                  sum(actcost)/practice_number_patients)
    
    per_patient_med_cost<-per_patient_med_cost%>%
      drop_na(cost)
    
    #  we differentiate the reference practiceid
    per_patient_med_cost$highlight <- 
      ifelse(per_patient_med_cost$practiceid == 
               practice_id, "Reference", "Other")
    
    plot_title <- 
      paste("Spend on Medication per patient compare to other 
            practices within the postcode area")
    
    plot(
      ggplot(
        per_patient_med_cost,
        aes(x = practiceid, y = cost,fill = highlight)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = plot_title) +
        theme_minimal() +
        scale_fill_manual(
          values = c("Other" = "blue", "Reference" = "red"),
          labels = c("Other Practice", "Reference Practice"),
          name = "Practice ID Highlight"
        ) +
        guides(fill = guide_legend(title = "Highlight")) +
        geom_text(
          aes(label = round(cost, 2)),
          position = position_dodge(width = 1),
          vjust = -0.5,
          size = 3
        ) +
        theme(axis.text.x = element_blank(),  # Remove x-axis text
              axis.ticks.x = element_blank()) # Remove x-axis ticks
              #axis.title.x = element_blank()) # Remove x-axis label
    )
    
    # Question1c(iv)
    # Report the rate of diabetes at the practice
    # Filter rows where the indicator column starts with "DM001"
    qof_practice_table <- data_qof%>%
      filter(orgcode == practice_id)
    
    practice_diabetes_rate<-qof_practice_table [grepl(
      "^DM001",qof_practice_table $indicator), c("orgcode","numerator",
                                             "field4","ratio")]
  
    # Display the filtered data, that is rate of diabetes at the practice
    cat("practice_diabetes rate is:",practice_diabetes_rate$ratio,"\n")
  
    #Question 1c(v)
    # first wales overall rate of diabetes
    wales_table <- data_qof%>%
      filter(orgcode == "WAL")
    
    wales_overall_diabetes_rate<-wales_table [grepl(
      "^DM001",wales_table $indicator), c("orgcode","numerator",
                                                 "field4","ratio")]
    cat("wales overall diabetes rate:",wales_overall_diabetes_rate$ratio,"\n")
    
    # other practices in wales comparism using point plot 
    all_practices_wales_diabetes_rate<-data_qof[grepl(
      "^DM001",data_qof$indicator), c("orgcode","numerator",
                                                "field4","ratio")]
    
    #we use ggplot
    ggplot(all_practices_wales_diabetes_rate, aes(x = orgcode, y = ratio)) +
      geom_point() +
      geom_point(data = practice_diabetes_rate, aes(x = orgcode,
                                                    y = ratio), color = "red") +
      labs(title = "Rate of Diabetes Comparison in other practices Wales") +
      theme_minimal() +
      theme(axis.text.x = element_blank())  

} else {
  cat("no data present for analysis.\n")
    
}  
 

#Question2 COMPARING THE RATE OF DIABETES AND INSULIN

analyze_diabetes_insulin <- function(period){
  
  all_practices_wales_diabetes_rate<-data_qof[grepl(
    "^DM001",data_qof$indicator), c("orgcode","ratio")]
  
  
  # 060101 in the bnf chemical column represent insulin 
  insulin_gp_data <- dbGetQuery(con, "SELECT * FROM gp_data_up_to_2015 
                              WHERE bnfcode LIKE '060101%'")
  
  
  qof_insulin <- left_join(insulin_gp_data, data_qof,
                                by = c('practiceid' = 'orgcode'))
  
  insulin <- which(colnames(qof_insulin) == "actcost")  
  insulin_rate <- qof_insulin[!is.na(qof_insulin[, insulin]),
                                   c("practiceid","numerator",
                                     "field4","ratio", "period", 
                                     "indicator")]%>%
  
    filter(period==period_input)%>%
  #When IFCC-HbA1c levels are 64mmol/mol, 
  #it suggests the prescription of Insulin. check the data_qof_ind and reference
  # in the user documentation
    filter(indicator=='DM008')%>%
    distinct(practiceid, .keep_all = TRUE)
  
  insulin_rate<- insulin_rate[,c(-2,-3,-5,-6)]
  
  all_diabetes_wales_rate <- all_practices_wales_diabetes_rate$ratio
  
  insulin_rate_wales <- insulin_rate$ratio
  
  # we now use inner join for wales rate practices and Insulin rate practices
  diabetes_insulin_rate <- 
    inner_join(all_practices_wales_diabetes_rate,insulin_rate,
               by = c("orgcode" = "practiceid"))
  
  # we now columns for correlation analysis
  all_diabetes_wales_rate <- diabetes_insulin_rate$ratio.x
  insulin_rate_wales <- diabetes_insulin_rate$ratio.y
  
  #we perform correlation(0.1786712)
  correlation <-cor(all_diabetes_wales_rate,insulin_rate_wales)
  
  #we perform correlation test
  correlation_test <- cor.test(all_diabetes_wales_rate,insulin_rate_wales)
  
  p_value <- correlation_test$p.value
  
  print(paste("Correlation between diabetes rate and insulin is:",correlation))
  print(paste("p-value of the correlation test is:",p_value))
  
  #Check for statistical significance
  if (p_value < 0.05) {
    print(paste("Statistical significant relationship exists",
                "between diabetes and insulin."))
  } else {
    print("There is no statistically significant relationship between them.")
  }
  
  # Create a scatter plot to visualize the relationship
  ggplot(diabetes_insulin_rate, aes(x = all_diabetes_wales_rate,
                                    y = insulin_rate_wales)) +
    geom_point() +
    labs(
      x = "Diabetes Rate",
      y = "Insulin Prescribing Rate",
      title = "Relationship between Diabetes Rate and Insulin Prescribing Rate"
    )
}  
# period between 201501 to 201512
period_input <- "201501" #this period can be changed to user s choice e.g 201511
analyze_diabetes_insulin(period_input)




# comparing the rate of Diabetes and Metformin

analyze_diabetes_metformin <- function(period){
  all_practices_wales_diabetes_rate<-data_qof[grepl(
    "^DM001",data_qof$indicator), c("orgcode","ratio")]
  
  # 060102 in the bnf chemical column represent metformin
  # Glucophage is a brand name for metformin in UK check reference in the 
  #user documentation
  metformin_gp_data <- dbGetQuery(con, "SELECT * FROM gp_data_up_to_2015 
                                   WHERE  bnfcode LIKE '0601022%'")
  
  
  qof_metformin <- left_join(metformin_gp_data, data_qof,
                                  by = c('practiceid' = 'orgcode'))
  
  metformin <- which(colnames( qof_metformin) == "actcost")  
  metformin_rate <-  qof_metformin[!is.na( qof_metformin[, metformin]),
                                       c("practiceid","numerator",
                                         "field4","ratio", "period",
                                         "indicator")]%>%
    filter(period==period_input)%>%
  
  #When IFCC-HbA1c levels are 59mmol/mol or lower, 
  #it suggests the prescription of metformin. check the data_qof_ind and 
  #reference in the user documentation
    filter(indicator=='DM007')%>%
    distinct(practiceid, .keep_all = TRUE)
  
  metformin_rate<- metformin_rate[,c(-2,-3,-5,-6)]
  
  
  # we now use inner join for wales rate practices and metformin rate practices
  diabetes_metformin_rate <- inner_join(all_practices_wales_diabetes_rate, 
                                        metformin_rate, 
                                        by = c("orgcode" = "practiceid"))
  
  
  
  all_diabetes_wales_rate <- diabetes_metformin_rate$ratio.x
  
  metformin_rate_wales <- diabetes_metformin_rate$ratio.y
  
  
  #correlation test,
  correlation <-cor(all_diabetes_wales_rate,metformin_rate_wales)
  
  correlation_test <- cor.test(all_diabetes_wales_rate,metformin_rate_wales)
  
  p_value <- correlation_test$p.value
  
  print(paste("Correlation betwen diabetes rate and metformin is:",correlation))
  print(paste("p-value of the correlation test is:",p_value))
  
  
  #Check for statistical significance
  if (p_value < 0.05) {
    print(paste("Statistical significant relationship exists",
                "between diabetes and metformin."))
  } else {
    print("There is no statistically significant relationship between them.")
  }
  
  # Create a plot to visualize the relationship
  ggplot(diabetes_metformin_rate,aes(x= all_diabetes_wales_rate,
                                     y = metformin_rate_wales)) +
    geom_point() +
    labs(
      x = "Diabetes Rate",
      y = "Metformin Prescribing Rate",
      title= "Relationship between Diabetes Rate and Metformin Prescribing Rate"
    )
}

# period between 201501 to 201512
period_input <- "201501" #this period can be changed to user s choice e.g 201511
analyze_diabetes_metformin(period_input)

# In summary
#The connection observed between diabetes rates and the use of metformin
#shows a much lower p-value (4.557e-05) compared to the connection between 
#diabetes rates and insulin use (0.0001209) for period 201501
#Consequently, this means that the relationship between diabetes rates and 
#metformin use holds stronger statistical significance when compared to the
#relationship between diabetes rates and insulin use.




#PART TWO OPEN ENDED

#  Question1(OPEN ENDED)
#  i:what is the total amount spent on medication for epilepsy by women in Wales
#     practices compare to that of mental health ?

# ii: show the practices and the five least spent with their combined cost
#on epilepsy and mental health by women in Wales.

# iii Analyze the prescribing costs for practices in wales for  two epilepsy
#medications, Levetiracetam and Zonisamide,perfom a statistical test to
#determine their relationshio and visualize the cost relationship.


# I start my analysis by creating a function my_analysis
# question i and ii in one function
my_analysis <- function() {
  
  # getting epilepsy cost data from the gp data, we start by looking at our data
  # bnfcode starting with 04080 represents drugs used to treat eplilepsy
  #(Antiepileptic drugs) details of these is indicated in the bnf table
  cost_gp_data_epilepsy <- dbGetQuery(con, "SELECT practiceid, bnfname, actcost 
                                          FROM gp_data_up_to_2015
                                          WHERE bnfcode LIKE '04080%' AND 
                                      period >= '201501'")
  
  # getting epilepsy women data
  #EP003 in the qof indicator table represent the percentage of women taking 
  # antiepileptic drug and have a record or information
  epilepsy_women_data <- data_qof[grepl("^EP003", data_qof$indicator),
                                  c("orgcode", "ratio")]
  
  # Joining epilepsy cost and women data using left_join
  epilepsy_cost_data <- left_join(cost_gp_data_epilepsy, epilepsy_women_data,
                                  by = c('practiceid' = 'orgcode'))
  
  # i remove the 4th column
  epilepsy_cost_data <- epilepsy_cost_data[, -4]
  
  #I now  Calculate total cost by practice for epilepsy
  total_by_practice_on_epilepsy <- epilepsy_cost_data %>%
    filter(!is.na(actcost)) %>%
    group_by(practiceid) %>%
    summarise(total_cost_epilepsy = sum(actcost)) %>%
    arrange(desc(total_cost_epilepsy))
  
  # getting mental health cost data
  cost_gp_data_mental_health<-dbGetQuery(con, "SELECT practiceid,bnfname,actcost 
                                                FROM gp_data_up_to_2015
                                                WHERE bnfcode LIKE '04020%'
                                           AND period >= '201501'")
  
  # getting mental health women data
  mental_health_women_data <- data_qof[grepl("^MH008", data_qof$indicator),
                                       c("orgcode", "ratio")]
  
  # Joining mental health cost and women data using left_join
  mental_health_cost_data <- left_join(cost_gp_data_mental_health, 
                                       mental_health_women_data, 
                                       by = c('practiceid' = 'orgcode'))
  mental_health_cost_data <- mental_health_cost_data[, -4]
  
  # I now calculate total cost by practice for mental health
  total_by_practice_on_mental_health <- mental_health_cost_data %>%
    group_by(practiceid) %>%
    summarise(total_cost_mental_health = sum(actcost)) %>%
    arrange(desc(total_cost_mental_health))
  
  # combining the two tables together using inner_join
  mh_epilepsy_cost_wales_practices_data <- 
    inner_join(total_by_practice_on_epilepsy, 
               total_by_practice_on_mental_health, by = "practiceid")
  
  # I now calculate combined total cost
  mh_epilepsy_cost_wales_practices_data$total_combined_cost <-
    mh_epilepsy_cost_wales_practices_data$total_cost_epilepsy +
    mh_epilepsy_cost_wales_practices_data$total_cost_mental_health
  
  # i take out the least cost spent in 5 practices based on combined total cost
  least_five_practices <- mh_epilepsy_cost_wales_practices_data %>%
    arrange(total_combined_cost) %>%
    head(5)
  
  # i now visualize
  least_five_practices_long <- least_five_practices %>%
    pivot_longer(cols = c(total_cost_epilepsy, total_cost_mental_health),
                 names_to = "Category",
                 values_to = "Cost")
  
  # grouped bar plot
  histogram <- ggplot(least_five_practices_long, 
                      aes(x = practiceid,
                          y = Cost, fill =Category )) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Practice ID", y = "Total Cost", 
         title =
           "Total costs on Epilepsy and Mental Health in least five Practices")+
    scale_fill_manual(values = c("total_cost_epilepsy" = "lightblue", 
                                 "total_cost_mental_health" = "orange")) +
    theme(axis.text.x = element_blank())
  
  # Printing the histogram
  plot(histogram)
  
  #  total cost on epilepsy by practiceid
  cat("Total by practice on epilepsy:\n")
  print(total_by_practice_on_epilepsy)
  
  # total cost on mental health by practiceid
  cat("\nTotal by practice on mental health:\n")
  print(total_by_practice_on_mental_health)
  
  # combine cost on epilepsy and mental health for the least five practiceid.
  cat("\nCombined cost on epilepsy and mental health for least 5 practices:\n")
  print(least_five_practices)
}

# i now call my function
my_analysis()



#Question iii Analyze the prescribing costs for practices in wales for  two
#epilepsy medications, Levetiracetam and Zonisamide,perfom a statistical test to
#determine their relationshio and visualize the cost relationship.
levetiracetam_zonisamide_analysis <- function() {
  levetiracetam_cost_t_epilepsy <- dbGetQuery(con, "SELECT practiceid,
  bnfname,bnfcode,actcost 
  FROM gp_data_up_to_2015 
  WHERE bnfcode LIKE '0408010A0%' 
  AND period >= '201501'")
  
  levetiracetam_cost_t_epilepsy <- levetiracetam_cost_t_epilepsy %>%
    group_by(practiceid) %>%
    summarise(total_cost = sum(actcost))
  
  
  
  zonisamide_cost_t_epilepsy <- dbGetQuery(con, "SELECT practiceid,
  bnfname,bnfcode,actcost 
  FROM gp_data_up_to_2015 
  WHERE bnfcode LIKE '0408010AD%' 
  AND period >= '201501'")
  
  zonisamide_cost_t_epilepsy <- zonisamide_cost_t_epilepsy %>%
    group_by(practiceid) %>%
    summarise(total_cost = sum(actcost))
  
  
  levetiracetam_zonisamide_combine_cost <- 
    inner_join(levetiracetam_cost_t_epilepsy,zonisamide_cost_t_epilepsy,
               by = 'practiceid')
  
  levetiracetam_total_cost_practices <-
    levetiracetam_zonisamide_combine_cost$total_cost.x
  
  zonisamide_total_cost_practices <- 
    levetiracetam_zonisamide_combine_cost$total_cost.y
  
  t_test <- t.test(levetiracetam_total_cost_practices,
                   zonisamide_total_cost_practices)
  print(t_test)
  
  # Extract p-value from the t-test result
  p_value <- t_test$p.value
  
  # Print the p-value
  print(paste("p-value for the t-test between the two drug is:",p_value))
  
  if (p_value < 0.05) {
    print(paste("Statistical significant relationship exists",
                "between levetiracetam and zonisamide."))
  } else {
    print("There is no statistically significant relationship between them.")
  }
  
  # Create a plot to visualize the relationship
  ggplot(levetiracetam_zonisamide_combine_cost,
         aes(x= levetiracetam_total_cost_practices,
             y = zonisamide_total_cost_practices)) +
    geom_point() +
    labs(
      x = "Levetiracetam costs",
      y = "Zonisamide costs",
      title="Relationship between Levetiracetam and Zonisamide Prescribing Cost"
    )
}  
levetiracetam_zonisamide_analysis()  




# OPEN ENDED Question 2
# Allow user to select two health boards:
#(1) calculate the total prescription for the selected user's health boards as 
#as well the top five prescribed drug in the two health boards.

#(2) See if relationtionship exits between any two choosen health boards
# by visualization

# Solution starts
# first lets see what health board(hb) looks like in the gp data
health_board_wales <- dbGetQuery(con,"SELECT DISTINCT(hb) 
                               FROM gp_data_up_to_2015")

#we have seven distinct health board in wales
print(health_board_wales)

#I start my analysis by setting attempt number
max_attempts <- 3

# i cast the top five drugs prescribed in the two health board into a list
top_five_drug_prescribed_by_hb <- list()


# i now use for loop to iterate
for (board_number in 1:2) {
  cat(sprintf("Enter health board %d value: ", board_number))
  
  for (attempt in 1:max_attempts) {
    user_input <-
      readline(prompt = 
                 sprintf("Enter the 'hb' value for health board %d: ",
                         board_number))
    
    if (!grepl("^[A-Za-z0-9]+$", user_input)) {
      cat("Please enter a valid alphanumeric 'hb' value.\n")
    } else {
      # Check if the entered 'hb' value exists in the database
      hb_exists <- dbGetQuery(con, sprintf("SELECT COUNT(*) as count 
                                           FROM gp_data_up_to_2015 
                                           WHERE hb = '%s'AND period >= 201501", 
                                           user_input))
      
      if (hb_exists$count == 0) {
        cat("No data found for the entered 'hb' value.\n")
      } else {
        top_prescribed_drug_hb <- dbGetQuery(con, 
                                             sprintf("SELECT hb, bnfname,items, 
        COUNT(bnfname) AS num_times_drug_is_prescribed 
        FROM gp_data_up_to_2015 
        WHERE period >= 201501 AND hb = '%s' 
        GROUP BY hb,bnfname, items 
        ORDER BY COUNT(bnfname) DESC", user_input))
        
        
        #calculate the total prescription for the user's health board
        prescription_sum <- sum(top_prescribed_drug_hb$items)
        
        # Output the sum
        cat(sprintf("The total prescription in health board %d is: %d\n", 
                    board_number, prescription_sum))
        
        #get the top five prescribed drug in the user's 
        #health board and visualize
        top_five_drug_prescribed_by_hb[[board_number]] <- 
          top_prescribed_drug_hb %>%
          group_by(hb) %>%
          top_n(5, num_times_drug_is_prescribed) %>%
          arrange(hb, desc(num_times_drug_is_prescribed))
        
        print(head(top_five_drug_prescribed_by_hb[[board_number]], n = 5))
        
        break  # Exit loop if data is retrieved successfully
      }
    }
    
    if (attempt == max_attempts) {
      cat("Maximum attempts reached for health board ", board_number, ". 
          Moving to the next board.\n")
      break
    }
  }
}

# we combine the two health board
two_hb_combined_data <- bind_rows(top_five_drug_prescribed_by_hb)

#  we plot histograms for each health board's top five prescribed drugs
ggplot(two_hb_combined_data, aes(x = bnfname, y = num_times_drug_is_prescribed, 
                                 fill = factor(hb))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top Prescribed Drugs for Health Boards",
       x = "Drug Name",
       y = "Prescription Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~hb, ncol = 1)



  


#OPEN ENDED Question3(a): top seven most prescribed drug in any locality  
           #1(b) top seven practices with the most number of prescriptions
           #1(C) Drug distribution across localities and visualize for the user 
           # selected locality and indicate what locality in Wales

# first lets see all the localities we have in Wales
locality_count <- dbGetQuery(con,
                               "SELECT locality, COUNT(locality)as count_num 
                      FROM gp_data_up_to_2015 
                      GROUP BY locality 
                      ORDER BY count_num")

# we have 22 localities in wales
print(locality_count$locality)

valid_locality <- FALSE


while (!valid_locality) {
  locality_input <- readline("Enter a locality: ")#choose from the 22 localities
  
  #  We check if the entered locality exists in the address table
  locality_check_query <- dbSendQuery(con, paste0("
  SELECT practiceid, locality 
  FROM address 
  WHERE practiceid = '", locality_input, "'
"))
  
  locality_info <- dbFetch(locality_check_query)
  
  
  if (nrow(locality_info) > 0) {
    # If locality exists, get the locality value from address table
    user_locality <- locality_info$locality[1]  
    
    locality_seven_most_prescribed_drug <- dbGetQuery(con, paste0("
      SELECT locality, bnfname, COUNT(bnfname) AS num_times_drug_prescribed
      FROM gp_data_up_to_2015
      WHERE locality = '", locality_input, "'
      GROUP BY locality, bnfname
      ORDER BY COUNT(bnfname) DESC
      LIMIT 7
    "))
    
    locality_top_seven_practices_num_prescription <- dbGetQuery(con, paste0("
      SELECT locality, practiceid, COUNT(bnfname) AS num_prescriptions
      FROM gp_data_up_to_2015
      WHERE locality = '", locality_input, "'
      GROUP BY locality, practiceid
      ORDER BY COUNT(bnfname) DESC
      LIMIT 7
    "))
    
    drug_distribution_across_localities <- dbGetQuery(con, "
      SELECT locality, bnfname, COUNT(bnfname) AS num_times_drug_prescribed
      FROM gp_data_up_to_2015
      GROUP BY bnfname, locality
      ORDER BY COUNT(bnfname) DESC
    ")
    
    if (nrow(locality_seven_most_prescribed_drug) > 0 &&
        nrow(locality_top_seven_practices_num_prescription) > 0 &&
        nrow(drug_distribution_across_localities) > 0) {
      
      # i now  print the results for drugs, practices, and drug distribution.
      print("Top prescribed drugs in the user locality:")
      print(locality_seven_most_prescribed_drug)
      
      print("Top seven prescribing practices in the user locality:")
      print(locality_top_seven_practices_num_prescription)
      
      print("Drug distribution across localities:")
      print(drug_distribution_across_localities)
      
      # set variable for top seven drugs in any locality
      top_seven_drugs_locality <- locality_seven_most_prescribed_drug$bnfname
      
      drug_distribution_top_seven_locality <- 
        drug_distribution_across_localities %>%
        filter(locality == 
                 locality_input & bnfname %in% top_seven_drugs_locality)
      
      # Create a bar chart for top prescribed drugs in the chosen locality
      plot(ggplot(data = drug_distribution_top_seven_locality,
                  aes(x = bnfname, y = num_times_drug_prescribed, 
                      fill = user_locality)) +
             geom_bar(stat = "identity", position = "dodge") +
             geom_text(aes(label = num_times_drug_prescribed), 
                       position = position_dodge(width = 0.5), 
                       vjust = -0.5, 
                       size = 3, 
                       color = "black") +  # Adjust label appearance as needed
             labs(title = paste("Top seven prescribed drugs in",
                                user_locality, "\n"), 
                  x = "Drug", y = "Count") +
             theme_minimal() +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                              hjust = 1)))
      
      valid_locality <- TRUE
      
    } else {
      cat("Your choosen locality does not exist,
          please choose from the 22 localities in Wales.\n")
    }
  } else {
    cat("Your choosen locality does not exist in Wales,p
        lease choose from 22 localities in Wales.\n")
  }
}







































  
  


