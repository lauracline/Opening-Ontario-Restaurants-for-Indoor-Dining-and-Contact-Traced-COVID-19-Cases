# Preamble ###############################################################################################################################################################################################################################################################

# ---
# title: 
# author: "Laura Cline, Lee Doucet, and Will Trefiak"
# email: laura.cline@mail.utoronto.ca
# date: "19/02/2021"
# abstract: "This paper uses multiple linear regression and multiple logistic regression to explore the relationship between opening Ontario restaurants for in-door dining and the number of COVID-19 cases traced back to that restaurant in March 2021. We used simple 
# random sampling on 38,000 Ontario restaurants in the Canadian Business Registries to pull a sample of ~18,900 restaurants who were split into treatment and control groups. The paper exposes that restaurants who received the treatment - open for 
# indoor dining - had significantly more COVID-19 cases traced back to their location compared to restaurants who did not receive the treatment. Therefore, the paper concludes that the Ontario restaurants should keep restaurants shutdown for indoor 
# dining in order to minimize the spread of the virus and protects Ontarians health."
# output:
#   bookdown::pdf_document2:
#   toc: FALSE
# bibliography: references.bib
# ---


# Setup Environment ######################################################################################################################################################################################################################################################

## Install packages and read libraries ##

# install.packages("kableExtra")
# install.packages("RColorBrewer")
# install.packages("gridExtra")

library(tidyverse)
library(RColorBrewer)
library(broom)
library(gridExtra)
library(grid)


# Stimulate the Survey Results #############################################################################################################################################################################################################################

## BUILD SURVEY ##

## Set Seed 
set.seed(888)


## Survey Questions 
# Q1: In what year was your business first established?
# Q2: Over the past year (March 2020 to present), how has each of the following changed for this business? (Sales)
# Q3: Over the past year (March 2020 to present), how has each of the following changed for this business? (Number of Employees)
# Q4: Over the past year (March 2020 to present), how has each of the following changed for this business? (Gross Revenue)
# Q5: Over the past year (March 2020 to present), how has each of the following changed for this business? (Gross Expenses)
# Q6: Over the past year (March 2020 to present), how has each of the following changed for this business? (Net Revenue)
# Q7: Which of the following have been obstacles to your business? 
# Q8: Due to COVID-19, was funding or credit for this business approved or received through any of the following sources? Pick the option where the majority of the business's funding is coming from. 
# Q9: If you answered “None of the Above” for the question above, for which of the following reasons has the business not accessed any funding or credit due to COVID-19? 
# Q10: Does the business have the ability to take on more debt?
# Q11: How long can this business continue to operate at its current level of revenue and expenditure before having to consider permanent closure or bankruptcy?
# Q12: Is 50% or more of our business owned by a by a BIPOC visible minority (BIPOC visible minority is Canada is defined as someone who is non-white in colour or race, regardless of place of birth)? 
# Q13: If >= 50% of this business’s owners are BIPOC visible minorities, please select the categories that describe the owner or owners?
# Q14: Is 50% or more of our business owned by a by immigrants to Canada? 
# Q15: Does your business have access to platforms like UberEats, Doordash, or SkipTheDishes to complete delivery and takeout orders?
# Q16: If you selected option a above, does your business use platforms like UberEats, Doordash, or SkipTheDishes to complete delivery and takeout orders?
# Q17: If you selected option d above, why did you stop using these platforms?


## Create a tibble and put in each survey question 

sampling_frame_restaurant_population <- 
  tibble(person = c(1:38000), #~38,000 restaurants in Ontario alone
         Q1 = runif(n = 38000, min = 1900, max = 2021) %>% #The oldest restaurant in Ontario was established in 1789, but it skews up my dataset so most retaurants were established in the 1800s (older than the majority of restaurants in Canada), so I'm starting my years in 1900 to be safe 
           round(digits = 0),
         Q2 = sample(x = c("Increased", "Stayed the Same", "Decreased"),
                     size = 38000,
                     replace = TRUE,
                     prob = c(0.15, 0.66, 0.19)),
         Q3 = sample(x = c("Increased", "Stayed the Same", "Decreased"),
                     size = 38000,
                     replace = TRUE,
                     prob = c(0.15, 0.66, 0.19)),
         Q4 = sample(x = c("Increased", "Stayed the Same", "Decreased"),
                     size = 38000,
                     replace = TRUE,
                     prob = c(0.15, 0.19, 0.66)),
         Q5 = sample(x = c("Increased", "Stayed the Same", "Decreased"),
                     size = 38000,
                     replace = TRUE,
                     prob = c(0.60, 0.20, 0.20)),
         Q6 = sample(x = c("Increased", "Stayed the Same", "Decreased"),
                     size = 38000,
                     replace = TRUE,
                     prob = c(0.15, 0.19, 0.66)),
         Q7 = sample(x = c("Shortage of labour force", "Recruiting and training skilled employees", "Shortage of space and equipment", "Supply chain challenges", "Insufficent and/or fluctuating customer demand", "Cost of insurance", "Government regulations", "Rising cost of inputs", "Increasing competition", "Maintaining sufficient cash flow or managing debt", "Rent expenses", "High speed internet", "Cost of delivery and take out", "Other", "NA"),
                     size = 38000,
                     replace = TRUE,
                     prob = c(0.06, 0.06, 0.06, 0.06, 0.06, 0.06, 0.12, 0.06, 0.06, 0.10, 0.06, 0.06, 0.06, 0.06, 0.06)),
         Q8 = sample(x = c("CEBA", "Temporary 10% Wage Subsidy", "CEWS", "CERS", "CECRA", "Regional Relief and Recovery Fund", "Provincial, Territorial or Municipal Government Programs", "Grant or Loan Funding from Philanthropic or Mutual-Aid Sources", "Financial Institution (Term Loan or Line-of-Credit", "Loan from Family or Friends", "Other", "None of the Above"),
                     size = 38000,
                     replace = TRUE,
                     prob = c(0.09, 0.09, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08, 0.09, 0.09, 0.08, 0.08)),
         Q9 = if_else(Q8 == "None of the Above",
                      sample(x = c("Funding or credit not needed", "Waiting for approval or submitting application", "Did not meet eligibility requirements", "Unable to fill out application", "Lack of awareness", "Other"),
                             size = 38000,
                             replace = TRUE,
                             prob = c(0.10, 0.25, 0.30, 0.15, 0.19, 0.01)),
                      "NA"),
         Q10 = sample(x = c("Yes", "No", "Don't Know"),
                      size = 38000,
                      replace = TRUE,
                      prob = c(0.34, 0.36, 0.30)),
         Q11 = sample(x = c("Less than 1 month", "1 month to less than 3 months", "3 months to less than 6 months", "6 months to less than 12 months", "12 months or more", "Not considering permanent closure or bankrupcy", "Don't Know"),
                      size = 38000,
                      replace = TRUE,
                      prob = c(0.25, 0.18, 0.15, 0.15, 0.25, 0.15, 0.10)),
         Q12 = sample(x = c("Yes", "No"),
                      size = 38000,
                      replace = TRUE,
                      prob = c(0.31, 0.69)),
         Q13 = if_else(Q12 == "Yes",
                       sample(x = c("South Asian", "Chinese", "Black", "Arab", "West Asian", "Southeast Asian", "Multiple Visible Minority", "Korean", "Latin American", "Japanese", "First Nations, Metis and/or Inuit"),
                              size = 38000,
                              replace = TRUE,
                              prob = c(0.31, 0.21, 0.17, 0.06, 0.05, 0.04, 0.06, 0.03, 0.03, 0.02, 0.02)),
                       "NA"),
         Q14 = sample(x = c("Yes", "No"),
                      size = 38000,
                      replace = TRUE,
                      prob = c(0.15, 0.85)),
         Q15 = sample(x = c("Yes", "No"),
                      size = 38000,
                      replace  = TRUE,
                      prob = c(0.80, 0.20)),
         Q16 = if_else(Q15 == "Yes",
                       sample(x = c("Yes", "No", "No, but our business is considering it", "No, but our business used to use it"),
                              size = 38000,
                              replace = TRUE,
                              prob = c(0.90, 0.01, 0.01, 0.08)),
                       "NA"),
         Q17 = if_else(Q16 == "No, but our business used to use it",
                       sample(x = c("Commission fees too high", "Too many orders to fulfill on platforms", "Platform terms and conditions too strict", "I don't trust these platforms", "I don't understand how these platforms work", "Other"),
                              size = 38000,
                              replace = TRUE,
                              prob = c(0.40, 0.01, 0.04, 0.20, 0.25, 0.01)),
                       "NA"),
  ) %>%
  mutate(in_frame_sample = sample(x = c(0:1), # Randomize by assigning every restaurant a number between 1 and 10 
                                  size = 38000,
                                  replace = TRUE)) %>%
  mutate(group = sample(x = c(1:2), # assign each sample a number: either 1 or 2 
                        size = 38000,
                        replace = TRUE)) %>%
  mutate(group = ifelse(in_frame_sample == 1, group, NA)) # our two groups will come from our in-frame samples 


# Create Treatment and Control Group ########################################################################################################################################################################################################################################################################################################################################################################################################################################

## Create Treatment Group ## 

treatment_group <- 
  sampling_frame_restaurant_population %>%
  filter(group == 1) %>% # Filter by group 1 
  mutate(contact_traced_covid_cases = rnorm(n = 9383, mean = 6, sd = 3) %>% # Add a new column for number of covid cases with 9383 values, mean of 6 and standard deviation of 3
           round(digits = 0) %>% # Remove decimal values 
           abs()) # Only want absolute values 

treatment_group

## Create Control Group ##

control_group <-
  sampling_frame_restaurant_population %>%
  filter(group == 2) %>% # Filter by group 2 
  mutate(contact_traced_covid_cases = rnorm(n = 9594, mean = 3, sd = 2) %>% # Create a new column for number of covid cases for 9594 values, a mean of 3 and a standard deviation of 2 
           round(digits = 0) %>% # Remove decimal places 
           abs()) # Only show adbsolute values 

control_group

## Bind the treatment and control datasets ##

# Simulates that from our sampling from of 38,000 restaurants, 18,977 of them responded 

simulated_dataset <- rbind(treatment_group, control_group)

simulated_dataset


## Save and Clean-Up ##

write_csv(simulated_dataset, 'simulated_dataset.csv')


# Exploring Ontario's Restaurant Population #################################################################################################################################################################################################################################################################################################################################################################################################################################

## Create a bar graph for the number of restaurants whose owners are BIPOC ##

sampling_frame_restaurant_population %>%
  ggplot(mapping = aes(x = Q12)) + # Creates a plot with BIPOC on the x-axis and filled by Q12 
  geom_bar(fill = "darkgreen") + # Create a bar chart 
  theme_minimal() + # Clean theme 
  labs( # Add labels 
    title = "Over Half of Ontario's Restaurant Owners are Not BIPOC (March 2021)",
    subtitle = "Only ~30% of Ontario restaurants are owned by a Black, Indigenous \nor Person of Colour",
    caption = "(data from the 'A+ survey')",
    x = "Is 50% or more of your business owned by a by a BIPOC visible minority?",
    y = "Number of Restaurants") +
  guides(fill = FALSE) # Remove legend


## Ethnic Distribution ##

# Clean ethnicity column by removing all NA values 

# Turn ethnicity (Q13) into a factor variables and sort the factors in ascending order 
ethnicity_sorted <-
  sampling_frame_restaurant_population %>%
  filter(Q13 != "NA")

ethnicity_sorted <-
  within(ethnicity_sorted,
         Q13 <- factor(Q13,
                       levels = names(sort(table(Q13),
                                           decreasing = FALSE))))

# Create bar chart for number Ontario restaurant owners per non-white ethnicity 

ethnicity_sorted %>% 
  ggplot(mapping = aes(x = Q13)) + # Creates plot with ethnicity (Q13) on the x-axis and filled by Q13 
  geom_bar(fill = "darkgreen") + # Create a bar plot 
  theme_minimal() + # Clean theme 
  coord_flip() + # Flip axis 
  labs( # Add labels 
    title = "South Asian, Chinese, and Black Canadians are the \nTop Visible Minority Ethnic Groups \nfor Restaurant Owners in Ontario (March 2021)",
    subtitle = "The ethnic distribution for restaurant owners matches the ethnic \ndistribution for the Ontario population.",
    caption = "(data from the 'A+ survey')",
    x = "BIPOC Ethnic Group", 
    y = "Number of Restaurants") +
  guides(fill = FALSE) # Remove legend 


## Compare Sales, Gross Revenue, Gross Expenses and Net Revenue ##

# Create first plot for "Number of Sales"

plot_1 <- sampling_frame_restaurant_population %>%
  ggplot(mapping = aes(x = Q2, fill = Q2)) + # Create plot with number of sales (Q2) on the x-axis  
  geom_bar() + # Create bar plot 
  theme_minimal() + # Clean theme 
  labs( # Add labels 
    title = "Number of Sales Stayed the Same",
    x = "How did Sales Change?", 
    y = "# of Restaurants") +
  guides(fill = FALSE) # Remove legend 

# Create second plot for "Gross Revenue"

plot_2 <- sampling_frame_restaurant_population %>%
  ggplot(mapping = aes(x = Q4, fill = Q4)) + # Create plot with gross revenues (Q4) on the x-axis 
  geom_bar() + # Create bar chart 
  theme_minimal() + # Clean theme
  labs( # Add labels 
    title = "Gross Revenue Decreased",
    x = "How did Gross Revenue Change?",
    y = "# of Restaurants") +
  guides(fill = FALSE) # Remove legend 

# Create third plot for "Gross Expenses"

plot_3 <- sampling_frame_restaurant_population %>%
  ggplot(mapping = aes(x = Q5, fill = Q5)) + # Create plot with Gross Expenses (Q5) on the x-axis 
  geom_bar() + # Create bar chart 
  theme_minimal() + # Clean theme 
  labs( # Add labels 
    title = "Gross Expenses Increased",
    x = "How did Gross Expenses Change?",
    y = "# of Restaurants") +
  guides(fill = FALSE) # Remove legend 

# Create fourth plot for "Net Revenue"

plot_4 <- sampling_frame_restaurant_population %>%
  ggplot(mapping = aes(x = Q6, fill = Q6)) + # Create plot with Net Revenue (Q6) on the x-axis 
  geom_bar() + # Create bar chart 
  theme_minimal() + # Clean theme 
  labs( # Add labels 
    title = "Net Revenue Decreased",
    x = "How did Net Revenue Change?",
    y = "# of Restaurants") +
  guides(fill = FALSE) # Remove legend 

# Combine the four plots above into a "dashboard"

gridExtra::grid.arrange(plot_1, plot_2, plot_3, plot_4, ncol = 2, top = textGrob("How did Number of Sales, Gross Revenue, Gross Expenses and Net Revenue Change \nfor Ontario Restaurant Owners between March 2020 to March 2021?")) 


## Change in Employees ##

# Plot for how number of employees changed 

number_of_employees <- sampling_frame_restaurant_population %>%
  ggplot(mapping = aes(x = Q3, fill = Q3)) + # Create a plot with number of employees (Q3) on the x-axis
  geom_bar() + # Create bar plot 
  theme_minimal() + # Clean theme 
  labs( # Add labels 
    title = "Majority of Ontario Restaurants Kept the Same Number of Employees between March 2020 to March 2021",
    subtitle = "Most Ontario restaurants maintained the same number of employees throughout the pandemic, \nbut about 18% of restaurants reduced their staffing in the same time period",
    caption = "(data from the 'A+ survey')",
    x = "How did Number of Employees Change?",
    y = "Number of Restaurants") +
  guides(fill = FALSE) # Remove legend 

number_of_employees


## Access to Delivery Apps ##

# Plot for restaurants with access to food delivery/take-out platforms 

sampling_frame_restaurant_population %>%
  ggplot(mapping = aes(x = Q15)) + # Create a plot with access to delivery platform (Q15) on x-axis 
  geom_bar(fill = "darkgreen") + # Create bar plot 
  theme_minimal() + # Clean theme 
  guides(fill = FALSE) + # Remove legend 
  labs( # Add labels 
    title = "Majority of Ontario Restaurants had Access to a Delivery/Takeout App \n(i.e., UberEats, Doordash, etc.) between March 2020 to March 2021",
    subtitle = "About 80% of Ontario Restaurants could access online delivery \nplatforms during the pandemic.",
    caption = "(data from the 'A+ survey')",
    x = "Did your Restaurant have Access to a Food Delivery/Take-Out App?",
    y = "Number of Restaurants") 


## How Long Current Operations Can Continue ##

# Plot for how long restaurants can operate before considering permanent closure or bankruptcy 

# Custom order Q11 column so its in the correct order 
reordered_data <-
  sampling_frame_restaurant_population

reordered_data$Q11 <- factor(reordered_data$Q11,
                             levels = c("Don't Know", "Not considering permanent closure or bankrupcy", "12 months or more", "6 months to less than 12 months", "3 months to less than 6 months", "1 month to less than 3 months", "Less than 1 month"))

# Create plot 
reordered_data %>%
  ggplot(mapping = aes(x = Q11)) + # Create plot with survival time (Q11) on the x-axis 
  geom_bar(fill = "darkgreen") + # Create bar plot 
  theme_minimal() + # Clean theme 
  guides(fill = FALSE) + # Remove legend 
  labs( # Add labels 
    title = "Vast Majority of Ontario Restaurants Will Not Survive \nAnother Year (March 2021)",
    subtitle = "Ontario Restaurants are Desperate with Respondents either \nplanning permanent shutdown or filing for bankrupcy in the next \nyear if their current revenues and expenses continue at their \ncurrent levels.",
    caption = "(data from the 'A+ survey')",
    x = "Restaurant Planning Permanent Closure or Bankrupcy?",
    y = "Number of Restaurants") +
  coord_flip()  # Flip the axes 


# Check if Treatment and Control Groups are Representative Samples ###############################################################################################################################################################################################################################################################################################################################################3##############################################################

# Check if the treatment and control group are representative by grouping the dataset by groups 1 and 2, and by Q15 (access to a delivery platform) 

restaurant_delivery_table <-
  simulated_dataset %>% 
  group_by(group, Q15) %>%
  count() %>%
  rename("Access to Delivery Service and Takeout Apps" = "Q15", 
         "Number of Restaurants" = "n",
         "Group" = "group") 


# Create table to show the numbers for each response in each group 

restaurant_delivery_table %>%
  knitr::kable(caption = "Number of Restaurants who have Access to a Delivery or Takeout Platform per Group") %>%
  kableExtra::kable_styling()

# Run a t-test to show that the treatment and control group are not statistically signifciant meaning the data is distributed and they are good representatives of the population 

simulated_dataset_delivery <-
  simulated_dataset %>%
  mutate(delivery_as_integer = case_when(
    Q15 == "No" ~ 0,
    Q15 == "Yes" ~ 1,
    TRUE ~ 999
  ))

group_1 <- 
  simulated_dataset_delivery %>%
  filter(group == 1) %>%
  select(delivery_as_integer) %>%
  as.vector() %>%
  unlist()

group_2 <- 
  simulated_dataset_delivery %>%
  filter(group == 2) %>%
  select(delivery_as_integer) %>%
  as.vector() %>%
  unlist()

# T-Test 
tidy(t.test(group_1, group_2))


# Linear Regression for Contact-Traced COVID-19 Cases ###########################################################################################################################################################################################

## Linear Regression ##

# Turn "group" into a factor 
simulated_dataset$group <- as.factor(simulated_dataset$group)

# Run a linear regression for DV "contact_traced_covid_cases" with IV "Group" and CVs "Q15" and "Q13" 
lm_restaurant_covidcases_model <-
  lm(contact_traced_covid_cases ~ group + Q12 + Q13,
     data = simulated_dataset) 

summary(lm_restaurant_covidcases_model)


# Logistic Regression to Predict if a Restaurant is Open for Indoor Dining ######################################################################################################################################################################

glm_restaurant_covidcases_model <-
  glm(group ~ contact_traced_covid_cases + Q12 + Q13,
      data = simulated_dataset,
      family = 'binomial') 

summary(glm_restaurant_covidcases_model)

# Visualizing the Relationship between the Indoor Dining and the Number of Contact-Traced COVID-19 Cases ########################################################################################################################################

# Graph of number of covid cases for restaurants with indoor dining vs. non-indoor dining 

simulated_dataset %>% 
  ggplot(aes(x = contact_traced_covid_cases, 
             fill = group)) + # Create graph with contact_traced_covid_cases on the x-axis and filled by the treatment/control group 
  geom_histogram(position = "dodge", # Create a histogram with a dodge position and bins 0.5 values wide 
                 binwidth = 0.5) +
  theme_minimal() + # Clean theme 
  labs( # Add labels 
    title = "Restaurants with In-Door Dining Have Significantly More \nContact-Traced COVID-19 Cases",
    x = "Number of Contact-Traced COVID-19 Cases",
    caption = "(data from the 'A+ survey')",
    y = "Number of restaurants",
    fill = "Restaurant received treatment") +
  scale_fill_brewer(palette = "Set1") # Use the Set1 palette 








