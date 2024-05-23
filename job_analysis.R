# Libraries!
library(tidyverse)
##########################################################################################
################################## DATA CLEANING #########################################
##########################################################################################
# Read data.
data <- read_csv("JOBSSS.csv")

# Clean up data.
data <- data %>%
  select(1:10) %>%
  filter(`Applied?` == 'Y') %>%
  select(-`Applied?`) %>%
  mutate(`Date Applied` = as.Date(`Date Applied`, format = "%m/%d/%Y")) %>%
  rename(job = JOB, company = Company, location = Location,
         date = `Date Applied`, app = Application, quick = `Quick Apply?`,
         cover_letter = `Cover Letter`, salary = Salary, source = Source) %>%
  mutate(ones_col = 1) %>%
  mutate(cumulative_row_n = cumsum(ones_col))
##########################################################################################
################################# CUMULATIVE APPS GRAPH ##################################
##########################################################################################
# Cumulative Application graph.
cum_apps_plot <- ggplot(data, aes(x = date, y = cumulative_row_n)) +
  geom_line(color = "#0072B2", linewidth = 1) +
  labs(title = "Cumulative applications over time",
       subtitle = "u/i_oper8 job hunt",
       x = "Date", y = "Cumulative Applications") +
  theme_minimal() +
  annotate("text", x = max(data$date), y = max(data$cumulative_row_n) - 25,
           label = nrow(data), hjust = 1, vjust = 1, color = "#0072B2", fontface = "bold",
           size = 6) +
  geom_vline(xintercept = as.Date("2024-03-21"), color = "#D73027",
             linetype = "dashed", size = 1) +
  annotate("text", x = as.Date("2024-03-21") - 2, y = max(data$cumulative_row_n) / 2,
           label = "Final\nApp. Submitted\n3/21", hjust = 1, vjust = 1, color = "#D73027", fontface = "bold", size = 3) +
  annotate("text", x = max(data$date), y = max(data$cumulative_row_n) - 40,
           label = "Total Apps.", hjust = 1, vjust = 1, color = "#0072B2", fontface = "bold",
           size = 3) +
  theme(plot.title = element_text(size = 20))
##########################################################################################
############################### JOB TITLE HIST ###########################################
##########################################################################################
# Job Title histogram.
# Count top 5 job titles.
top_titles <- data %>%
  count(job) %>%
  top_n(5, wt=n) %>%
  arrange(desc(n))

# top jobs account for?
tops_account_for <- round(sum(top_titles$n) / nrow(data), 2)

# x-ticks
jobs_explicit <- c("Data Analyst", "Data Scientist", "Machine Learning Engineer",
                   "Software Engineer", "Data Engineer")

# Plot.
titles_plot <- ggplot(top_titles, aes(x = reorder(job, -n), y=n)) +
  geom_bar(stat = "identity", fill = "#0072B2") +
  labs(x = "Titles", y = "Frequency",
       title = "Top 5 job titles",
       subtitle = "u/i_oper8 job hunt") +
  theme_minimal() +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4, fontface = "bold") +
  theme(plot.title = element_text(size = 20),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = jobs_explicit) +
  annotate("text", x = which(jobs_explicit == "Software Engineer"), y = 101,
           label = paste("Top 5 titles\naccounted for:",
                         paste0(format(tops_account_for * 100, nsmall = 0),
                                "%"), "\n of titles or", sum(top_titles$n), "jobs"),
           color = "#D73027", fontface = "bold", size = 5)
##########################################################################################
################################## TOP LOC PIE ###########################################
##########################################################################################
# Top locations pie graph.
top_locations <- data %>%
  count(location) %>%
  arrange(desc(n))

data

##########################################################################################
################################ SALARY DIST #############################################
##########################################################################################
# Salary Distribution
salary_data <- data %>%
  filter(salary != "N/A")

# Get lower bound if it's a range
extract_lower_bound <- function(value) {
  ifelse(str_detect(value, "-"),
         str_extract(value, "\\d*\\.?\\d+k(?=-)"),
         value)
}
salary_data <- salary_data %>%
  mutate(lower_bound = extract_lower_bound(salary))

# Fixing a row
salary_data[182, "lower_bound"] <- "57k"

# Getting lower bounds to numeric
salary_data <- salary_data %>%
  mutate(lower_bound = as.numeric(str_remove(lower_bound, "k")) * 1000)

# mean
mean_salary_value <- mean(salary_data$lower_bound, na.rm = TRUE)
final_sal <- 46000

# x ticks
breaks <- seq(0, 200000, by = 50000)
labels <- paste0("$", breaks/1000, "k")

# Plotting
sal_plot <- ggplot(salary_data, aes(x = lower_bound)) +
  geom_histogram(binwidth = 15000, fill = "#0072B2", color = "black") +
  labs(title = "Distribution of salaries",
       x = "Salary (in USD)",
       y = "Frequency",
       subtitle = "u/i_oper8 job hunt") +
  geom_density() +
  geom_vline(xintercept = mean_salary_value,
             linetype = "dashed",
             color = "#D73027", size = 1.5) +
  annotate("text", x = mean_salary_value + 20000, y = 48,
           label = paste("Mean:", paste0("$", format(round(mean_salary_value / 1000), nsmall = 0), "k")),
           color = "#D73027", fontface = "bold", size = 5) +
  geom_vline(xintercept = final_sal,
             linetype = "dashed",
             color = "#FF8C00", size = 1.5) +
  annotate("text", x = final_sal - 20000, y = 48,
           label = paste("Final:", paste0("$", format(round(final_sal / 1000), nsmall = 0), "k")),
           color = "#FF8C00", fontface = "bold", size = 5) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20))
##########################################################################################
##########################################################################################


