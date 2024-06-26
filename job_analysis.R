# Libraries!
library(tidyverse)
library(ggsankeyfier)

# colors i use
red <-  "#E31A1C"
blue <- "#1F78B4"
oj <- "#FF7F00"
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
  geom_line(color = "#1F78B4", linewidth = 1) +
  labs(title = "Cumulative applications over time",
       caption = "u/i_oper8 job hunt",
       x = NULL, y = NULL) +
  theme_minimal() +
  annotate("text", x = max(data$date), y = max(data$cumulative_row_n) - 25,
           label = nrow(data), hjust = 1, vjust = 1, color = "#1F78B4", fontface = "bold",
           size = 6) +
  geom_vline(xintercept = as.Date("2024-03-21"), color = "#E31A1C",
             linetype = "dashed", size = 1) +
  annotate("text", x = as.Date("2024-03-21") - 2, y = max(data$cumulative_row_n) / 2,
           label = "App. Submitted\nfor job I accepted\n3/21", hjust = 1, vjust = 1, color = "#E31A1C", fontface = "bold", size = 4) +
  annotate("text", x = max(data$date), y = max(data$cumulative_row_n) - 40,
           label = "Total Apps.", hjust = 1, vjust = 1, color = "#1F78B4", fontface = "bold",
           size = 4) +
  theme(plot.title = element_text(size = 20, hjust = 0.5))
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
titles_plot <- ggplot(top_titles, aes(x = reorder(job, -n), y = n, fill = reorder(job, -n))) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = NULL,
       title = "Top 5 job titles",
       caption = "u/i_oper8 job hunt") +
  theme_minimal() +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4, fontface = "bold") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  scale_x_discrete(labels = jobs_explicit) +
  scale_fill_brewer(palette = "Paired") +
  annotate("text", x = which(jobs_explicit == "Software Engineer"), y = 101,
           label = paste("Top 5 titles\naccounted for:",
                         paste0(format(tops_account_for * 100, nsmall = 0),
                                "%"), "\n of titles or", sum(top_titles$n), "jobs"),
           color = "black", fontface = "italic", size = 5)
##########################################################################################
################################## TOP LOC PIE ###########################################
##########################################################################################
# Top locations pie graph.

# Define simple locations
bay_area <- c("SF", "Fremont", "Pleasanton", "Cupertino", "San Jose",
              "Sunnyvale", "Palo Alto", "Santa Clara", "Oakland",
              "Berkeley", "Foster City", "Redwood City")
home <- c("Reno", "Carson City", "Sparks", "Incline", "Verdi", 
          "Virginia City")
la <- c("LA", "El Segundo", "Irvine", "Santa Monica", "Long Beach",
        "Culver City", "Glendale", "Marina del Rey", "City of Industry",
        "Santa Ana")
sd <- c("SD", "Carlsbad", "La Jolla", "Oceanside", "Poway")
lv <- c("LV", "Las Vegas")
ca <- c("California", "Bakersfield", "Salinas")

data <- data %>%
  mutate(simple_loc = case_when(
    location %in% bay_area ~ "Bay Area",
    location %in% home ~ "Hometown",
    location %in% la ~ "LA Area",
    location %in% sd ~ "SD Area",
    str_detect(location, "Remote") ~ "Remote",
    str_detect(location, "SF") ~ "Bay Area",
    str_detect(location, "SD") ~ "SD Area",
    is.na(location) ~ "Remote",
    location %in% lv ~ "Las Vegas",
    location %in% ca ~ "California (other)",
    TRUE ~ location
  ))

top_locations <- data %>%
  count(simple_loc) %>%
  arrange(desc(n))


loc_plot <- ggplot(top_locations, aes(x = reorder(simple_loc, -n), y = n, fill = reorder(simple_loc, -n))) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = NULL,
       title = "Job Locations",
       caption = "u/i_oper8 job hunt") +
  theme_minimal() +
  geom_text(aes(label = n), vjust = -0.5, color = "black", size = 4, fontface = "bold") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        panel.grid.major.x = element_blank(),
        legend.position = "none") +
  scale_fill_brewer(palette = "Paired")
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

# How many jobs posted sal?
n_salaries <- nrow(salary_data)
n_jobs <- nrow(data)
n_posted_sals <- n_salaries / n_jobs

# Plotting
sal_plot <- ggplot(salary_data, aes(x = lower_bound)) +
  geom_histogram(binwidth = 15000, fill = blue, color = "black") +
  labs(title = "Distribution of salaries",
       x = NULL,
       y = NULL,
       caption = "u/i_oper8 job hunt") +
  geom_density() +
  geom_vline(xintercept = mean_salary_value,
             linetype = "dashed",
             color = red, size = 1.5) +
  annotate("text", x = mean_salary_value + 20000, y = 48,
           label = paste("Mean:", paste0("$", format(round(mean_salary_value / 1000), nsmall = 0), "k")),
           color = red, fontface = "bold", size = 5) +
  geom_vline(xintercept = final_sal,
             linetype = "dashed",
             color = oj, size = 1.5) +
  annotate("text", x = final_sal - 20000, y = 48,
           label = paste("Final:", paste0("$", format(round(final_sal / 1000), nsmall = 0), "k")),
           color = oj, fontface = "bold", size = 5) +
  annotate("text", x = 200000, y = 48,
           label = paste(n_salaries, "jobs listed a salary\nor", paste0(round(n_posted_sals,2) * 100, "%"), "of applications"),
           color = "black", fontface = "italic", size = 5) +
  scale_x_continuous(breaks = breaks, labels = labels) +
  theme_minimal() +
  theme(plot.title = element_text(size = 22, hjust = 0.5))
##########################################################################################
##########################################################################################