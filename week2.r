library(tidyverse)
library(readxl)
library(scales)

data <- read_xlsx("data/tidy_tuesday_week2.xlsx")

# tidy data
data <- data %>% gather(position, salary, -year)

# add offensive/defensive position label
data <- data %>% 
  mutate(ofdef = case_when((position %in% c("Running Back",
                                           "Quarterback",
                                           "Offensive Lineman",
                                           "Tight End",
                                           "Wide Receiver"))~"O",
                           TRUE~"D"))

# top paid players by position and year
top <- data %>% 
  group_by(position, year) %>% 
  top_n(16, salary)

# represent salaries in millions as there appears no easy way of formatting
# big dollar values using library(scales)
div = 1e6

# add salary rank
top <- top %>% 
  mutate(rank = dense_rank(desc(salary)))

# plot evolution of top salaries
ggplot(top, aes(year, salary/div)) +
  geom_line(aes(group=rank, alpha=1/rank)) +
  geom_smooth(aes(year, salary/div), span=0.3, se=FALSE, linetype=1) +
  facet_wrap(ofdef~position, nrow = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  theme_minimal() +
  ggtitle("Evolution of top salaries by position") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(alpha=FALSE) +
  scale_y_continuous(labels = dollar_format(suffix="m")) +
  xlab("Year") +
  ylab("Salary") +
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, size=1)

ggsave("img/week2.png", width=20, height=10, units="cm")
