## MIDTERM + FINAL GRADE PROJECTION

library(pacman)
p_load(readr, dplyr, ggplot2, ggthemes)

## Load Data ----

grades <- read_csv("EC-311-Midterm-Grades.csv")
grades <- grades %>%
  filter(midterm != "NA") %>%
  mutate(
    midterm_nom = (midterm/80)*100,
    mid_curve = sqrt(midterm_nom)*10,
    course_curve = sqrt(final_score)*10,
    mid_grade = ifelse(mid_curve >= mid_a, "Gold", 
                       ifelse(mid_curve >= mid_b, "Silver",
                              ifelse(mid_curve >= mid_c, "Bronze", "Needs Improvement"))),
    course_grade = ifelse(final_score >= course_a, "A", 
                          ifelse(final_score >= course_b, "B", 
                                 ifelse(final_score >= course_c, "C", "D")))
  ) %>%
  group_by(mid_grade) %>%
  mutate(
    min_score = min(midterm)
  ) %>%
  ungroup() %>%
  group_by(course_grade) %>%
  mutate(
    min_grade = min(final_score)
  ) %>%
  ungroup() %>%
  select(
    student, course_grade, mid_curve, mid_grade, 
    min_score, min_grade, midterm, final_score, 
    everything()
  )


  

## Midterm Grades ----

# Cutoffs (Created strictly from percentiles)
mid_mean <- mean(grades$mid_curve, na.rm = TRUE)
mid_a <- quantile(grades$mid_curve, probs = c(0.9))
mid_b <- quantile(grades$mid_curve, probs = c(0.4))
mid_c <- quantile(grades$mid_curve, probs = c(0.2))


midterm_grades <- ggplot(grades) + 
  geom_rect(aes(xmin = 10, xmax = 30, ymin = 0, ymax = Inf), fill = "#C85537", alpha = 0.25) + 
  geom_rect(aes(xmin = 30, xmax = 40, ymin = 0, ymax = Inf), fill = "#cd7f32", alpha = 0.25) + 
  geom_rect(aes(xmin = 40, xmax = 60, ymin = 0, ymax = Inf), fill = "#C0C0C0", alpha = 0.25) + 
  geom_rect(aes(xmin = 60, xmax = Inf, ymin = 0, ymax = Inf), fill = "gold") + 
  geom_histogram(aes(x = midterm), binwidth = 2, fill = "darkgreen", color = "black") +
  labs(
    title = "Midterm Score Placement", 
    x = "Midterm Score", 
    y = "Count"
    ) + 
  scale_x_continuous(breaks = seq(10, 100, by = 5)) + 
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggsave('midterm-grade-distribution.jpg', plot = midterm_grades,
       width = 25, height = 20, units = 'cm', dpi = 350)

## Course Grades ----

# Cutoffs (Created after observing percentile breaking points)
course_a <- quantile(grades$mid_curve, probs = c(0.9))
course_b <- quantile(grades$mid_curve, probs = c(0.4))
course_c <- quantile(grades$mid_curve, probs = c(0.2))

## Course Grade Projection (Not All Participation Included)
course_gg <- ggplot(grades) + 
  geom_rect(aes(xmin = 40, xmax = 60, ymin = 0, ymax = Inf), fill = "#C85537", alpha = 0.25) + 
  geom_rect(aes(xmin = 60, xmax = 70, ymin = 0, ymax = Inf), fill = "#cd7f32", alpha = 0.25) + 
  geom_rect(aes(xmin = 70, xmax = 86, ymin = 0, ymax = Inf), fill = "#C0C0C0", alpha = 0.25) + 
  geom_rect(aes(xmin = 86, xmax = Inf, ymin = 0, ymax = Inf), fill = "gold") + 
  geom_histogram(aes(x = final_score), binwidth = 2, fill = "darkgreen", color = "black") +
  labs(
    title = "Nominal Course Scores", 
    x = "Score", 
    y = "Count"
  ) + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(10, 100, by = 5)) + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )
ggsave('course-grade-projection.jpg', plot = course_gg,
       width = 25, height = 20, units = 'cm', dpi = 350)
