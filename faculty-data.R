# Generate random salary data

# Set up
library(dplyr) # data wrangling
library(lme4) # modeling
library(ggplot2) # visualization

# Parameters
departments <- c('sociology', 'biology', 'english', 'informatics', 'statistics')
base.salaries <- c(40000, 50000, 60000, 70000, 80000)
constant.base <- mean(base.salaries)
annual.raises <- c(2000, 500, 500, 1700, 500)
constant.raise <- mean(annual.raises)
faculty.per.dept <- 20
total.faculty <- faculty.per.dept * length(departments)

# Generate dataframe of faculty and (random) years of experience
ids <- 1:total.faculty
department <- rep(departments, faculty.per.dept)
experience <- floor(runif(total.faculty, 1, 10))
bases <- rep(base.salaries, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
raises <- rep(annual.raises, faculty.per.dept) * runif(total.faculty, .9, 1.1) # noise
df <- data.frame(ids, department, bases, experience, raises)

# Generate random salaries: constant base and raise (no diff. by department)
df <- df %>% mutate(
            constant.salary = constant.base + experience * constant.raise, 
            varying.base = bases + experience * constant.raise, 
            varying.raise = constant.base + experience * raises, 
            varying.base.raise = bases + experience * raises
            )

# Model with varying intercept
m1 <- lmer(varying.base.raise ~ experience + (1|department), data = df)
df$random.intercpet.preds <- predict(m1)

# Model with varying slope
m2 <- lmer(varying.base.raise ~ experience + (0 + experience|department), data=df)
df$random.slope.preds <- predict(m2)

# Model with varying slope and intercept
m3 <- lmer(varying.base.raise ~ experience + (1 + experience|department), data=df)
df$random.slope.int.preds <- predict(m3)

# Visualize random intercept
ggplot(data=df, aes(x=experience, y=random.intercpet.preds, group = department, colour = department)) +
  geom_line() + 
  labs(x="Experience", y="Predicted Salary") +
  ggtitle("Varying Intercept Salary Prediction") + 
  scale_colour_discrete('Department')     

# Visualize random slope
ggplot(data=df, aes(x=experience, y=random.slope.preds, group = department, colour = department)) +
  geom_line() + 
  labs(x="Experience", y="Predicted Salary") +
  ggtitle("Varying Slope Salary Prediction") + 
  scale_colour_discrete('Department')

# Visualize random slope + intercept
ggplot(data=df, aes(x=experience, y=random.slope.int.preds, group = department, colour = department)) +
  geom_line() + 
  labs(x="Experience", y="Predicted Salary") +
  ggtitle("Varying Slope and Intercept Salary Prediction") + 
  scale_colour_discrete('Department')
