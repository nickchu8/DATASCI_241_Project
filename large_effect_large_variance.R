library(data.table)
library(ggplot2)

set.seed(1234)

# start off by making a data frame with control and treatment data
# columns: focus_of_attention, 

df <- data.table(id = 1:300)

df[ , ':='(
  y0 = rnorm(.N, mean = 96, sd = 45),
  tau_external = rnorm(.N, mean = 15)
)]

df[ , focus_of_attention := sample(c('internal', 'external'), 
                                   size = .N, replace = TRUE)]

df[focus_of_attention == 'internal', Y := rnorm(.N, mean = 96, sd = 45)]
df[focus_of_attention == 'external', Y := rnorm(.N, mean = 111, sd = 45)]

# df[ , .(group_mean = mean(Y)), keyby = .(focus_of_attention)]

samples <- seq(20, 500, by = 20)

length_original <- length(df$Y)

data <- data.frame(
  sample_size = numeric(length(samples)),
  power = numeric(length(samples))
)

for (i in seq_along(samples)){
  group_size <- (samples[i] / 2) 
  data$sample_size[i] <- group_size * 2
  
  sim_p_values <- replicate(1000, {
    sample_treat   <- sample(df[df$focus_of_attention == 'external']$Y, 
                             size = group_size, 
                             replace = TRUE)
    sample_control <- sample(df[df$focus_of_attention == 'internal']$Y, 
                             size = group_size, 
                             replace = TRUE)
    
    t_test_sim <- t.test(sample_treat, sample_control)
    
    return(t_test_sim$p.value)
  })
  
  data$power[i] <- mean(sim_p_values < 0.05)
  
}

ggplot(data, aes(x = sample_size, y = power)) +
  geom_line() +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "darkred", alpha = 0.7) +
  geom_point() +
  labs(
    title = "Power vs. Sample Size [Effect Size = 15, SD = 45]",
    x = "Total Sample Size (Treatment + Control)",
    y = "Power"
  )
