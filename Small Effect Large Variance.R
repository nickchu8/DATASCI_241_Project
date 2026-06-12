library(ggplot2)

set.seed(123)

# Small effect size, large variance scenario
effect_size <- 5
control_mean <- 60
sd_wall_sit <- 45

num_simulations <- 1000
sample_sizes <- seq(20, 300, by = 20)

power_estimates <- rep(NA, length(sample_sizes))

for(i in seq_along(sample_sizes)) {
  
  n_total <- sample_sizes[i]
  n_treat <- round(n_total / 2)
  n_control <- n_total - n_treat
  
  p_values <- replicate(
    num_simulations,
    {
      treatment_group <- rnorm(
        n_treat,
        mean = control_mean + effect_size,
        sd = sd_wall_sit
      )
      
      control_group <- rnorm(
        n_control,
        mean = control_mean,
        sd = sd_wall_sit
      )
      
      sample_data <- data.frame(
        wall_sit_time = c(treatment_group, control_group),
        external_focus = c(rep(1, n_treat), rep(0, n_control))
      )
      
      t.test(
        wall_sit_time ~ external_focus,
        data = sample_data
      )$p.value
    }
  )
  
  power_estimates[i] <- mean(p_values < 0.05)
}

power_df <- data.frame(
  sample_size = sample_sizes,
  power = power_estimates
)

power_df

ggplot(power_df, aes(x = sample_size, y = power)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.80, linetype = "dashed") +
  labs(
    title = "Power Analysis: Small Effect Size and Large Variance",
    x = "Sample Size",
    y = "Power"
  )