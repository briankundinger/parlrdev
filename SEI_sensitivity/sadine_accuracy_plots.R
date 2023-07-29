library(tidyverse)

result_files <- list.files(path = "SEI_sensitivity/out/sim_acc", full.names = T)
results <- lapply(result_files, readRDS) %>%
  do.call(rbind, .) %>%
  as.data.frame()

results_df <- results %>%
  select(-RR, -Time_Elapsed) %>%
  filter(Method %in% c("fabl_resolved", "BRL")) %>%
  pivot_longer(cols = 1:3, names_to = "Metric") %>%
  group_by(Method, Metric, Error, Overlap) %>%
  summarize(avg = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975),
            .groups = "drop") %>%
  mutate(Metric = factor(Metric, c("Recall", "Precision", "F-Measure")),
         Error = factor(Error, c("One Error", "Two Errors", "Three Errors")),
         Overlap = case_when(
           Overlap == 50 ~ "10% Overlap",
           Overlap == 250 ~ "50% Overlap",
           Overlap == 450 ~ "90% Overlap"
         ),
         Method = case_when(
           Method == "BRL" ~ "BRL",
           Method == "fabl_resolved" ~ "fabl"
         ))

acc_plot <- results_df %>%
  ggplot(aes(x = Metric, y = avg,
             ymin  = lower, ymax = upper,
             color = Method)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .3) +
  facet_grid(Overlap ~ Error) +
  labs(x = NULL,
       y = NULL,
       color = "Method") +
  theme_bw(base_size = 7) +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_brewer(palette="Set1")

acc_plot

ggsave(filename = "notes/figures/sadinle_sim_plot3.png",
       plot = acc_plot)

results_partial_df <- results %>%
  mutate(DR = 1 - RR) %>%
  select(-RR, -Time_Elapsed) %>%
  filter(Method %in% c("fabl_partial", "BRL_partial")) %>%
  pivot_longer(cols = c(1, 2, 3, 7), names_to = "Metric") %>%
  group_by(Method, Metric, Error, Overlap) %>%
  summarize(avg = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975),
            .groups = "drop") %>%
  filter(Metric != "F-Measure") %>%
  mutate(Error = factor(Error, c("One Error", "Two Errors", "Three Errors")),
         Overlap = case_when(
           Overlap == 50 ~ "10% Overlap",
           Overlap == 250 ~ "50% Overlap",
           Overlap == 450 ~ "90% Overlap"
         ),
         Method = case_when(
           Method == "BRL_partial" ~ "BRL",
           Method == "fabl_partial" ~ "fabl"
         ),
         Metric = case_when(
           Metric == "Recall" ~ "NPV",
           Metric == "Precision" ~ "PPV",
           Metric == "DR" ~ "DR"
         ),
         Metric = factor(Metric, c("NPV", "PPV", "DR")))

acc_plot_partial <- results_partial_df %>%
  ggplot(aes(x = Metric, y = avg,
             ymin  = lower, ymax = upper,
             color = Method)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .3) +
  facet_grid(Overlap ~ Error) +
  labs(x = NULL,
       y = NULL,
       color = "Method") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_brewer(palette="Set1")

acc_plot_partial

ggsave(filename = "notes/figures/sadinle_sim_plot_partial2.png",
       plot = acc_plot_partial)
