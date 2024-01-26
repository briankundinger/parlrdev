library(tidyverse)

result_files <- list.files(path = "SEI_sensitivity/out/sim_acc", full.names = T)
results <- lapply(result_files, readRDS) %>%
  do.call(rbind, .) %>%
  as.data.frame()

results_df <- results %>%
  select(-RR, -time) %>%
  filter(method %in% c("vabl", "fabl_resolved", "BRL")) %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  group_by(method, metric, error, overlap) %>%
  summarize(avg = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975),
            .groups = "drop") %>%
  mutate(metric = factor(metric, c("recall", "precision", "f-Measure")),
         error = factor(error, c("One Error", "Two Errors", "Three Errors")),
         overlap = case_when(
           overlap == 50 ~ "10% Overlap",
           overlap == 250 ~ "50% Overlap",
           overlap == 450 ~ "90% Overlap"
         ),
         method = case_when(
           method == "vabl" ~ "vabl",
           method == "BRL" ~ "BRL",
           method == "fabl_resolved" ~ "fabl"
         ))

acc_plot <- results_df %>%
  ggplot(aes(x = metric, y = avg,
             ymin  = lower, ymax = upper,
             color = method)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .3) +
  facet_grid(overlap ~ error) +
  labs(x = NULL,
       y = NULL,
       color = "Method") +
  theme_bw(base_size = 7) +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_brewer(palette="Set1")

acc_plot

# ggsave(filename = "notes/figures/vabl_accuracy.png",
#        plot = acc_plot)

# ggsave(filename = "notes/figures/sadinle_sim_plot3.png",
#        plot = acc_plot)

results_partial_df <- results %>%
  mutate(DR = 1 - RR) %>%
  select(-RR, -time) %>%
  filter(method %in% c("fabl_partial", "BRL_partial", "vabl_partial")) %>%
  pivot_longer(cols = c(1, 2, 3, 7), names_to = "metric") %>%
  group_by(method, metric, error, overlap) %>%
  summarize(avg = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975),
            .groups = "drop") %>%
  filter(metric != "f-measure") %>%
  mutate(error = factor(error, c("One Error", "Two Errors", "Three Errors")),
         overlap = case_when(
           overlap == 50 ~ "10% Overlap",
           overlap == 250 ~ "50% Overlap",
           overlap == 450 ~ "90% Overlap"
         ),
         method = case_when(
           method == "BRL_partial" ~ "BRL",
           method == "fabl_partial" ~ "fabl",
           method == "vabl_partial" ~ "vabl"
         ),
         metric = case_when(
           metric == "recall" ~ "NPV",
           metric == "precision" ~ "PPV",
           metric == "DR" ~ "DR"
         ),
         metric = factor(metric, c("NPV", "PPV", "DR")))

acc_plot_partial <- results_partial_df %>%
  ggplot(aes(x = metric, y = avg,
             ymin  = lower, ymax = upper,
             color = method)) +
  geom_pointrange(position = position_dodge2(width = .5),
                  size = .3) +
  facet_grid(overlap ~ error) +
  labs(x = NULL,
       y = NULL,
       color = "method") +
  theme_bw(base_size = 8) +
  theme(plot.title = element_text(hjust = .5),
        plot.subtitle = element_text(hjust = .5)) +
  scale_color_brewer(palette="Set1")

acc_plot_partial

ggsave(filename = "notes/figures/sadinle_sim_plot_partial_vabl.png",
       plot = acc_plot_partial)
