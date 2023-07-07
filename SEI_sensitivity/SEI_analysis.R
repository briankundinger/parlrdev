
R_vals <- c(1, 2, 5, 10, 20, "No SEI") %>%
  factor(., levels = .)
Z_paths <- list.files(path = "SEI_sensitivity/out/Zs", full.names = T)

Zs_list <- lapply(Z_paths, readRDS)
names(Zs_list) <- R_vals

Zs <- lapply(Zs_list, function(x){
  dim(unique(x, MARGIN = 2))[2]
})

Zs <- Zs %>%
  unlist() %>%
  data.frame(R_vals, .) %>%
  mutate(metric = "Zs",
         stat = "distinct")



names(Zs) <- c("R", "value", "metric", "stat")
rownames(Zs) <- NULL

Z_plot <-  Zs %>%
  ggplot() +
  aes(x = R, y = value, group = 1) +
  geom_line(size = .5) +
  theme_bw(base_size = 6) +
  labs(y = "Distinct Z Estimates", x = "S")

Z_plot

ggsave(plot = Z_plot, filename = "SEI_sensitivity/Z_plot.png")

eval_paths <- list.files(path = "SEI_sensitivity/out/eval", full.names = T)
eval_list <- lapply(eval_paths, readRDS)

thing <- map2(eval_list, R_vals, ~data.frame(t(.x), .y)) %>%
  do.call(rbind, .)
names(thing) <- c("recall", "precision", "fmeasure", "R")

eval_summary <- thing %>%
  pivot_longer(cols = 1:3, names_to = "metric") %>%
  group_by(metric, R) %>%
  summarize(avg = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975),
            .groups = "drop")

eval_plot <- eval_summary %>%
  filter(metric != "fmeasure") %>%
  ggplot(aes(x = R, y = avg,
             ymin  = lower, ymax = upper)) +
  geom_pointrange(size = .2) +
  facet_wrap(~metric) +
  theme_bw(base_size = 6) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))+
  labs(y = "Value", x = "S")


eval_plot

ggsave(plot = eval_plot, filename = "SEI_sensitivity/eval_plot.png")






# recall_mean <- lapply(eval_list, function(x){
#   mean(x[1, ])
# })
#
# recall_var <- lapply(eval_list, function(x){
#   var(x[1, ])
# })
#
# recall_mean <- recall_mean %>%
#   unlist() %>%
#   data.frame() %>%
#   rownames_to_column %>%
#   mutate(metric = "recall",
#          stat = "mean")
#
# recall_var <- recall_var %>%
#   unlist() %>%
#   data.frame() %>%
#   rownames_to_column %>%
#   mutate(metric = "recall",
#          stat = "variance")
#
# names(recall_mean) <- c("R", "value", "metric", "stat")
# names(recall_var) <- c("R", "value", "metric", "stat")
#
# precision_var <- lapply(eval_list, function(x){
#   var(x[2, ])
# })
#
# precision_mean <- lapply(eval_list, function(x){
#   mean(x[2, ])
# })
#
# precision_var <- precision_var %>%
#   unlist() %>%
#   data.frame() %>%
#   rownames_to_column %>%
#   mutate(metric = "precision",
#          stat = "variance")
#
# precision_mean <- precision_mean %>%
#   unlist() %>%
#   data.frame() %>%
#   rownames_to_column %>%
#   mutate(metric = "precision",
#          stat = "mean")
#
# names(precision_var) <- c("R", "value", "metric", "stat")
# names(precision_mean) <- c("R", "value", "metric", "stat")
#
# results <- rbind(Zs, recall_mean, recall_var, precision_mean, precision_var)
#
# mean_plot <- results %>%
#   filter(stat == "mean") %>%
#   mutate(R = factor(R, levels =R_vals)) %>%
#   ggplot() +
#   aes(x = R, y = value, group = 1) +
#   geom_line() +
#   facet_wrap(~metric, scales = "free", dir = "v") +
#   labs(y = "Mean", x = "S")
#
# mean_plot
#
# variance_plot <- results %>%
#   filter(stat == "variance") %>%
#   mutate(R = factor(R, levels =R_vals)) %>%
#   ggplot() +
#   aes(x = R, y = value, group = 1) +
#   geom_line() +
#   facet_wrap(~metric, scales = "free", dir = "v") +
#   labs(y = "Variance", x = "S")
#
# variance_plot
