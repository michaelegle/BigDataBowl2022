###### model analysis #######

library(paletteer)

###### RIGHT

blocking_rf_right <- readRDS("blocking_model_right.rds")

importance_right <- as.data.frame(blocking_rf_right$importance)

importance_right$variable <- rownames(importance_right)

importance_right <- importance_right %>%
  mutate(variable = str_remove(variable, "V")) %>%
  separate("variable", into = c("x", "y"), sep = "_")

importance_right <- importance_right %>%
  mutate(x = as.numeric(x),
         y = as.numeric(y))

importance_right %>%
  ggplot(aes(x = x, y = y, fill = IncNodePurity)) +
  geom_tile() +
  coord_fixed() +
  ylim(0, 54) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  labs(title = "Importance of Block Locations - Punts Landing in the Right Side of the Field",
       subtitle = "Each square is one square yard | Darker Shade = Higher Importance",
       x = "Yards Downfield") +
  theme_bw() +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_hline(yintercept = 160/3) +
  geom_hline(yintercept = 0) +
  annotate(geom = "text", x = -2, y = 25, label = "Line Of Scrimmage", angle = 90) -> right_plot

right_plot

##### CENTER

blocking_rf_center <- readRDS("blocking_model_center.rds")

importance_center <- as.data.frame(blocking_rf_center$importance)

importance_center$variable <- rownames(importance_center)

importance_center <- importance_center %>%
  mutate(variable = str_remove(variable, "V")) %>%
  separate("variable", into = c("x", "y"), sep = "_")

importance_center <- importance_center %>%
  mutate(x = as.numeric(x),
         y = as.numeric(y))

importance_center %>%
  ggplot(aes(x = x, y = y, fill = IncNodePurity)) +
  geom_tile() +
  coord_fixed() +
  ylim(0, 54) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  labs(title = "Importance of Block Locations - Punts Landing in the Center of the Field",
       subtitle = "Each square is one square yard | Darker Shade = Higher Importance",
       x = "Yards Downfield") +
  theme_bw() +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_hline(yintercept = 160/3) +
  geom_hline(yintercept = 0) +
  annotate(geom = "text", x = -2, y = 25, label = "Line Of Scrimmage", angle = 90) -> center_plot


##### LEFT

blocking_rf_left <- readRDS("blocking_model_left.rds")

importance_left <- as.data.frame(blocking_rf_left$importance)

importance_left$variable <- rownames(importance_left)

importance_left <- importance_left %>%
  mutate(variable = str_remove(variable, "V")) %>%
  separate("variable", into = c("x", "y"), sep = "_")

importance_left <- importance_left %>%
  mutate(x = as.numeric(x),
         y = as.numeric(y))

importance_left %>%
  ggplot(aes(x = x, y = y, fill = IncNodePurity)) +
  geom_tile() +
  coord_fixed() +
  ylim(0, 54) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "black") +
  labs(title = "Importance of Block Locations - Punts Landing in the Left Side of the Field",
       subtitle = "Each square is one square yard | Darker Shade = Higher Importance",
       x = "Yards Downfield") +
  theme_bw() +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_hline(yintercept = 160/3) +
  geom_hline(yintercept = 0) +
  annotate(geom = "text", x = -2, y = 25, label = "Line Of Scrimmage", angle = 90) -> left_plot


center_plot
right_plot
left_plot

ggsave("center_block_plot.png", center_plot, width = 7, height = 6, bg = "transparent")
ggsave("right_block_plot.png", right_plot, width = 7, height = 6, bg = "transparent")
ggsave("left_block_plot.png", left_plot, width = 7, height = 6, bg = "transparent")

right_block %>%
  pivot_longer(cols = V0_1:V39_53,
               names_to = "var",
               values_to = "block") %>%
  group_by(var) %>%
  summarize(block_freq = sum(block)) %>%
  separate("var", into = c("x", "y"), sep = "_") %>%
  mutate(x = as.numeric(str_remove(x, "V")),
         y = as.numeric(y)) %>%
  ggplot(aes(x = x, y = y, fill = block_freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme_bw() +
  coord_fixed() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_hline(yintercept = 160/3) +
  geom_hline(yintercept = 0) +
  annotate(geom = "text", x = -2, y = 25, label = "Line Of Scrimmage", angle = 90) +
  labs(x = "Yards Downfield",
       title = "Block Frequency on Punts in the Right Side of the Field",
       subtitle = "Darker Shade of Blue Indicates Higher Frequency",
       fill = "Frequency") -> right_block_freq_plot

center_block %>%
  pivot_longer(cols = V0_1:V39_53,
               names_to = "var",
               values_to = "block") %>%
  group_by(var) %>%
  summarize(block_freq = sum(block)) %>%
  separate("var", into = c("x", "y"), sep = "_") %>%
  mutate(x = as.numeric(str_remove(x, "V")),
         y = as.numeric(y)) %>%
  ggplot(aes(x = x, y = y, fill = block_freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme_bw() +
  coord_fixed() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_hline(yintercept = 160/3) +
  geom_hline(yintercept = 0) +
  annotate(geom = "text", x = -2, y = 25, label = "Line Of Scrimmage", angle = 90) +
  labs(x = "Yards Downfield",
       title = "Block Frequency on Punts in the Center of the Field",
       subtitle = "Darker Shade of Blue Indicates Higher Frequency",
       fill = "Frequency") -> center_block_freq_plot

left_block %>%
  pivot_longer(cols = V0_1:V39_53,
               names_to = "var",
               values_to = "block") %>%
  group_by(var) %>%
  summarize(block_freq = sum(block)) %>%
  separate("var", into = c("x", "y"), sep = "_") %>%
  mutate(x = as.numeric(str_remove(x, "V")),
         y = as.numeric(y)) %>%
  ggplot(aes(x = x, y = y, fill = block_freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "blue") +
  theme_bw() +
  coord_fixed() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_hline(yintercept = 160/3) +
  geom_hline(yintercept = 0) +
  annotate(geom = "text", x = -2, y = 25, label = "Line Of Scrimmage", angle = 90) +
  labs(x = "Yards Downfield",
       title = "Block Freuency on Punts in the Left Side of the Field",
       subtitle = "Darker Shade of Blue Indicates Higher Frequency",
       fill = "Frequency") -> left_block_freq_plot

ggsave("center_block_freq_plot.png", center_block_freq_plot, width = 8, height = 6, bg = "transparent")
ggsave("right_block_freq_plot.png", right_block_freq_plot, width = 8, height = 6, bg = "transparent")
ggsave("left_block_freq_plot.png", left_block_freq_plot, width = 8, height = 6, bg = "transparent")
