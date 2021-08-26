ccc_func <- function(.) {
  epi.ccc(.$actual, .$estimate,  ci = "z-transform", conf.level = 0.95)
}

metrics <- sad %>% 
  nest(data = c(-method, -rater)) %>% 
  mutate(fit = map(data, ccc_func))

metric_results <- metrics %>% 
  unnest(fit) %>% 
  rowwise %>% 
  mutate(fit = toString(unlist(fit))) %>%
  group_by(method, rater) %>% 
  summarize(string = paste(fit, collapse = "_")) %>% 
  separate(col = string, 
           into = c("rho", "s_shift", "l_shift", "C_b"), 
           sep = "_") %>% 
  separate(col = rho, 
           into = c("rho", "ccc.lower", "ccc.upper"), 
           sep = ",") %>% 
  mutate_at(vars(method, rater), as.factor) %>% 
  mutate_if(is.character, as.double) %>% 
  ungroup#

metric_results %>% 
  pivot_longer(cols= c(-method, -rater), 
               names_to="stat", 
               values_to ="coef") %>% 
  separate(method, c("author","method"),  "_") %>% 
  mutate(method = fct_rev(method)) %>% 
  filter(!str_detect(stat, "ccc")) -> metric_results_long

metric_results_long %>% 
  ggplot()+
  facet_grid(author ~ stat) +
  aes(x=method, y=coef)+
  geom_boxplot()

### Concordance coefficient
# pc
library(multcomp)

mix_pc <- lmer(rho ~ method + (1 | rater), data = metric_results, REML = FALSE)
# library(lsmeans)
mean_pc <- emmeans(mix_pc, ~ method)
df_pc <- cld(mean_pc)
df_pc

mixed_model <- function(.) {
  lmer(coef ~ method + (1|rater), data = .)
}

nested_data_old <- metric_results_long %>% 
  filter(author=="old") %>% 
  nest(data = c(-stat)) 

fits_old <- nested_data_old %>% 
  mutate(model = map(data, mixed_model),
         model_anova = map(data, ~car::Anova(lmer(coef ~ method + (1|rater), .)))
  )

out_fits_old <- fits_old %>% 
  mutate(tidy_model = map(model_anova, broom::tidy)) %>% #,
  # model_qual = map(model, MuMIn::r.squaredGLMM)) %>% 
  dplyr::select(stat, tidy_model) %>%
  unnest(tidy_model) 
out_fits_old

nested_data_new <- metric_results_long %>% 
  filter(author=="new") %>% 
  nest(data = c(-stat)) 

fits_new <- nested_data_new %>% 
  mutate(model = map(data, mixed_model),
         model_anova = map(data, ~car::Anova(lmer(coef ~ method + (1|rater), .))), 
         emmeans = map(model, ~cld(emmeans(., ~ method), Letters=letters))
  )

out_fits_new <- fits_new %>% 
  mutate(
    tidy_model = map(model_anova, broom::tidy), 
    tidy_emmeans = map(emmeans, broom::tidy)) %>% #,
  # model_qual = map(model, MuMIn::r.squaredGLMM)) %>% 
  dplyr::select(stat, tidy_emmeans) %>%
  unnest(c(tidy_emmeans))
out_fits_new


