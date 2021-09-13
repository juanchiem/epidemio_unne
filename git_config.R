```{r}
# git config --global --unset user.name
# git config --global --unset user.email
```


```{r}
usethis::use_git_config(
  scope = "project",
  user.name = "juanchiem",
  user.email = "edwardsmolina@gmail.com"
)
credentials::set_github_pat("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")
```

```{r}
gitcreds::gitcreds_set()
```
