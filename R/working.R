# Working file

sdata <- sampledata %>%
  dplyr::mutate(num = seq(1, nrow(.)))

subdata <-
  subsample_data(sdata) %>%
  dplyr::mutate(Length = categorize_num(Length, 1),
                Age = categorize_num(Age, 1))


aged <- subdata %>%
  tidyr::drop_na(Age)

lengthed <- subdata %>%
  dplyr::filter(is.na(Age))


lcount <-
  count.ldata(lengthed) %>%
  dplyr::rename(Frequency = n)
#  tidyr::pivot_wider(names_from = Length, values_from = n)%>%
#  tidyr::pivot_longer(cols = everything(), names_to = "Length", values_to = "Frequency")


alkey <-
aged %>%
  dplyr::select(Length, Age) %>%
  create_reverse_alk() %>%
  tidyr::pivot_longer(cols = c(-Age), names_to = "Length", values_to="prop") %>%
  tidyr::pivot_wider(names_from = "Age", values_from = "prop") %>%
  tidyr::nest(data = c(`0`:`7`)) %>%
  dplyr::mutate(Length = as.double(Length)) %>%
    dplyr::left_join(.,lcount) %>%
  dplyr::mutate(dat2 = purrr::map2(data, Frequency, ~ data * Frequency))
  purrr::map2(.x = data, .y = Frequency, ~.x * .y)
