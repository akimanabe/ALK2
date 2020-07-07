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
  create_forward_alk() %>%
  tidyr::pivot_longer(cols = c(-Age), names_to = "Length", values_to="prop") %>%
  dplyr::mutate(Length = as.double(Length)) %>%
  dplyr::inner_join(., lcount, by = "Length") %>%
  dplyr::mutate(fnumber = prop * Frequency) %>%
  dplyr::select(Age, Length, fnumber) %>%
  tidyr::pivot_wider(names_from = "Length", values_from = "fnumber")
