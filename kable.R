
options(kableExtra.latex.load_packages = FALSE)
library(kableExtra)

options(knitr.table.format = "latex")

kbl(dt, booktabs = T)

kbl(dt, booktabs = T) %>%
  kable_styling(latex_options = "striped")
kable_styling(latex_options = "hold_position")
kable_styling(latex_options = "scale_down")
(latex_options = c("repeat_header")
  kable_styling(position = "center")
  kable_styling(position = "float_right")
  
  #tamaños
  kbl(text_tbl, booktabs = T) %>%
    kable_styling(full_width = F) %>%
    column_spec(1, bold = T, color = "red") %>%
    column_spec(2, width = "30em")
  
  #colores
  that_cell <- c(rep(F, 7), T)
  mtcars[1:8, 1:8] %>%
    kbl(booktabs = T, linesep = "") %>%
    kable_paper(full_width = F) %>%
    column_spec(2, color = spec_color(mtcars$mpg[1:8]),
                link = "https://haozhu233.github.io/kableExtra") %>%
    column_spec(6, color = "white",
                background = spec_color(mtcars$drat[1:8], end = 0.7),
                popover = paste("am:", mtcars$am[1:8])) %>%
    column_spec(9, strikeout = that_cell, bold = that_cell,
                color = c(rep("black", 7), "red"))
  
  #insert images
  tbl_img <- data.frame(
    name = c("kableExtra 1", "kableExtra 2"),
    logo = ""
  )
  tbl_img %>%
    kbl(booktabs = T) %>%
    kable_paper(full_width = F) %>%
    column_spec(2, image = "kableExtra_sm.png")

  #tamaño fotos
  tbl_img %>%
    kbl(booktabs = T) %>%
    kable_paper(full_width = F) %>%
    column_spec(2, image = spec_image(
      c("kableExtra_sm.png", "kableExtra_sm.png"), 50, 50))
  
  
  #grouped columns/rows
  #Add header rows to group columns
  kbl(dt, booktabs = T) %>%
    kable_styling() %>%
    add_header_above(c(" " = 1, "Group 1" = 2, "Group 2" = 2, "Group 3" = 2))
  
  
  
  #group rows
  #foot notes
  
  
  
  
  kbl(dt, align = "c") %>%
    kable_styling(full_width = F) %>%
    footnote(general = "Here is a general comments of the table. ",
             number = c("Footnote 1; ", "Footnote 2; "),
             alphabet = c("Footnote A; ", "Footnote B; "),
             symbol = c("Footnote Symbol 1; ", "Footnote Symbol 2")
    )
  
  
  #https://haozhu233.github.io/kableExtra/awesome_table_in_pdf.pdf
  
  
  