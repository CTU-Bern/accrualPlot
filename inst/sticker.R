



library(ggplot2)
library(emojifont)
library(scales)
library(hexSticker)

load.fontawesome()

# # btable(r)
# icon <- ggplot() +
#   geom_text(aes(x = c(3), y = c(1), label = fontawesome('fa-table')),
#             family = "fontawesome-webfont",
#             size = 70) +
#   geom_text(aes(x = c(0), y = 1, label = c("b")),
#             size = 70, family = "Aller_Rg") +
#   xlim(-1.5,5) +
#   theme_void() + theme_transparent()
# iconr <- ggplot() +
#   geom_text(aes(x = c(3.2), y = c(1), label = fontawesome('fa-table')),
#             family = "fontawesome-webfont",
#             size = 70) +
#   geom_text(aes(x = -.2, y = 1, label = c("b")),
#             size = 70, family = "Aller_Rg") +
#   geom_text(aes(x = 6.3, y = 1, label = c("r")),
#             size = 70, family = "Aller_Rg") +
#   xlim(-1.5,7) +
#   theme_void() + theme_transparent()
#
#
#
# # s <- sticker(last_plot(), package="CTUtemplate",
# #              p_size=18
# #              p_family = "Arial",
# #              s_x=1, s_y=1, s_width=1.3, s_height=1,
# #              filename="sticker.png",
# #              h_fill = CTUtemplate::unibeRed(.3),
# #              h_color = CTUtemplate::unibeRed()
# #              , spotlight = FALSE,
# #              l_x = 1,
# #              l_y = 1.7,
# #              l_width = 5,
# #              l_height = 5,
# #              l_alpha = .5,
# #              )
#
#
# s <- sticker(icon, package="",
#              s_x=1, s_y=1.15, s_width=1.8, s_height=2,
#              filename="man/figures/sticker.png",
#              h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
#              h_color = CTUtemplate::unibeRed(),
#              h_size = 2,
#              url = "btable",
#              u_size = 12,
#              u_x = 1,
#              u_y = 0.15
# )
# s <- sticker(iconr, package="",
#              s_x=1, s_y=1.15, s_width=1.8, s_height=2,
#              filename="man/figures/sticker2.png",
#              h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
#              h_color = CTUtemplate::unibeRed(),
#              h_size = 2,
#              url = "btabler",
#              u_size = 12,
#              u_x = 1,
#              u_y = 0.15
# )
# s







# accrualplot
p1 <- "fa-female" #sample(c("fa-female", "fa-male"), 1)
p2 <- "fa-male" #sample(c("fa-female", "fa-male"), 1)
p3 <- "fa-female" #sample(c("fa-female", "fa-male"), 1)
p4 <- "fa-female" #sample(c("fa-female", "fa-male"), 1)
p5 <- "fa-male" #sample(c("fa-female", "fa-male"), 1)
p6 <- "fa-male" #sample(c("fa-female", "fa-male"), 1)
p7 <- "fa-female" #sample(c("fa-female", "fa-male"), 1)
n <- 10
d <- data.frame(x = c(1:n, 3:n, 4:n, 6:n, 7:n, 8:n, 10:n),
                y = c(rep(1, n), rep(2,n-2), rep(3,n-3), rep(4,n-5), rep(5,n-6), rep(6, n-7), rep(7, n-9)),
                label = c(rep(fontawesome(p1), n),
                          rep(fontawesome(p2), n-2),
                          rep(fontawesome(p3), n-3),
                          rep(fontawesome(p4), n-5),
                          rep(fontawesome(p5), n-6),
                          rep(fontawesome(p6), n-7),
                          rep(fontawesome(p7), n-9)

                          )
                )
ap <- ggplot() +
  geom_text(data = d, aes(x = x, y = y, label = label),
            family = "fontawesome-webfont",
            size = 10) +
  ylim(0.5,7.5) +
  theme_void() + theme_transparent()
ap
s <- sticker(ap, package="",
             s_x=1, s_y=1.15, s_width=1.2, s_height=1,
             filename="man/figures/sticker.png",
             h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "accrualPlot",
             u_size = 12,
             u_x = 1,
             u_y = 0.15
)
s
