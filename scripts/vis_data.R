e <- read_csv("dataset/editions.csv")

p <- e %>% filter(!is.na(year)) %>% filter(author != "NOAUTHOR") %>%
    group_by(year) %>% summarise(n = n_distinct(author_std)) %>%
    filter(year>1917 & year < 1985) %>%
    ggplot(aes(x = year, y = n, group = 1)) + geom_line(size=1, color="navyblue") +
    geom_vline(xintercept = 1932, lty = 2, color = "darkred") +
    annotate("text", x = 1934, y = 700, label = "1932", color="darkred", size=9, angle = 90) +
    annotate("rect", xmin = 1939, xmax = 1945, ymin = -Inf, ymax = Inf, alpha=0.3) +
    annotate("text", x = 1942, y = 530, label = "1939-1945", color="gray20", size=9, angle = 90) +
    labs(title = "«Демография» детской литературы", subtitle = "ежегодно издаваемые писатели (1918—1984)") +
    xlab("") + ylab("количество авторов, чьи книги издавались в этом году") +
    +theme(axis.text.x = element_text(size=22))
p

ggsave(filename="results/starcev-viz.png", p, width = 8, height = 6)
