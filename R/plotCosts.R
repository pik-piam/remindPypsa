#' Plot annualised capital costs and marginal costs
#' This function uses the remind2pypsa data file.
#'
#' @param remind2pypsa rds data file
#' @param colorsTech Colour mapping
#' @param namesTech Pretty names mapping
#'
#' @return List of ggplot2 objects
#' @export
#'
plotCosts <- function(remind2pypsa, colorsTech, namesTech) {

  capCost <- remind2pypsa %>%
    filter(.data$variable == "capCost") %>%
    quitte::order.levels(tech = names(colorsTech)) %>%
    mutate(iter = as.integer(.data$iter),
           value = .data$value / 1E3)  # $/MW to $/kW

  # Anunalised capital costs over years
  p1 <- ggplot() +
    geom_line(data = capCost,
              mapping = aes(x = .data$year, y = .data$value, color = .data$tech)) +
    scale_color_manual(name = "Technology",
                       values = colorsTech,
                       labels = namesTech) +
    facet_wrap(~ iter) +
    xlab("Year") +
    ylab("$/kW") +
    ggtitle("Annualised capital costs over years") +
    theme_bw()

  # Annualised capital costs over iterations
  p2 <- ggplot() +
    geom_line(data = capCost,
              mapping = aes(x = .data$iter, y = .data$value, color = .data$tech)) +
    scale_color_manual(name = "Technology",
                       values = colorsTech,
                       labels = namesTech) +
    facet_wrap(~ year) +
    xlab("Iteration") +
    ylab("$/kW") +
    ggtitle("Annualised capital costs over iterations") +
    theme_bw()

  margCost <- remind2pypsa %>%
    filter(.data$variable == "margCost") %>%
    quitte::order.levels(tech = names(colorsTech)) %>%
    mutate(iter = as.integer(.data$iter))

  # Marginal costs over years
  p3 <- ggplot() +
    geom_line(data = margCost,
              mapping = aes(x = .data$year, y = .data$value, color = .data$tech)) +
    scale_color_manual(name = "Technology",
                       values = colorsTech,
                       labels = namesTech) +
    facet_wrap(~ iter) +
    xlab("Year") +
    ylab("$/MWh") +
    ggtitle("Marginal costs over years") +
    theme_bw()

  # Marginal costs over iterations
  p4 <- ggplot() +
    geom_line(data = margCost,
              mapping = aes(x = .data$iter, y = .data$value, color = .data$tech)) +
    scale_color_manual(name = "Technology",
                       values = colorsTech,
                       labels = namesTech) +
    facet_wrap(~ year) +
    xlab("Iteration") +
    ylab("$/MWh") +
    ggtitle("Marginal costs over iterations") +
    theme_bw()

  return(list(p1, p2, p3, p4))
}
