# function to clean and explain the diagnostic framework variables
name_the_tiers <- function(data){

  # create tier 1 names
  data <- data %>% 
    mutate(tier1 = sub("\\d.*", "", dimension),
           tier1 = case_when(
             tier1 == "rs"  ~ "Resource Systems (RS)",
             tier1 == "gs"  ~ "Governance Systems (GS)",
             tier1 == "a"   ~ "Actors (A)",
             tier1 == "o"   ~ "Outcomes (O)",
             tier1 == "i"   ~ "Interactions (I)"
           )) %>% 
    mutate(tier1 = factor(tier1, ordered = TRUE, levels = c(
      "Resource Systems (RS)",
      "Governance Systems (GS)",
      "Actors (A)",
      "Interactions (I)",
      "Outcomes (O)"
    )))
  
  # create tier 3 names
  data <- data %>% 
    mutate(tier3 = case_when(
      dimension == "a4.1.leadership.accountability"                           ~ "A4.1: Leadership accountability",
      dimension == "a5.1.actor.group.trust"                                   ~ "A5.1: Actor group trust",
      dimension == "a5.2.inter-group.trust"                                   ~ "A5.2: Inter-group trust",
      dimension == "gs2.1.external.support"                                   ~ "GS2.1: External support",
      dimension == "gs3.2.property.security"                                  ~ "GS3.2: Property security",
      dimension == "gs4.1.rules-in-use"                                       ~ "GS4.1: Rules-in-use",
      dimension == "gs4.2.governance.strictness.trend"                        ~ "GS4.2: Governance strictness trend",
      dimension == "gs5.1.external.recognition"                               ~ "GS5.1: External recognition",
      dimension == "gs5.4.participation.in.zoning"                            ~ "GS5.4: Participation in zoning",
      dimension == "gs5.3.participation.in.rule.making"                       ~ "GS5.3: Participation in rule making",
      dimension == "gs5.5.commons.political.power"                            ~ "GS5.5: Commons political power",
      dimension == "gs6.2.outsider.exclusion"                                 ~ "GS6.2: Outsider exclusion",
      dimension == "gs7.1.environmental.monitoring"                           ~ "GS7.1: Environmental monitoring",
      dimension == "gs7.2.self.sanctions"                                     ~ "GS7.2: Self sanctions",
      dimension == "gs7.3.external.sanctions"                                 ~ "GS7.3: External sanctions",
      dimension == "i1.1.conflict.resolution"                                 ~ "I1.1: Conflict resolution",
      dimension == "i2.1.participation.in.social.monitoring.(enforcement)"    ~ "I2.1: Participation in social monitoring",
      dimension == "o1.1.compliance"                                          ~ "O1.1: Compliance",
      dimension == "o2.1.commons.condition.trend"                             ~ "O2.1: Commons condition trend",
      dimension == "o2.3.invasives"                                           ~ "O2.3: Invasives",
      dimension == "rs2.1.commons.boundaries"                                 ~ "RS2.1: Commons boundaries",
      dimension == "rs2.2.commons.boundary.negotiability"                     ~ "RS2.2: Commons boundaries negotiability"
    )) %>% 
    # create descriptions of items
    mutate(descr = case_when(
      dimension == "a4.1.leadership.accountability"                         ~ "Mechanisms of accountability are very effective and the group members represented by a leader are able to invoke/use them",
      dimension == "a5.1.actor.group.trust"                                 ~ "Members have full faith and confidence in one another to fullfill promises",
      dimension == "a5.2.inter-group.trust"                                 ~ "Groups have the full faith and confidence that the other groups will fulfill their promises",
      dimension == "gs2.1.external.support"                                 ~ "Higher level organizations involved provide extensive, ongoing support to lower level jurisdictions",
      dimension == "gs3.2.property.security"                                ~ "There is a strong common understanding of what aspects of a commons are owned and these rights are complied with",
      dimension == "gs4.1.rules-in-use"                                     ~ "Rules are in place that govern when and where to graze livestock",
      dimension == "gs4.2.governance.strictness.trend"                      ~ "There has been change towards enhanced enforcement and/or increased application of sanctions",
      dimension == "gs5.1.external.recognition"                             ~ "Complete recognition of communities' autonomy in decision-making regarding the rangeland by larger governmental jurisdictions",
      dimension == "gs5.4.participation.in.zoning"                          ~ "The actor group is in charge of the zoning of the rangeland",
      dimension == "gs5.3.participation.in.rule.making"                     ~ "Users have active engagement in decision-making processes",
      dimension == "gs5.5.commons.political.power"                          ~ "User or leadership group with high levels of power have the ability to change rules on their own",
      dimension == "gs6.2.outsider.exclusion"                               ~ "Users are able to prevent the great majority to all incursion by outsiders",
      dimension == "gs7.1.environmental.monitoring"                         ~ "The group engages in frequent and systematic monitoring efforts to observe changes in rangeland's conditions.",
      dimension == "gs7.2.self.sanctions"                                   ~ "Sanctions are applied by and to the members of the group for violations of extraction rules",
      dimension == "gs7.3.external.sanctions"                               ~ "Sanctions are applied by other actor groups (i.e., external authority) to the members of the group for violations of extraction rules",
      dimension == "i1.1.conflict.resolution"                               ~ "Mechanisms are in place to address conflicts that arise over the use of the rangeland by the user group",
      dimension == "i2.1.participation.in.social.monitoring.(enforcement)"  ~ "The users always participate in monitoring other people's grazing behaviors",
      dimension == "o1.1.compliance"                                        ~ "This user group almost always or always complies with formal rules",
      dimension == "o2.1.commons.condition.trend"                           ~ "Condition of the rangeland has improved",
      dimension == "o2.3.invasives"                                         ~ "Invasive species do not pose a threat to this resource",
      dimension == "rs2.1.commons.boundaries"                               ~ "The limits of the rangeland are clearly defined and highly visible",
      dimension == "rs2.2.commons.boundary.negotiability"                   ~ "Negotiations to access this village rangeland by non-members are not possible or not fruitful",
    ))
  
  # check data
  stopifnot(sum(is.na(data$tier1))==0)
  stopifnot(sum(is.na(data$tier3))==0)
  
  return(data)
  
}

# helper function for radar plots, adapted from:
# https://stackoverflow.com/questions/67334137/increase-space-for-long-axis-labels-in-radar-chart
coord_radar2 <- function(theta = "x", start = 0, direction = 1, clip = "off") {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") {
    "y"
  } else {
    "x"
  }
  ggproto("CoordRadar", ggplot2::CoordPolar,
          theta = theta,
          r = r, start = start, clip = clip,
          direction = sign(direction), is_linear = function(coord) TRUE
  )
}

ggRadar2 <- function(data, mapping = NULL, rescale = TRUE, legend.position = "top",
                     colour = "red", alpha = 0.3, size = 3, ylim = NULL, scales = "fixed",
                     use.label = FALSE, interactive = FALSE, clip = "off", ...) {
  data <- as.data.frame(data)
  (groupname <- setdiff(names(mapping), c("x", "y")))
  groupname
  mapping
  length(groupname)
  if (length(groupname) == 0) {
    groupvar <- NULL
  }
  else {
    groupvar <- ggiraphExtra:::getMapping(mapping, groupname)
  }
  groupvar
  facetname <- colorname <- NULL
  if ("facet" %in% names(mapping)) {
    facetname <- ggiraphExtra:::getMapping(mapping, "facet")
  }
  (colorname <- setdiff(groupvar, facetname))
  if ((length(colorname) == 0) & !is.null(facetname)) {
    colorname <- facetname
  }
  data <- ggiraphExtra:::num2factorDf(data, groupvar)
  (select <- sapply(data, is.numeric))
  if ("x" %in% names(mapping)) {
    xvars <- ggiraphExtra:::getMapping(mapping, "x")
    xvars
    if (length(xvars) < 3) {
      warning("At least three variables are required")
    }
  }
  else {
    xvars <- colnames(data)[select]
  }
  (xvars <- setdiff(xvars, groupvar))
  if (rescale) {
    data <- ggiraphExtra:::rescale_df(data, groupvar)
  }
  temp <- sjlabelled::get_label(data)
  cols <- ifelse(temp == "", colnames(data), temp)
  if (is.null(groupvar)) {
    id <- ggiraphExtra:::newColName(data)
    data[[id]] <- 1
    longdf <- reshape2::melt(data, id.vars = id, measure.vars = xvars)
  }
  else {
    cols <- setdiff(cols, groupvar)
    longdf <- reshape2::melt(data, id.vars = groupvar, measure.vars = xvars)
  }
  temp <- paste0("plyr::ddply(longdf,c(groupvar,'variable'), dplyr::summarize,mean=mean(value,na.rm=TRUE))")
  df <- eval(parse(text = temp))
  colnames(df)[length(df)] <- "value"

  if (is.null(groupvar)) {
    id2 <- ggiraphExtra:::newColName(df)
    df[[id2]] <- "all"
    id3 <- ggiraphExtra:::newColName(df)
    df[[id3]] <- 1:nrow(df)
    df$tooltip <- paste0(df$variable, "=", round(
      df$value,
      1
    ))
    df$tooltip2 <- paste0("all")
    p <- ggplot(data = df, aes_string(
      x = "variable", y = "value",
      group = 1
    )) +
      ggiraph::geom_polygon_interactive(aes_string(tooltip = "tooltip2"),
                                        colour = colour, fill = colour, alpha = alpha
      ) +
      ggiraph::geom_point_interactive(aes_string(
        data_id = id3,
        tooltip = "tooltip"
      ), colour = colour, size = size)
  }
  else {
    if (!is.null(colorname)) {
      id2 <- ggiraphExtra:::newColName(df)
      df[[id2]] <- df[[colorname]]
    }
    id3 <- ggiraphExtra:::newColName(df)
    df[[id3]] <- 1:nrow(df)
    df$tooltip <- paste0(
      groupvar, "=", df[[colorname]], "<br>",
      df$variable, "=", round(df$value, 1)
    )
    df$tooltip2 <- paste0(groupvar, "=", df[[colorname]])
    p <- ggplot(data = df, aes_string(
      x = "variable", y = "value",
      colour = colorname, fill = colorname, group = colorname
    )) +
      ggiraph::geom_polygon_interactive(aes_string(tooltip = "tooltip2"),
                                        alpha = alpha
      ) +
      ggiraph::geom_point_interactive(aes_string(
        data_id = id3,
        tooltip = "tooltip"
      ), size = size)
  }

  if (!is.null(facetname)) {
    formula1 <- as.formula(paste0("~", facetname))
    p <- p + facet_wrap(formula1, scales = scales)
  }
  p <- p + xlab("") + ylab("") + theme(legend.position = legend.position)
  
  p <- p + coord_radar2(clip = clip)
  if (!is.null(ylim)) {
    p <- p + expand_limits(y = ylim)
  }
  p
}

# function to format numeric outputs
style_number <- function(x, dig = 3){
  format(janitor::round_half_up(x, dig), nsmall = dig)
}