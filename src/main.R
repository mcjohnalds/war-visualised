suppressMessages(library(network))
suppressMessages(library(sna))
suppressMessages(library(ggplot2))
suppressMessages(library(GGally))
suppressMessages(library(Hmisc))
suppressMessages(library(scales))

# Returns a data frame of the ICB actor crises dataset
read.icb.actor.data <- function() {
    icb.actor <- read.csv("datasets/icb_actor_crises.csv")
    icb.actor$yrtrig <- as.numeric(as.character(icb.actor$yrtrig))
    icb.actor
}

# Returns a data frame of the ICB system crises dataset
read.icb.system.data <- function() {
    icb.system <- read.csv("datasets/icb_system_crises.csv")
    icb.system$yrtrig <- as.numeric(as.character(icb.system$yrtrig))
    icb.system
}

# Returns a data frame of the UCDP/PRIO conflicts dataset
read.ucdp.conflicts.data <- function() {
    ucdp.conflicts <- read.csv("datasets/ucdp_armed_conflicts.csv", as.is=TRUE)
    # Factor to string conversion
    ucdp.conflicts$GWNoA <- lapply(ucdp.conflicts$GWNoA, as.character)
    ucdp.conflicts$GWNoA2nd <- lapply(ucdp.conflicts$GWNoA2nd, as.character)
    ucdp.conflicts$GWNoB2nd <- lapply(ucdp.conflicts$GWNoB2nd, as.character)
    # "a, b, c" -> c("a", "b", "c")
    comma.separated.string.to.vector <- function(x) {
        strs <- strsplit(x, ", ")[[1]]
    }
    # Comma separated string to list conversion
    ucdp.conflicts$GWNoA <- lapply(ucdp.conflicts$GWNoA,
                                      comma.separated.string.to.vector)
    ucdp.conflicts$GWNoA2nd <- lapply(ucdp.conflicts$GWNoA2nd,
                                      comma.separated.string.to.vector)
    ucdp.conflicts$GWNoB2nd <- lapply(ucdp.conflicts$GWNoB2nd,
                                      comma.separated.string.to.vector)
    ucdp.conflicts
}

# Returns a data frame of the Gleditch and Ward state list dataset
read.gw.states.data <- function() {
    gw.independent.states <- read.csv("datasets/gw_independent_states.csv",
                                      as.is=TRUE, stringsAsFactors=FALSE)
    gw.microstates <- read.csv("datasets/gw_microstates.csv", as.is=TRUE,
                               stringsAsFactors=FALSE)
    gw.states <- rbind(gw.independent.states, gw.microstates)
    gw.states$id <- lapply(gw.states$id, as.character)
    # Some states disappear then appear again. So you get two rows with the same
    # id, code, and name, only differing by start and end dates. Since we don't
    # care about start and end dates of states, we remove rows which share their
    # id with another row.
    gw.states <- subset(gw.states, !duplicated(gw.states$id))
    gw.states
}

# Saves a plot comparing state nuclear weapon capability with crisis violence
nuclear.compared.with.violence.plot <- function() {
    palette <- c("#F8B2AE", "#F89B95", "#F8847C", "#F86D63", "#F8564A")
    df <- as.data.frame(table(nuclear=icb.actor$nuclear, viol=icb.actor$viol))
    ggplot(data=df, aes(x=nuclear, y=Freq, fill=viol)) +
        geom_bar(stat="identity", position="fill") +
        # ggtitle("Country nuclear capability compared with crisis violence") +
        xlab("\n\nNuclear capability of actors in crises") +
        ylab("Frequency of level of violence in crises") +
        scale_x_discrete(labels=nuclear.names) +
        scale_y_continuous(labels=percent) +
        scale_fill_manual(values=palette,
                          name="",
                          labels=viol.names,
                          guide=guide_legend(reverse=T)) +
        default.theme + transparent.bg.theme
    suppressMessages(ggsave("tmp/nuclear_compared_with_violence.pdf"))
    suppressMessages(ggsave("out/nuclear_compared_with_violence.png"))
}

# Saves a graphic containing 4 plots of war over time
war.over.time.plot <- function() {
    df <- as.data.frame(table(yrtrig=icb.system$yrtrig, viol=icb.system$viol))
    df$yrtrig <- as.numeric(as.character(df$yrtrig))
    levels(df$viol) <- viol.names
    ggplot(data=df, aes(x=yrtrig, y=Freq, color=viol)) +
        geom_point(show.legend=FALSE) +
        geom_smooth(method="loess", se=FALSE, show.legend=FALSE) +
        facet_wrap(~viol) +
        guides(fill=FALSE) +
        # ggtitle("Violence in crises over time") +
        xlab("Year") +
        ylab("Number of conflicts for given year") +
        scale_x_continuous() +
        default.theme +
        theme(plot.background = element_rect(fill = "transparent",colour = NA))
    suppressMessages(ggsave("tmp/war_over_time.pdf"))
    suppressMessages(ggsave("out/war_over_time.png"))
}

# Creates a network where nodes are states and edges are instances where one
# state has assisted a primary party in a conflict. Returns a list lst
# containing 3 items:
#
# lst$edges: An adjacency matrix in the format
#     1 2 3 4
#   1 0 1 0 0
#   2 1 0 0 1
#   3 0 0 0 0
#   4 0 1 0 0
# Here, states with ids 1 and 2 have worked together and so have states with ids
# 4 and 1.
#
# lst$nodes: A subset of gw.states containing only the states that have assisted
# or been assisted by another state in a conflict.
#
# lst$network: A network object created from the above information.
ally.network <- function() {
    edges <- matrix(0, nrow=nrow(gw.states), ncol=nrow(gw.states))
    dimnames(edges) <- list(gw.states$code, gw.states$code)

    # Create a edge matrix where an edge between two states A and B represents
    # an instance where one assisted the other in a conflict. Edges are
    # weighted by total number of conflicts and intensities per conflict.
    by(ucdp.conflicts, 1:nrow(ucdp.conflicts), function(row) {
        edges <<- add.edges.between.states(edges, row$GWNoA[[1]], row$GWNoA[[1]])
        edges <<- add.edges.between.states(edges, row$GWNoA[[1]], row$GWNoA2nd[[1]])
        edges <<- add.edges.between.states(edges, row$GWNoA2nd[[1]], row$GwNoA2nd[[1]])
    })

    important <- gw.states # The list of important states (see below)
    # Remove states which have no edges
    lapply(gw.states$code, function(state) {
        if (sum(edges[state, ]) == 0 && sum(edges[, state]) == 0) {
            edges <<- edges[rownames(edges) != state, ]
            edges <<- edges[, colnames(edges) != state]
            important <<- important[important$id != state, ]
        }
    })

    net <- network(edges, vertex.attr=important)
    list(network=net, edges=edges, nodes=important)
}

# Saves a network plot of states that are allies
ally.network.plot <- function() {
    label <- c("AFG", "IRQ", "MLI", "USA")
    ggnet2(ally.network()$network, label=label, size="degree", label.size=2,
           mode="kamadakawai",
           edge.color="#A9F86D", node.color="#6DEFF8") +
        # ggtitle("Countries fight on the same side") +
        guides(color=FALSE, size=FALSE) +
        default.theme + transparent.bg.theme
    suppressMessages(ggsave("tmp/ally_network.pdf"))
    suppressMessages(ggsave("out/ally_network.png"))
}

# Convert a G&W state ID to a G&W state code
state.id.to.code <- function(id) {
    gw.states[which(gw.states$id == id)[1], ]$code
}

# Modify edges to include links between all states in a and all states in b.
# edges is an adjacency matrix. a and b are lists of numbers which are state
# ids. If any elements of a or b aren't inside gw.states$id then they are
# ignored.
#
# Example:
#
# edges <- matrix(0, nrow=3, ncol=3)
# dimnames(edges) <- list(1:3, 1:3)
# edges
#   1 2 3
# 1 0 0 0
# 2 0 0 0
# 3 0 0 0
# a <- list(1)
# b <- list(2, 3)
# add.edges.between.states(edges, a, b)
#   1 2 3
# 1 0 1 1
# 2 1 0 0
# 3 1 0 0
add.edges.between.states <- function(edges, a, b) {
    lapply(a, function(sideA) {
        lapply(b, function(sideB) {
            sideA <- state.id.to.code(sideA)
            sideB <- state.id.to.code(sideB)
            if (!is.na(sideA) && !is.na(sideB) && sideA != sideB) {
                inc(edges[sideA, sideB]) <<- 1
                inc(edges[sideB, sideA]) <<- 1
            }
        })
    })
    edges
}

# Creates a network where nodes are states and edges are instances where one
# state has fought another in a conflict. Returns a list lst containing 3
# items:
#
# lst$edges: An adjacency matrix in the format
#     1 2 3 4
#   1 0 1 0 0
#   2 1 0 0 1
#   3 0 0 0 0
#   4 0 1 0 0
# Here, states with ids 1 and 2 have fought each-other and so have states with
# ids 4 and 1.
#
# lst$nodes: A subset of gw.states containing only the states that have fought
# with another state in a conflict.
#
# lst$network: A network object created from the above information.
enemy.network <- function() {
    # TODO: Check all edges are there
    # We are only concerned with interstate conflicts
    interstate <- ucdp.conflicts[ucdp.conflicts$GWNoB != "", ]

    edges <- matrix(0, nrow=nrow(gw.states), ncol=nrow(gw.states))
    dimnames(edges) <- list(gw.states$code, gw.states$code)

    # Create a edge matrix where an edge between two states A and B represents
    # an instance where one fought the other in a conflict. Edges are
    # weighted by total number of conflicts and intensities per conflict.
    by(interstate, 1:nrow(interstate), function(row) {
        edges <<- add.edges.between.states(edges, row$GWNoA[[1]], row$GWNoB[[1]])
        edges <<- add.edges.between.states(edges, row$GWNoA[[1]], row$GWNoB2nd[[1]])
        edges <<- add.edges.between.states(edges, row$GWNoA2nd[[1]], row$GWNoB[[1]])
        edges <<- add.edges.between.states(edges, row$GWNoA2nd[[1]], row$GWNoB2nd[[1]])
    })

    important <- gw.states # The list of important states (see below)
    lapply(gw.states$code, function(state) {
        if (sum(edges[state, ]) == 0 && sum(edges[, state]) == 0) {
            edges <<- edges[rownames(edges) != state, ]
            edges <<- edges[, colnames(edges) != state]
            important <<- important[important$id != state, ]
        }
    })

    net <- network(edges, vertex.attr=important)
    list(network=net, edges=edges, nodes=important)
}

# Saves a network plot of states that are allies
enemy.network.plot <- function() {
    label <- c("PRK", "RUS", "CHN", "IRQ")
    ggnet2(enemy.network()$network, label=label, size="degree", label.size=2,
           mode="mds",
           edge.color="#F8766D", node.color="#6DEFF8") +
        # ggtitle("Countries fight on opposite sides") +
        guides(color=FALSE, size=FALSE) +
        default.theme + transparent.bg.theme
    suppressMessages(ggsave("tmp/enemy_network.pdf"))
    suppressMessages(ggsave("out/enemy_network.png"))
}

# Convert a UCDP/PRIO region ID to the region's name
state.id.to.region <- function(id) {
    id <- as.numeric(id)
    if (id >= 200 && id <= 395) {
        "Europe"
    } else if (id >= 630 && id <= 698) {
        "Middle East"
    } else if (id >= 700 && id <= 990) {
        "Asia"
    } else if (id >= 400 && id <= 625) {
        "Africa"
    } else if (id >= 2 && id <= 165) {
        "Americas"
    } else {
        "Invalid region"
    }
}

# Like enemy.network.plot but shows country region. Not functional.
conflict.region.plot <- function() {
    enemy <- enemy.network()
    enemy$network %v% "Region" <- lapply(enemy$nodes$id, state.id.to.region)
    # print(lapply(enemy$nodes$id, state.id.to.region))
    print(summary(enemy$network))
    print(length(enemy$nodes$id))
    label <- c("PRK", "RUS", "CHN", "IRQ")
    ggnet2(enemy$network, label=label, size="degree", label.size=2,
           mode="mds", color="Region",
           edge.color="#F8766D") +
        # ggtitle("Countries fight on opposite sides") +
        guides(size=FALSE) +
        default.theme
}

# Shows the involvement of Russia in crises over time. Not functional.
suinv.over.time.plot <- function() {
    df <- icb.actor[icb.actor$actor == "RUS", ]
    df$yrtrig <- as.numeric(as.character(df$yrtrig))
    df$majres <- factor(as.numeric(as.character(df$majres)), levels=1:9)
    df <- as.data.frame(table(yrtrig=df$yrtrig, majres=df$majres))
    df$yrtrig <- as.numeric(as.character(df$yrtrig))
    df$majres <- as.numeric(as.character(df$majres))
    ggplot(df, aes(x=yrtrig, y=Freq)) +
        geom_point() +
        geom_smooth(se=FALSE) +
        xlab("year") +
        ylab("frequency") +
        scale_x_continuous()
        scale_y_continuous(breaks=NULL) +
        # scale_y_continuous(breaks=1:9, labels=majres.names)
        default.theme
    suppressMessages(ggsave("tmp/suinv_over_time.pdf"))
    suppressMessages(ggsave("out/suinv_over_time.png"))
}

viol.names <- c("No violence",
                "Minor clashes",
                "Serious clashes",
                "Full-scale war")

nuclear.names <- c("No foreseeable\nnuclear\ncapability",
                   "Could build\nnuclear\nweapons",
                   "Has nuclear\nweapons",
                   "Second strike\ncapability")

majres.names <- c("No response",
                  "Verbal act",
                  "Political act",
                  "Economic act",
                  "Other non-violent act",
                  "Non-violent military act",
                  "Multiple including\nnon-violent military act",
                  "Violent military act",
                  "Multiple including\nviolent military act")

icb.actor <- read.icb.actor.data()
icb.system <- read.icb.system.data()
ucdp.conflicts <- read.ucdp.conflicts.data()
gw.states <- read.gw.states.data()
default.theme <- theme(text=element_text(family="serif", size=16))
transparent.bg.theme <- theme(plot.background = element_rect(fill = "transparent",colour = NA),
                              legend.background = element_rect(fill="transparent"))

set.seed(1234) # Make sure network plots are the same each time

nuclear.compared.with.violence.plot()
war.over.time.plot()
ally.network.plot()
enemy.network.plot()

# conflict.region.plot()
# suinv.over.time.plot()
