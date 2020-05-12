#' Emission factors from EMEP EEA Tier 2
#'
#' @description \code{\link{ef}} Returns emission factors from EMEP EEA Tier2.
#'
#' @param nfr Character; NFR.
#' @param sector Character; sector.
#' @param table Character; table.
#' @param type Character; type.
#' @param tech Character; technology.
#' @param fuel Character; fuel.
#' @param aba Character; abatement.
#' @param region Character; region.
#' @param pol Character; pollutant.
#' @param df Logical; To return as data.frame or not. When T it hows also
#' the units, confidence interval lower, upper and the reference.
#' @return Emission factors or data.frame.
#' @export
#' @examples \dontrun{
#' # do not run
#' ef
#' }
ef <- function(nfr,
               sector,
               table,
               type,
               tech,
               fuel,
               aba,
               region,
               pol,
               df) {
  ef <- sysdata
  ef[is.na(ef)] <- "NONE"
  if(missing(nfr)) {
    choice <- utils::menu(unique(ef$NFR), title="Choose NFR")
    nfr <- unique(ef$NFR)[choice]
    ef <- ef[ef$NFR == nfr, ]
  }

  if(missing(sector)) {
    choice <- utils::menu(unique(ef$Sector), title="Choose Sector")
    sector <- unique(ef$Sector)[choice]
    ef <- ef[ef$Sector == sector, ]
  }

  if(missing(table)) {
    choice <- utils::menu(unique(ef$Table), title="Choose Table")
    table <- unique(ef$Table)[choice]
    ef <- ef[ef$Table == table, ]
  }

  if(missing(type)) {
    choice <- utils::menu(unique(ef$Type), title="Choose Type")
    type <- unique(ef$Type)[choice]
    ef <- ef[ef$Type == type, ]
  }

  if(missing(tech)) {
    choice <- utils::menu(unique(ef$Technology), title="Choose Technology")
    type <- unique(ef$Technology)[choice]
    ef <- ef[ef$Technology == tech, ]
  }

  if(missing(fuel)) {
    choice <- utils::menu(unique(ef$Fuel), title="Choose Technology")
    fuel <- unique(ef$Fuel)[choice]
    ef <- ef[ef$Fuel == fuel, ]
  }

  if(missing(aba)) {
    choice <- utils::menu(unique(ef$Abatement), title="Choose Abatement")
    aba <- unique(ef$Abatement)[choice]
    ef <- ef[ef$Abatement == aba, ]
  }

  if(missing(region)) {
    choice <- utils::menu(unique(ef$Region), title="Choose Region")
    region <- unique(ef$Region)[choice]
    ef <- ef[ef$Region == region, ]
  }

  if(missing(pol)) {
    choice <- utils::menu(unique(ef$Pollutant), title="Choose Pollutant")
    region <- unique(ef$Pollutant)[choice]
    ef <- ef[ef$Pollutant == pol, ]
  }

  value <- as.numeric(as.character(ef$Value))
  units <- ef$Unit

  if (df) {
    return(df)
  } else {
    cat("units: ", units, "\n")
    return(value)
  }
}
