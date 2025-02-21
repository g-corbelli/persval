#' Computing and visualizing Schwartz's Personal Values
#'
#' The persval package provides functions for computing personal values scores
#' from various questionnaires based on the theoretical constructs proposed by
#' Schwartz (Schwartz, 1992, 1996, 2015; Schwartz et al., 2017).
#' Designed to assist researchers and practitioners in psychology,
#' sociology, and related fields, the package facilitates the quantification
#' of different dimensions related to personal values from survey data and the
#' graphical representation of their relative importance.
#' It incorporates the recommended statistical adjustment (ipsatization) to
#' enhance the accuracy and interpretation of the results.
#'
#' Supported questionnaires within the package include:
#' \itemize{
#'   \item \code{\link{svs}}: SVS-57, the Schwartz Value Survey with 57 items (Schwartz, 1992).
#'   \item \code{\link{pvq40}}: PVQ-40, the Portrait Values Questionnaire with 40 items (Schwartz, 2006; Schwartz et al., 2001).
#'   \item \code{\link{pvq21}}: PVQ-21/ESS-21, the shortened version of PVQ used in the European Social Survey (Schwartz, 2003).
#'   \item \code{\link{twivi}}: TwIVI, the Twenty Item Values Inventory (Sandy et al., 2017).
#'   \item \code{\link{tivi}}: TIVI, the Ten Item Values Inventory (Sandy et al., 2017).
#' }
#'
#' The package includes functions to generate spider charts that graphically
#' depict the relative importance of each of the ten basic personal values and
#' the four higher order values, making it easier to present, interpret, and
#' confront visually an individual's or group's value orientation:
#' \itemize{
#'   \item \code{\link{tenvalplot}}: Function to plot spider charts for ten basic personal values.
#'   \item \code{\link{fourvalplot}}: Function to plot spider charts for four higher order personal values.
#' }
#'
#' @section Note:
#' This package is independently developed based on Schwartz’s theoretical framework and is not
#' directly endorsed by Professor Schwartz. Researchers are advised to consult the original
#' studies and validation articles for comprehensive insights into the theoretical and empirical basis
#' of the instruments used (e.g.: Closs, 1996; Cornwell & Dunlop, 1994; Rudnev, 2021;
#' Saris, 1988; Schwartz, 1992, 1996, 2015; Schwartz et al., 2017; Smith, 2004;
#' Van Rosmalen et al., 2010).
#'
#' @references
#' Closs, S. J. (1996). On the factoring and interpretation of ipsative data. \emph{Journal of Occupational and Organizational Psychology}, 69(1), 41-47. doi:10.1111/j.2044-8325.1996.tb00598.x
#'
#' Cornwell, J. M., & Dunlap, W. P. (1994). On the questionable soundness of factoring ipsative data: A response to Saville and Willson (1991). \emph{Journal of Occupational and Organizational Psychology}, 67, 89-100. doi:10.1111/j.2044-8325.1994.tb00553.x
#'
#' Rudnev, M. (2021). Caveats of non-ipsatization of basic values: A review of issues and a simulation study. \emph{Journal of Research in Personality}, 93, 104-118. doi:10.1016/j.jrp.2021.104118
#'
#' Sandy, C. J., Gosling, S. D., Schwartz, S. H., & Koelkebeck, T. (2017). The development and validation of brief and ultrabrief measures of values. \emph{Journal of Personality Assessment}, 99(5), 545-555. doi:10.1080/00223891.2016.1231115
#'
#' Saris, W. E. (Ed.). (1988). \emph{Variation in response functions: A source of measurement error in attitude research}. Amsterdam: Sociometric Research Foundation.
#'
#' Schwartz, S. H. (1992). Universals in the content and structure of values: Theory and empirical tests in 20 countries. In M. Zanna (Ed.), \emph{Advances in experimental social psychology} (Vol. 25, pp. 1-65). New York: Academic Press. doi:10.1016/s0065-2601(08)60281-6
#'
#' Schwartz, S. H. (1996). Value priorities and behavior: Applying a theory of integrated value systems. In C. Seligman, J. M. Olson, & M. P. Zanna (Eds.), \emph{The psychology of values: The Ontario symposium} (Vol. 8, pp. 1-24). Hillsdale, NJ: Erlbaum.
#'
#' Schwartz, S. H., Melech, G., Lehmann, A., Burgess, S., & Harris, M. (2001). Extending the cross-cultural validity of the theory of basic human values with a different method of measurement. \emph{Journal of Cross-Cultural Psychology}, 32, 519-542. doi:10.1177/0022022101032005001
#'
#' Schwartz, S. H. (2003). A proposal for measuring value orientations across nations. \emph{Questionnaire package of the European Social Survey}, 259(290), 261.
#'
#' Schwartz, S. H. (2006). Basic human values: Theory, measurement, and applications. \emph{Revue Francaise de Sociologie}, 47, 929-968.
#'
#' Schwartz, S. H. (2015). Basic individual values: Sources and consequences. In D. Sander & T. Brosch (Eds.), \emph{Handbook of value} (pp. 63-84). Oxford, UK: Oxford University Press. doi:10.1093/acprof:oso/9780198716600.003.0004
#'
#' Schwartz, S. H., Cieciuch, J., Vecchione, M., Torres, C., Dirilem-Gumus, O., & Butenko, T. (2017). Value tradeoffs propel and inhibit behavior: Validating the 19 refined values in four countries. \emph{European Journal of Social Psychology}, 47(3), 241–258. doi:10.1002/ejsp.2228
#'
#' Smith, P. B. (2004). Acquiescent response bias as an aspect of cultural communications style. \emph{Journal of Cross-Cultural Psychology}, 35(1), 50-61. doi:10.1177/0022022103260380
#'
#' Van Rosmalen, J., Van Herk, H., & Groenen, P. J. F. (2010). Identifying response styles: A latent-class bilinear multinomial logit model. \emph{Journal of Marketing Research}, 47(1), 157-172. doi:10.1509/jmkr.47.1.157
#'
#' @docType package
#' @name persval
#' @aliases persval-package persval
"_PACKAGE"
