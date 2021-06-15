sc_inv_predict <- function(newdata, model, transform = identity, 
                           inv.transform = identity) {
  if (!is.data.frame(newdata))
    stop("newdata must be a data.frame")
  vars <- all.vars(formula(model))
  if (length(vars) != 2)
    stop("standard curve model must have exactly 1 independent and 1 dependent 
         variable")
  if (!vars[1] %in% colnames(newdata))
    stop(sprintf("variable %s must be present in newdata", vars[1]))
  if (vars[2] %in% colnames(newdata))
    stop(sprintf("variable %s is already present in newdata... stopping so as
                 not to overwrite.", vars[2]))
  co <- coef(model)
  if(length(co) != 2)
    stop("standard curve model must have intercept and slope")
  b <- co[1]
  m <- co[2]
  newdata[vars[2]] <- inv.transform((transform(newdata[vars[1]]) - b) / m)
  newdata
}