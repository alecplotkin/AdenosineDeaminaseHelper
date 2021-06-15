sc_inv_predict <- function(model, newdata, yname, newxname = "activity",
                           transform = identity, inv.transform = identity) {
  if (!is.data.frame(newdata))
    stop("newdata must be a data.frame")
  co <- coef(model)
  if(length(co) != 2)
    stop("model must have exactly 2 terms")
  b <- co[1]
  m <- co[2]
  newdata[newxname] <- inv.transform((transform(newdata[yname]) - b) / m)
  newdata
}