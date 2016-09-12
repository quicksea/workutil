#' interpolate x based on y. Usually x is vdm, y is sigma, interpolate the required vdm@-1s, 0s and 1s
#'
#'take a data.table and interpolate x based y level
#'@param dt a data.table
#'@param x x colname string
#'@param y y colname string
#'@param grp group by column string
#'@param already_ordered if the table is already sorted
#'@return a table.table giving lowbound highbound and fitted solution
#'@export
#'@import data.table
interpolatex <-
  function(dt,target = c(0),x = 'x', y = 'y', grp = 'grp', already_ordered =
             FALSE) {
    dt <- dt[, c(x,y,grp), with = FALSE];
    colnames(dt) <- c('x', 'y', 'grp');
    if (!already_ordered) {
      setorder(dt,grp,x);
    }
    for (i in 1:length(target)) {
      tari = target[i];
      dt[,flag := ifelse((y - tari) <= 0 &
                              (shift(y,type = 'lead') - tari) > 0, 'lowbound',
                            ifelse((y - tari) >= 0 &
                                     (shift(y,type = 'lag') - tari) < 0, 'highbound',
                                   'other')
      )]
      dtsub <- dt [flag == 'lowbound' | flag == 'highbound'];
      dtsub [, target := tari]
      if (i == 1) {
        dtcat = dtsub
      } else {
        dtcat = rbindlist(list(dtcat,dtsub));
      }
    }
    dtcat <- dcast(dtcat, grp + target ~ flag, value.var = c("x","y"));
    dtcat[, fitvalue := (x_highbound - x_lowbound) / (y_highbound - y_lowbound) *
            (target - y_lowbound) + x_lowbound];
    return(dtcat);
  }
