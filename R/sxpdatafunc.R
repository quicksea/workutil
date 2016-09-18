#' interpolate x based on y. Usually x is vdm, y is sigma, interpolate the required vdm@-1s, 0s and 1s
#'

#'take a data.table and interpolate x based y level
#'@param dt a data.table
#'@param x x colname string
#'@param y y colname string
#'@param grp group by column string
#'@param already_ordered if the table is already sorted
#'@return a table.table giving lowbound highbound and fitted solution
#'@import data.table
#'@export
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

#'take a data.table, split col into multiple columns based separator
#'
#'in place change to dt
#'@param dt a data.table
#'@param col column to split, no quote
#'@param sep1 level 1 sep1
#'@param sep2 level 1 sep2
#'@param mode default 1
#'@param splitcolnames used in mode 0, names of splitted columns
#'@return no return, inplace change to dt
#'@examples
#'mode 1: split "X:1_Y:2" into two columns X,Y, fill with 1,2;
#'mode 0: split "1_2" into 1 and 2, column names defined by splitcolnames if given, otherwise use V1,V2...
#'@export
splitcol <- function(dt, col, sep1 = '_', sep2 = ':', mode = 1, splitcolnames = NULL) {
  grp = deparse(substitute(col))
  if (mode == 1) {
    str1 = dt[1,grp];
    headerlist = as.list(unlist(strsplit(str1, sep1)));
    namesplit <- function(x, sep) {unlist(strsplit(x,sep))[1]};
    headers = unlist(lapply(headerlist, namesplit, sep2));
    dt [, headers := tstrsplit(grp, sep1), with=FALSE];
    for (header in headers) {
      dt [, eval(header) := tstrsplit(get(header), sep2)[2]]
    }
  } else if (mode == 0) {
    str1 = dt[1,grp];
    headerlist = as.list(unlist(strsplit(str1, sep1)));
    n = length(headerlist)
    if (is.null(splitcolnames)) {splitcolnames=paste0(rep('V', n),1:n)}
    dt [, splitcolnames := tstrsplit(grp, sep1), with=FALSE];
  }
}
