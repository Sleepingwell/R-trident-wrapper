#   Copyright 2010 Simon Knapp
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#       http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

func <- function(x, y, R=function(x) print(x)) {
    structure(list(x=as.data.frame(matrix(1:100, nrow=10)), y='hi there', R=integer(100)), class=c('dummy', 'list'))
}

func.dummy.call <- function() {
    func(1,2,3)
}

extendTabs <- function(tabs) list(paste(tabs, '\t', sep=''))
process <- function(x, name, tabs) UseMethod("process")
process.default <- function(x, name, tabs) NextMethod("process")

process.list <- function(x, name, tabs) {
    cat(tabs, '<result name=', name, ' type="list" leaf="FALSE">\n', sep='')
    nms <- names(x)
    nms <- if(is.null(nms)) paste(name, 1:length(x), sep='_')
    else if(any(nms == '')) ifelse(nms=='', paste(name, 1:length(nms), sep='_'), nms)
    else paste(name, nms, sep='_')
    mapply(process, x, nms, MoreArgs=extendTabs(tabs))
    cat(tabs, '</result>\n')
}

process.data.frame <- function(x, name, tabs) {
    cat(tabs, '<result name=', name, ' type="data.frame" leaf="FALSE">\n', sep='')
    mapply(process, x, paste(name, names(x), sep='_'), MoreArgs=extendTabs(tabs))
    cat(tabs, '</result>\n')
}

process.matrix <- function(x, name, tabs){
    cat(tabs, '<result name=', name, ' type="matrix" leaf="FALSE">\n', sep='')
    nms <- dimnames(x)[[2]]
    nms <- if(is.null(nms)) paste(name, 1:ncol(x), sep='_')
    else paste(name, nms, sep='_')
    mapply(process, apply(x, 2, function(x) x), nms, MoreArgs=extendTabs(tabs))
    cat(tabs, '</result>\n')
}

process.integer <- function(x, name, tabs) cat(tabs, '<result name="', name, '" type="integer" leaf="TRUE"/>\n', sep='')

process.numeric <- function(x, name, tabs) cat(tabs, '<result name="', name, '" type="numeric" leaf="TRUE"/>\n', sep='')

process.character <- function(x, name, tabs) cat(tabs, '<result name="', name, '" type="character" leaf="TRUE"/>\n', sep='')

process.default <- function(x, name, tabs) cat(tabs, '<result name="', name, '" type="unknown" leaf="TRUE"/>\n', sep='')

dump.func <- function(f) {
    arg.dumper <- function(nm, val) {
        cat('\t<argument name="', nm, sep='')
        if(val=='') cat('"/>\n')
        else cat('" value="', deparse(val), '"/>\n', sep='')
    }
    
    # open the xml for the function
    func.name <- substitute(f)
    cat('<function name="', func.name, '">\n', sep='')
    
    # dump the argument definitions
    ags <- formals(func)
    mapply(arg.dumper, names(ags), ags)
    
    # dump the return type
    result <- do.call(paste(func.name, '.dummy.call', sep=''), list())
    process(result, 'result', '\t')
    
    # close the xml for the function
    cat('</function>\n')
}

dump.func(func)
