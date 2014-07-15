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
    structure(list(x=as.data.frame(matrix(1:100, nrow=10)), y='hi there', R=100), class=c('dummy', 'list'))
}

func.dummy.call <- function() {
    func(1,2,3)
}

process <- function(x, name, next.index, cls, indexes) UseMethod("process")

process.list <- function(x, name, next.index, cls, indexes) {
    nms <- names(x)
    nms <- if(is.null(nms)) paste(name, 1:length(x), sep='_')
    else if(any(nms == '')) ifelse(nms=='', paste(name, 1:length(nms), sep='_'), nms)
    else paste(name, nms, sep='_')
    mapply(process, x, nms, 0:(length(x)-1), MoreArgs=list(cls=cls, indexes=c(indexes, next.index)))
}

process.data.frame <- function(x, name, next.index, cls, indexes) {
    mapply(process, x, paste(name, names(x), sep='_'), 0:(ncol(x)-1), MoreArgs=list(cls=cls, indexes=c(indexes, next.index)))
}

process.matrix <- function(x, name, next.index, cls, indexes){
    nms <- dimnames(x)[[2]]
    nms <- if(is.null(nms)) paste(name, 1:ncol(x), sep='_')
    else paste(name, nms, sep='_')
    mapply(process, apply(x, 2, function(x) x), nms, 0:(ncol(x)-1), MoreArgs=list(cls=cls, indexes=c(indexes, next.index)))
}

process.integer <- function(x, name, next.index, cls, indexes) {
    make.leaf(name, cls, 'integer', c(indexes, next.index))
}

process.numeric <- function(x, name, next.index, cls, indexes) {
    make.leaf(name, cls, 'numeric', c(indexes, next.index))
}

process.character <- function(x, name, next.index, cls, indexes) {
    make.leaf(name, cls, 'character', c(indexes, next.index))
}

process.default <- function(x, name, next.index, cls, indexes) {
    make.leaf(name, cls, '', c(indexes, next.index))
}

make.leaf <- function(nm, cls, type, indexes) {
    type.string <- ifelse(type=='character', 'string[]', ifelse(type=='integer', 'int[]', ifelse(type=='numeric', 'double[]', 'PyObject')))
    extract.method <- ifelse(type=='character', 'asStringArray', ifelse(type=='integer', 'asIntArray', ifelse(type=='numeric', 'asDoubleArray', 'asPyObject')))
    cat('\t\tSystem.Workflow.ComponentModel.DependencyProperty ', nm, 'Property = System.Workflow.ComponentModel.DependencyProperty.Register("', nm, '", typeof(', type.string, '), typeof(', cls, '));\n', sep='')
    cat('\t\t[Name("argument ', nm, '")]\n', sep='')
    cat('\t\t[OutputParam]\n')
    cat('\t\t[DescriptionAttribute("see the R doco")]\n')
    cat('\t\t[CategoryAttribute("', nm, ' Category")]\n', sep='')
    cat('\t\t[BrowsableAttribute(true)]\n')
    cat('\t\t[DesignerSerializationVisibilityAttribute(DesignerSerializationVisibility.Visible)]\n')
    cat('\t\tpublic ', type.string, ' ', nm, ' {\n', sep='')
    
    cat('\t\t\tget{\n')
    cat('\t\t\t\tint[] indexes = new int[]{', paste(indexes, collapse=','), '};\n', sep='')
    cat('\t\t\t\tPyObject res = ManagedObjectExtractor.doExtract(result, indexes);\n')
    cat('\t\t\t\treturn ManagedObjectExtractor.', extract.method, '(res);\n', sep='')
    cat('\t\t\t}\n')
    
    cat('\t\t\tset{\n')
    cat('\t\t\t\t//This might be tricky! If I set on the base class, it is out of sync with the result object... yet I do not want to set on the result object.\n')
    cat('\t\t\t\t//Since this is only an output Parameter, is it really necessary of have a Setter anyway? If so, then do I really need to set on the Base class?\n')
    cat('\t\t\t\t//I guess the answer can be found by testing, but If it does need to be set on the base, then Oh well (this and the System.Workflow. ... stuf makes\n')
    cat('\t\t\t\t//me wonder about the design choices here - why not just tell the base type the type of this class and let it get the types for itself\n')
    cat('\t\t\t\t//Note that it is a bit funny for in/out params.\n')
    cat('\t\t\t\t//If one really has to set on the base class, this should happen in the call to execute (for instance, with a stupid call like "this.x=this.x").\n')
    cat('\t\t\t}\n')
    
    cat('\t\t}\n\n')
}

dump.using.statements <- function(){
    dumps <- c(
        "using System;",
        "using System.ComponentModel;",
        "using System.ComponentModel.Design;",
        "using System.Collections;",
        "using System.Drawing;",
        "using System.Linq;",
        "using System.Workflow.ComponentModel;",
        "using System.Workflow.ComponentModel.Design;",
        "using System.Workflow.ComponentModel.Compiler;",
        "using System.Workflow.ComponentModel.Serialization;",
        "using System.Workflow.Runtime;",
        "using System.Workflow.Activities;",
        "using System.Workflow.Activities.Rules;",
        "using System.Globalization;",
        "using System.Text;",
        "using Microsoft.Research.ScientificWorkflow;"
    )
    cat(dumps, sep='\n')
    cat('\n')
}

#--------------------- IMPORTANT NOTE ------------------------
# there is a potentially nasty problem in the following.
# I if the input and output parameters have the same names,
# I will create two properties of the same name... which
# will cause problems.
#
# The solution is to compare the list of input and output
# argument names and for those that occur in both, make them
# 'in/out' parameters. This could be a little tricky, because
# if, for an out or in/out, I set on the base class (as they
# do in the example wrappers I have looked at, the base goes
# out of sync with the result object... yet I do not want to
# set on the result object, and I don't really want to extract
# everything from the result object (as this just takes up
# more space and time. See the note in the setters of the output
# parameters for more comment on this.
#------------------ END OF IMPORTANT NOTE --------------------
dump.func <- function(f, output.file) {
    # open the xml for the function
    sink(output.file)
    
    #function for dumping a single argument
    arg.dumper <- function(nm, index, cls) {
        cat('\t\tSystem.Workflow.ComponentModel.DependencyProperty ', nm, 'Property = System.Workflow.ComponentModel.DependencyProperty.Register("', nm, '", typeof(object), typeof(', cls, '));\n', sep='')
        cat('\t\t[Name("argument ', nm, '")]\n', sep='')
        cat('\t\t[RequiredInputParam]\n')
        cat('\t\t[DescriptionAttribute("see the R doco")]\n')
        cat('\t\t[CategoryAttribute("', nm, ' Category")]\n', sep='')
        cat('\t\t[BrowsableAttribute(true)]\n')
        cat('\t\t[DesignerSerializationVisibilityAttribute(DesignerSerializationVisibility.Visible)]\n')
        cat('\t\tpublic object ', nm, ' {\n', sep='')
        
        cat('\t\t\tget {\n')
        cat('\t\t\t\t//Do I really need to set this on the base (though for input parameters it is not such a problem... just a waste of space)?\n')
        cat('\t\t\t\treturn (object)(base.GetValue(', cls, '.', nm, 'Property));\n', sep='')
        cat('\t\t\t}\n')
        
        cat('\t\t\tset {\n')
        cat('\t\t\t\targs[', index, '] = RContext.makePyObject(value);\n', sep='')
        cat('\t\t\t\tbase.SetValue(', cls, '.', nm, 'Property, value);\n', sep='')
        cat('\t\t\t}\n')
        
        cat('\t\t}\n\n')
    }
    
    # get the necessary bits and pieces from the function
    func.name <- as.character(substitute(f))
    ags <- formals(func)
    
    # dump the header, create the namespace and open the class
    dump.using.statements()
    cat('\nnamespace Cool.Stuff.From.Simon {\n')
    cat('\t[Name("', func.name, '")]\n', sep='')
    cat('\t[Description("does stuff")]\n')
    cat('\tpublic class ', func.name, ': Activity\n\t{\n', sep='')
    
    # write the private members
    cat('\t\tprivate PyObject result;\n')
    cat('\t\tprivate static string method = "', func.name, '";\n', sep='')
    cat('\t\tprivate PyObject[] args = new PyObject[]{', length(ags), '};\n')
    cat('\n')
    
    # dump the argument definitions
    mapply(arg.dumper, names(ags), 0:(length(ags)-1), MoreArgs=list(cls=func.name))
    
    # dump the return type (if it exists)
    test.func.name <- paste(func.name, '.dummy.call', sep='')
    if(exists(test.func.name)) {
        result <- do.call(test.func.name, list())
        process(result, 'result', NULL, func.name, NULL)
    }
    
    # write the execute function
    cat('\t\tpublic void Execute()\n\t\t{\n')
    cat('\t\t\tresult = R.instance().doCall(method, args);\n')
    cat('\t\t}\n')
    
    # close the class definition and namespace.
    cat('\t}\n}\n')
    sink()
}

dump.func(func, 'func.cs')
