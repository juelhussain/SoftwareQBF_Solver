
# This file is part of qbf2epr.
# qbf2epr is a tool for transforming QBF to 
# various more expressive logics like EPR

#Copyright (c) 2011, 2012, Martina Seidl, Robert Aistleitner, Gregor Dorfbauer, JKU Linz

#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


import qbfparser
import sys

import optparse

if __name__ == "__main__":
    parser = optparse.OptionParser()

    parser.add_option("-i", "--infile", dest="infile",
                help="qdimacs input FILE", metavar="FILE",
                default="-", type="string", action="store")

    parser.add_option("-o", "--outfile", dest="outfile",
                help="output FILE", metavar="FILE",
                default="-", type="string", action="store")

    parser.add_option("-t", "--type", dest="type",
                help="transformation type ([q]dimacs, [e]pr with prefix, epr with [d]ependencies, [f]irst order, epr in [c]nf, [s]atallax, QBF in qp[r]o format)", metavar="TYPE",
                default="e", type="string", action="store")

    parser.add_option("-p", "--optimize-predicates", dest="optimize_predicates", 
                action="store_true", default=False)

    parser.add_option("-f", "--disable-formatcheck", dest="disable_formatcheck", 
                action="store_true", default=False)
    
    parser.add_option("-q", "--quiet", dest="quiet", 
                action="store_true", default=False)
 

    (options, args) = parser.parse_args()

    if options.type in ("v"):
	print "qbf2epr version 1.0"
	sys.exit(1)
    if options.type not in ("q", "e", "c","s","f","d","r"):
        print "Please specify correct transformation type [q,e,c,s,f,d,r]"
        sys.exit(1)
    
    infile = sys.stdin
    if options.infile != "-":
        try: 
            infile = open(options.infile, "r")            
        except IOError:
            print "Input file %s not readable" % options.infile
            sys.exit(1)

    outfile = sys.stdout
    if options.outfile != "-":
        try: 
            outfile = open(options.outfile, "w")
        except IOError:
            print "Output file %s not writeable" % options.outfile
            sys.exit(1)

    p = qbfparser.QBFParser(options.quiet)
    if p.parseFile(infile, options.disable_formatcheck):
        if options.type == "q":
            p.outputQBF(outfile)
        elif options.type == "r":
            p.outputQPRO(outfile)
        elif options.type == "v":
	    print("qbf2epr: version 1.0")
            sys.exit(1)
        elif options.type == "f":
            p.outputFO(outfile)
        elif options.type == "e":
            p.outputEPR(outfile, True)
        elif options.type == "s":
            p.outputHOL(outfile)
        elif options.type == "d":
            p.outputCNFDep(outfile, True, options.infile)
        elif options.type == "c":
            p.outputCNF(outfile, True)
    
    else:
        print "Error in parsing file %s" % options.infile
    infile.close()
    outfile.close()
