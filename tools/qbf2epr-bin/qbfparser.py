
# This file is part of qbf2epr.
# qbf2epr is a tool for transforming QBF to 
# various more expressive logics like EPR

#Copyright (c) 2011, 2012, Martina Seidl, Robert Aistleitner, Gregor Dorfbauer, JKU Linz

#Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

#The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

#THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

from model import Variable, QuantifierList, Clause
import os
import socket
import time
import re
            
class QBFParser:
    def __init__(self, quiet):
        self.__quiet = quiet == True
        self.__sourceFile = None

        self.__nrVar = 0
        self.__nrClause = 0
        self.__commentList = []
        self.__quantifierList = []
        self.__clauseList = []

        self.__savedLine = None
        self.__lastLine = None
        self.__lastQType = None
        
    def __log(self, message, level="DEBUG"):
        if level != "DEBUG" and not self.__quiet:
            print "LOG [%s]: %s" % (level, message)
        
    def outputQPRO(self, outfile):
        for c in self.__commentList:
            outfile.write("c %s\n" % c)
        outfile.write("QBF\n%d\n q\n" % (self.__nrVar))

        for q in self.__quantifierList:
            outfile.write("%s\n" % q)

	outfile.write("c\n\n\n")
  
      	for c in self.__clauseList:
            outfile.write("d\n")
	    for l in c.varlist:
	      if (not l.inv):
		outfile.write(" %d" % l.name)
	    outfile.write("\n")
	    for l in c.varlist:
	      if (l.inv):
		outfile.write(" %d" % l.name)
	    outfile.write("\n")
	    outfile.write("/d\n")


	outfile.write("/c\n/q\n")
    def outputQBF(self, outfile):
        """
            1:1 transformation - after reading qdimacs, write out qdimacs again to check 
        """

        for c in self.__commentList:
            outfile.write("c %s\n" % c)
        outfile.write("p cnf %d %d\n" % (self.__nrVar, self.__nrClause))
        
        for q in self.__quantifierList:
            outfile.write("%s 0\n" % q)
        
        for c in self.__clauseList:
            outfile.write("%s 0\n" % c)
        return True

    def outputEPR(self, outfile,optimize):
        # actual allquantor-id
        actaq = 1
        # actual eskalation degree
        actesk = 1
        actAllquantors = []
        actExquantors = []

        #contains a rename-mapping of old variable names (1,2,3) to new (X1 or esk(...)).
        newVariableNames = {}

        for q in self.__quantifierList:
            if q.qtype == "a":
                #rename all-quantified variables to X.
                for x in q.getVariableNames():
                    newVarName = "X%d" % actaq
                    newVariableNames[x] = ("a", newVarName)
                    actAllquantors.append(newVarName)
                    actaq += 1
            elif q.qtype == "e":
		actesk = 1
                #rename existential-quantified variables to skolemized ones
                for x in q.getVariableNames():
                    if actaq-1 == 0:
			newVarName = "Y%d" % actesk
                        newVariableNames[x] = ("f",newVarName)
		 	actExquantors.append(newVarName) 
                    else:
                        newVariableNames[x] = ("e", "esk%d_%d(%s)" % (actesk, actaq-1, ",".join(actAllquantors)))
                    actesk += 1
	outfile.write("fof(quant,axiom,(")
	if (actExquantors != []): 
		outfile.write("! ["+",".join(actExquantors)+"] : ")

	if (actAllquantors != []): 
		outfile.write("? ["+",".join(actAllquantors)+"] : ")
        

	actclause = 1
        for c in self.__clauseList:
            clause = []
            vlist = []
	    if actclause != 1:
		  outfile.write(" & ")
	    outfile.write("(")
            for var in c.varlist:
                inv = ""
                if var.inv:
                    inv = "~"
                #remove predicate on existential quantified and skolemized terms   
                if optimize and newVariableNames[var.name][0] == "e":
                    vlist.append("%s%s" % (inv, newVariableNames[var.name][1]))
                else:
                    vlist.append("%sp(%s)" % (inv,newVariableNames[var.name][1]))

            clause.append(" | ".join(vlist))
            clause.append(")")
            
            outfile.write("".join(clause))
            actclause += 1
	outfile.write(")).")
	

    def outputFO(self, outfile):
        """
            write a first order formula (using quantifiers ? !) and 
	    predicates p.
            Every variable gets re-written into V1, V2, ...
        """

#fof(axiom_0,axiom,
#   ( ! [V2] : ? [V1]:
#       ( p(V2) | p(V1) ) & (~p(V2) | ~p(V1) ) & ( p(V2) | ~p(V1) ) )).
#        outfile.write("""cnf(rule_true,axiom, p(1)).
#cnf(rule_false,axiom, ~p(0)).
#""")
        outfile.write("fof(quant,axiom,(\n\t")
        for q in self.__quantifierList:

            if q.qtype == "a":
                outfile.write(" ! ")
            elif q.qtype == "e":
                outfile.write(" ? ")
            variables = ["V%d" % x for x in q.getVariableNames()]
            
            outfile.write("[ %s ] :  \n\t" % ",".join(variables))
        clauselist = []   
        outfile.write(" ( \n\t p(true) & ~p(false) & \n  ") 
        for c in self.__clauseList:
            clause = []
            clause.append("( ")
            vlist = []
            for var in c.varlist:
                if var.inv:
                    vlist.append("~p(V%s)" % var.name)
                else:
                    vlist.append(" p(V%s)" % var.name)
            clause.append(" | ".join(vlist))
            clause.append(") ")
            clauselist.append("".join(clause))
        outfile.write("\n\t & ".join(clauselist))
        outfile.write(" ) ");
        outfile.write("\n)).")
            
    def outputCNFDep(self, outfile, optimize, infile):
        """
            Write out a skolemized CNF by using standard dependency scheme. Every quantified variable gets rewritten into:
                - forall: X1, X2, ...
                - exists: esk_$nr_$degree( all outer depending for-all variables).

            The for-all-variables use the predicate p(X1).
            If optimize is set, all existential variables (which use esk_ predicates) strip the usage
            of p(esk()) because one of those predicates should be enough.    
        """

	os.system("depService %s 2020 &" % infile)
	

	count = 0
	while True :
	  try :
	    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
	    s.connect(('localhost', 2020))
	    count = count + 1
	    if count > 10 :
		print "Cannot connect to socket"
	    else :
	        break
	  except : 
	    time.sleep(2)

        # actual allquantor-id
        actaq = 1
        # actual eskalation degree
        actesk = 1
        actAllquantors = []
        varDeps = [[] for i in range(self.__nrVar+1)]

        #contains a rename-mapping of old variable names (1,2,3) to new (X1 or esk(...)).
        newVariableNames = {}

        for q in self.__quantifierList:
            if q.qtype == "a":
                #rename all-quantified variables to X.
                for x in q.getVariableNames():
                    newVarName = "X%d" % actaq
                    newVariableNames[x] = ("a", newVarName)
                    actaq += 1
	       	    s.send('deps')
		    v1 = "%s" % x
	    	    data = s.makefile().readline()
	            s.send(v1)
	  	    data = s.makefile().readline()
		    da_re = re.compile(r' ')
		    list = da_re.split(data)
		    for z in list: 
			if z != "unused\n" and z != 0:
			    varDeps[int(z)].append(newVarName)

	actaq = 1
	for q in self.__quantifierList:
	    if q.qtype == "e":
	       actesk = 1
	       for x in q.getVariableNames():
                   if varDeps[x] == []:
                        newVariableNames[x] = ("e", "esk%d_%d" % (actesk, actaq-1))
                   else:
                        newVariableNames[x] = ("e", "esk%d_%d(%s)" % (actesk, actaq-1, ",".join(varDeps[x])))
                   actesk += 1
            else: actaq += 1

        actclause = 1
        for c in self.__clauseList:
            clause = []
            clause.append("cnf(i_0_%d,plain,(" % actclause)
            vlist = []
            for var in c.varlist:
                inv = ""
                if var.inv:
                    inv = "~"
                #remove predicate on existential quantified and skolemized terms   
                if optimize and newVariableNames[var.name][0] == "e":
                    vlist.append("%s%s" % (inv, newVariableNames[var.name][1]))
                else:
                    vlist.append("%sp(%s)" % (inv,newVariableNames[var.name][1]))

            clause.append(" | ".join(vlist))
            clause.append(")).\n")
            
            outfile.write("".join(clause))
            actclause += 1
	s.send('quit')
        s.close()
        #output base cases to introduce the predicate p.
        outfile.write("cnf(rule_true,axiom, p(1)).\n")
        outfile.write("cnf(rule_false,axiom, ~p(0)).\n")


    def outputHOL(self, outfile):
      outfile.write("thf(c,conjecture,")
      brack = ""
      for q in self.__quantifierList:
	brack += ")"
	outfile.write("(")
	if q.qtype == "a": 
	  outfile.write("! [")
	else:
	  outfile.write("? [")
	
        outfile.write(", ".join(map(lambda x: ("X%s : $o " % x),q.getVariableNames())))
	outfile.write("] : ")
	
      outfile.write("("+" & ".join(map(lambda c:
		"("+"|".join(map(lambda l: l.inv and  "~(X%s)" % l.name or "X%s" % l.name,c.varlist))+")", 
					self.__clauseList))+")")        

      outfile.write(brack+").")



    def outputCNF(self, outfile, optimize):
        """
            Write out a skolemized CNF. Every quantified variable gets rewritten into:
                - forall: X1, X2, ...
                - exists: esk_$nr_$degree( all outer for-all variables).

            The for-all-variables use the predicate p(X1).
            If optimize is set, all existential variables (which use esk_ predicates) strip the usage
            of p(esk()) because one of those predicates should be enough.    
        """

        # actual allquantor-id
        actaq = 1
        # actual eskalation degree
        actesk = 1
        actAllquantors = []

        #contains a rename-mapping of old variable names (1,2,3) to new (X1 or esk(...)).
        newVariableNames = {}

        for q in self.__quantifierList:
            if q.qtype == "a":
                #rename all-quantified variables to X.
                for x in q.getVariableNames():
                    newVarName = "X%d" % actaq
                    newVariableNames[x] = ("a", newVarName)
                    actAllquantors.append(newVarName)
                    actaq += 1
            elif q.qtype == "e":
                #rename existential-quantified variables to skolemized ones
                for x in q.getVariableNames():
                    if actaq-1 == 0:
                        newVariableNames[x] = ("e", "esk%d_%d" % (actesk, actaq-1))
                    else:
                        newVariableNames[x] = ("e", "esk%d_%d(%s)" % (actesk, actaq-1, ",".join(actAllquantors)))
                    actesk += 1

        actclause = 1
        for c in self.__clauseList:
            clause = []
            clause.append("cnf(i_0_%d,plain,(" % actclause)
            vlist = []
            for var in c.varlist:
                inv = ""
                if var.inv:
                    inv = "~"
                #remove predicate on existential quantified and skolemized terms   
                if optimize and newVariableNames[var.name][0] == "e":
                    vlist.append("%s%s" % (inv, newVariableNames[var.name][1]))
                else:
                    vlist.append("%sp(%s)" % (inv,newVariableNames[var.name][1]))

            clause.append(" | ".join(vlist))
            clause.append(")).\n")
            
            outfile.write("".join(clause))
            actclause += 1

        #output base cases to introduce the predicate p.
        outfile.write("cnf(rule_true,axiom, p(1)).\n")
        outfile.write("cnf(rule_false,axiom, ~p(0)).\n")

             
    def parseFile(self, infile, disable_formatcheck):
        self.__log("Parsing %s" % infile)

        error = False
        self.__sourceFile = infile

        if self.__sourceFile != None:
            if not self.__parseHeader():
                self.__log("Header malformed!", "SEVERE")
                error = True
            if not self.__parseQuantifiers():
                self.__log("Quantifiers malformed!", "SEVERE")
                error = True
            if not self.__parseClauses():
                self.__log("Clauses malformed!", "SEVERE")
                error = True
            if not (self.__nrVar == self.__checkNrVars()):
                self.__log("Nr. Variables mismatch: %d != %d" % ( self.__nrVar, self.__checkNrVars()), "SEVERE")
                error = True
            if disable_formatcheck and error:
                self.__log("Disabling format check, continuing", "SEVERE")
                error = False               
        else:
            error = True
        return not error
            
    def __checkNrVars(self):
        """ checks if all variables set in header are at least in one quantifier list or in one clause.
        """
        variables = set()
        for q in self.__quantifierList:
            for var in q.getVariableNames():
                variables.add("%s" % var)
        for c in self.__clauseList:
            for var in c.getVariableNames():
                variables.add("%s" % var)
                
        return len(variables)

    def __pushBackLine(self):
        self.__savedLine = self.__lastLine
    def __nextLine(self):
        if self.__savedLine != None:
            self.__savedLine = None
            return self.__lastLine
        self.__lastLine = self.__sourceFile.readline().strip()
        return self.__lastLine
        
    def __parseHeader(self):
        """
            parse comment header and saves the size of the file (number of clauses and variables)
        """
        line = self.__nextLine()
        while line.startswith("c"):
            self.__log("adding comment %s" % line)
            self.__commentList.append(line[2:])
            line = self.__nextLine()
        parts = line.split()
        if len(parts) == 4 and parts[0] == "p" and parts[1] == "cnf":
            self.__nrVar = int(parts[2])
            self.__nrClause = int(parts[3])
            assert(self.__nrVar > 0 and self.__nrClause > 0)
            return True
        else:
            return False
            
    def __parseQuantifiers(self):
        """
            parse Quantifier section of QDIMACS file
                check for: alternating quantifiers, last quantifier must be "e"
                variable objects are generated here
        """
        line = self.__nextLine()
        
        while line[0] in ("e", "a"):   
            parts = line.split()
            if len(parts) > 2:
                typ = parts[0]
                if self.__lastQType == None:
                    self.__lastQType = typ
                    
                elif self.__lastQType == typ:
                    self.__log("Not changing quantifiers", "SEVERE")
                    return False 
                else:
                    self.__lastQType = typ
                if parts[-1] == "0":
                    variables = [Variable(x) for x in parts[1:-1] ]
                    q = QuantifierList(typ, variables)
                    self.__quantifierList.append(q)
                else:
                    self.__log("Quantifier line not terminated with 0","SEVERE")
                    return False
            else:
                self.__log("Quantifier line too short: %s" % line, "SEVERE")
                return False
            
            line = self.__nextLine()
        self.__pushBackLine()
        if self.__lastQType == "e":
            return True
        else:
            self.__log("Not ending with e quantifier")
            return False
        
    def __parseClauses(self):
        """ parse clauses from QDIMACS file and do sanity checking (nr. of clauses correctly)   
        """
        line = self.__nextLine()
        
        while line != "":
               
            self.__log("clause line %r" % (line))
            
            parts = line.split()
            if len(parts) > 1:
                if parts[-1] == "0":
                    variables = [Variable(x) for x in parts[:-1]]
                    c = Clause(variables)
                    
                    self.__clauseList.append(c)
                else:
                    self.__log("Clause not terminated with 0", "SEVERE")
                    return False
            else:
                self.__log("Clause line too short: %s" % line, "SEVERE")
                return False
            line = self.__nextLine()

        if len(self.__clauseList) == self.__nrClause:
            return True
        else:
            self.__log("Number of clauses diffs: %d != %d" % (len(self.__clauseList), self.__nrClause))
            return False
            
