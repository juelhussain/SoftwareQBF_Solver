
class Variable:
    def __init__(self, name):
        if name[0] == "-":
            self.inv = True
            self.name = int(name[1:])
        else:
            self.inv = False
            self.name = int(name)
        
    def __str__(self):
        return "%s%d" % (self.inv and "-" or "", self.name)   


class QuantifierList:
    def __init__(self, qtype, varlist):
        self.qtype = qtype
        self.varlist = varlist
    def getVariableNames(self):
        return [x.name for x in self.varlist]
    def __str__(self):
        return "%s %s" % (self.qtype, " ".join(["%s" % x for x in self.varlist]))
        

class Clause:
    def __init__(self, varlist):
        self.varlist = varlist
    def getVariableNames(self):
        return [x.name for x in self.varlist]
    def __str__(self):
        return " ".join(["%s" % x for x in self.varlist])
        
    def addVar(self, var):
        if var not in addVar:
            addVar.append(var)
            return True
        else:
            return False
            
    def delVar(self, var):
        if var in addVar:
            addVar.remove(var)
            return True
        else:
            return False
