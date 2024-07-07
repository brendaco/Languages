import sys
import time
TIMEOUT = 1
ARGS_ALLOWED = 4

"""
You might be wondering: "Why is it so long?"
Let us explain... this is an extremely expandable "translator"
It can translate to any language with a few steps!!
All that matters is there is a translation dictionary for variable 
assignment, loops, function definition, function calls, and conditionals
The grammar is very adaptable, there is very little "hard code", so it 
is possible to add and modify features very easily.
"""

class PyVal:
    def __init__(self, val) -> None:
        self.val = val[0]
        typ = val[-1].lstrip("!t")
        if typ == "num":
            self.val = AExpr(self.val, "python")
            self.val = str(self.val)
        elif typ == "string":
            temp_langstring = "\""
            escape = False
            for c in self.val[0]:
                if c == "\\":
                    escape = True
                elif escape:
                    escape = False
                    if c == "s":
                        temp_langstring += " "
                    elif c == "n":
                        temp_langstring += "\\\n"
                    elif c == "t":
                        temp_langstring += "\t"
                    else:
                        temp_langstring += "\\" + c
                elif c == "{":
                    temp_langstring += "\" + str("
                elif c == "}":
                    temp_langstring += ") + \""
                else:
                    temp_langstring += c
            temp_langstring += "\""
            self.val = temp_langstring
        elif typ == "boolVal":
            self.val = LExpr(self.val, "python")
            self.val = str(self.val)
        elif typ == "var":
            self.val = self.val[0]
    def __str__(self) -> str:
        return self.val

class AExpr:
    def __init__(self, expr, language) -> None:
        self.expr = expr
        if language == "python":
            self.langstring = self.build_pystring(expr)
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)
    
    def build_pystring(self, expr):
        pyops = {
            "less": " < ",
            "lessEq": " <= ",
            "eq": " == ",
            "notEq": " != ",
            "grtrEq": " >= ",
            "grtr": " > ",

            "add": " + ",
            "sub": " - ",
            "mult": " * ",
            "pow": " ** ",
            "fdiv": " / ",
            "idiv": " // ",
            "mod": " % "
        }
        if type(expr[0]) == str:
            return expr[0]
        left = self.build_pystring(expr[0][0])
        op = expr[1][0][0].lstrip("!t")
        right = self.build_pystring(expr[2][0])
        return "(" + left + pyops[op] + right + ")"

class LExpr:
    def __init__(self, expr, language) -> None:
        self.expr = expr
        if language == "python":
            self.langstring = self.build_pystring(expr)
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)
    
    def build_pystring(self, expr):
        pyops = {
            "less": " < ",
            "lessEq": " <= ",
            "eq": " == ",
            "notEq": " != ",
            "grtrEq": " >= ",
            "grtr": " > "
        }
        if type(expr[0]) == str:
            if expr[0] == "true" or expr[0] == "false":
                return expr[0][0].upper() + expr[0][1:]
            else:
                return expr[0]
        left = self.build_pystring(expr[0][0])
        op = expr[1][0][0].lstrip("!t")
        right = self.build_pystring(expr[2][0])
        if op == "xor":
            return "((" + left + " and not " + right + ") or (" + right + " and not " + left + "))"
        elif op == "and" or op == "or":
            return "(" + left + " " + op + " " + right + ")"
        else:
            return "(" + left + pyops[op] + right + ")"

class VarAssign:
    def __init__(self, var, val, language, indent="") -> None:
        self.var = var[0][0]
        self.val = str(PyVal(val))
        self.indent = indent
        if language == "python":
            if self.var[:3] == "arg":
                self.var = "args" + "[" + str(self.var[3]) + "]"
            elif self.var == "return":
                self.var = "retval"
            self.langstring = self.var + " = " + self.val + "\n"
    def func_var(self):
        self.var = self.var + "_args"
        if self.val[:4] != "args":
            self.langstring = "args[0] = " + self.val + "\n" + self.var + " = args[:1]\n"
        else:
            self.langstring = self.var + " = " + self.val[:4] + "[:" + self.val[4] + "]\n"
    def func_val(self):
        self.val = self.val + "_result"
        self.langstring = self.var + " = " + self.val + "\n"
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)

class FunCall:
    def __init__(self, name, language, indent="") -> None:
        self.name = name[0][0]
        if language == "python":
            self.langstring = self.name + "_result = " + self.name + "(*" + self.name + "_args)\n"
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)

class Pass:
    def __init__(self, language, indent="") -> None:
        if language == "python":
            self.langstring = "pass\n"
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)

class FunAssign:
    def __init__(self, name, procedure, language, indent="") -> None:
        global ARGS_ALLOWED
        self.name = name[0][0]
        self.procedure = procedure
        if language == "python":
            argstring = ""
            for i in range(ARGS_ALLOWED):
                argstring += "arg" + str(i) + "=None, "
            argstring = argstring.rstrip(", ")
            new_proc = ""
            for line in self.procedure:
                new_proc += indent + "    " + str(line)
            self.langstring = "def " + self.name + "(" + argstring + "):\n" + indent + "    retval = None\n" + new_proc + indent + "    return retval\n"
    def get_sublist(self):
        return self.procedure
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)

class Loop:
    def __init__(self, condition, procedure, language, indent="") -> None:
        self.condition = LExpr(condition[0], language)
        self.procedure = procedure
        if language == "python":
            new_proc = ""
            for line in self.procedure:
                new_proc += indent + "    " + str(line)
            self.langstring = "while " + str(self.condition) + ":\n" + new_proc
    def get_sublist(self):
        return self.procedure
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)

class Conditional:
    def __init__(self, condition, procedure_true, procedure_false, language, indent="") -> None:
        self.condition = LExpr(condition[0], language)
        self.procedure_true = procedure_true
        self.procedure_false = procedure_false
        if language == "python":
            self.str_proc_true = ""
            for line in self.procedure_true:
                self.str_proc_true += indent + "    " + str(line)
            self.str_proc_false = ""
            for line in self.procedure_false:
                self.str_proc_false += indent + "    " + str(line)
            self.langstring = "if " + str(self.condition) + ":\n" + self.str_proc_true + indent + "else:\n" + self.str_proc_false
    def get_sublist(self):
        return self.procedure_true + self.procedure_false
    def __str__(self) -> str:
        return self.langstring
    def __repr__(self) -> str:
        return str(self)

def main():
    global TIMEOUT
    global ARGS_ALLOWED
    grammar = {
        ("!tvarAssign", "!t;") : "!tcodeBlock",
        ("!tfunAssign", "!t;") : "!tcodeBlock",
        ("!tconditional", "!t;") : "!tcodeBlock",
        ("!tloop", "!t;") : "!tcodeBlock",
        ("!tfunCall", "!t;") : "!tcodeBlock",
        ("!tpass", "!t;") : "!tcodeBlock",
        ("!tcodeBlock", "!tcodeBlock") : "!tcodeBlock",

        ("!tvar", "!t<-", "!tvar") : "!tvarAssign",
        ("!tvar", "!t<-", "!tnum") : "!tvarAssign",
        ("!tvar", "!t<-", "!tstring") : "!tvarAssign",
        ("!tvar", "!t<-", "!tboolVal") : "!tvarAssign", 

        ("!trun", "!tvar") : "!tfunCall",       

        ("!tvar", "!t::", "!t(", "!tcodeBlock", "!t)") : "!tfunAssign",

        ("!tboolVal", "!t?", "!t(", "!tcodeBlock", "!t)", "!t||", "!t(", "!tcodeBlock", "!t)") : "!tconditional",

        ("!tboolVal", "!t@", "!t(", "!tcodeBlock", "!t)") : "!tloop",

        ("!t(", "!tboolVal", "!tlogicOp", "!tboolVal", "!t)") : "!tboolVal",
        ("!t(", "!tboolVal", "!tlogicOp", "!tvar", "!t)") : "!tboolVal",
        ("!t(", "!tvar", "!tlogicOp", "!tboolVal", "!t)") : "!tboolVal",
        ("!t(", "!tvar", "!tlogicOp", "!tvar", "!t)") : "!tboolVal",

        ("!t(", "!tstring", "!tcmpOp", "!tstring", "!t)") : "!tboolVal",
        ("!t(", "!tstring", "!tcmpOp", "!tvar", "!t)") : "!tboolVal",
        ("!t(", "!tvar", "!tcmpOp", "!tstring", "!t)") : "!tboolVal",

        ("!t(", "!tnum", "!tcmpOp", "!tnum", "!t)") : "!tboolVal",
        ("!t(", "!tnum", "!tcmpOp", "!tvar", "!t)") : "!tboolVal",
        ("!t(", "!tvar", "!tcmpOp", "!tnum", "!t)") : "!tboolVal",

        ("!t(", "!tvar", "!tcmpOp", "!tvar", "!t)") : "!tboolVal",
        
        ("!t&",) : "!tlogicOp",
        ("!t|",) : "!tlogicOp",
        ("!txor",) : "!tlogicOp",

        ("!t<",) : "!tcmpOp",
        ("!t<=",) : "!tcmpOp",
        ("!t=",) : "!tcmpOp",
        ("!t~=",) : "!tcmpOp",
        ("!t>=",) : "!tcmpOp",
        ("!t>",) : "!tcmpOp",

        ("!t+",) : "!tarithOp",
        ("!t-",) : "!tarithOp",
        ("!t*",) : "!tarithOp",
        ("!t^",) : "!tarithOp",
        ("!t/",) : "!tarithOp",
        ("!tdiv",) : "!tarithOp",
        ("!tmod",) : "!tarithOp",

        ("!t(", "!tstring", "!t+", "!tstring", "!t)") : "!tstring",

        ("!t(", "!tnum", "!tarithOp", "!tnum", "!t)") : "!tnum",
        ("!t(", "!tvar", "!tarithOp", "!tnum", "!t)") : "!tnum",
        ("!t(", "!tnum", "!tarithOp", "!tvar", "!t)") : "!tnum",
        ("!t(", "!tvar", "!tarithOp", "!tvar", "!t)") : "!tnum",
    }
    tokens = {";", "(", ")", "pass", "run", "::", "?", "|", "&", "xor", "@", "||", "+", "-", "*", "/", "mod", "div", "^", "<", "<=", "=", ">=", ">", "<-", "~="}
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = "program.txt"
    if len(sys.argv) > 2:
        outfile = sys.argv[2]
    else:
        outfile = "run_this.py"
    langstring = get_langstring(filename)
    preprocessed = preprocess(langstring, tokens)
    partial_tokenized = tokenize1(preprocessed, tokens)
    tokenized = tokenize2(partial_tokenized, tokens)
    if len(sys.argv) > 4:
        TIMEOUT = float(sys.argv[4])
    parsed = parse(tokenized, grammar)
    print("Successfully Compiled")
    if len(sys.argv) > 3 and sys.argv[3] == "show_parse":
        print_parse_tree(parsed)
    if len(sys.argv) > 5:
        ARGS_ALLOWED = int(sys.argv[5])
    code = pre_order_linearize(parsed)
    # Modifies instructions to reflect functions correctly
    instructions = all_instructions(code)
    label_funcs(instructions)

    # Writes to the outfile
    with open(outfile, "w") as f:
        argstring = "["
        for _ in range(ARGS_ALLOWED):
            argstring += "None, "
        f.write("args = " + argstring.rstrip(", ") +"]\n")
        for ins in code:
            f.write(str(ins))
    print("Successfully written to file \"" + outfile + "\"")

### TRANSLATING ###

'''
Labels variables that should be functions as such
'''
def label_funcs(instructions):
    func_names = {"print", "input", "exit", "str", "int", "float"}
    for ins in instructions:
        if type(ins) == FunAssign:
            func_names.add(ins.name)
    for ins in instructions:
        if type(ins) == VarAssign:
            if ins.val in func_names:
                ins.func_val()
            if ins.var in func_names:
                ins.func_var()

'''
Gets a list of all the instructions with no nesting
'''
def all_instructions(code):
    sublists = {
        FunAssign : FunAssign.get_sublist, 
        Loop : Loop.get_sublist, 
        Conditional : Conditional.get_sublist
    }
    insructions = []
    for line in code:
        insructions.append(line)
        typ = type(line)
        if typ in sublists:
            insructions += all_instructions(sublists[typ](line))
    return insructions
                
'''
Creates a list of the defined code blocks with their arguments
'''
def pre_order_linearize(tree, indent=""):
    if tree[-1] == "!tvarAssign":
        return [VarAssign(tree[0][0], tree[0][1], "python", indent)]
    elif tree[-1] == "!tfunCall":
        return [FunCall(tree[0][1], "python", indent)]
    elif tree[-1] == "!tpass":
        return [Pass("python", indent)]
    elif tree[-1] == "!tfunAssign":
        return [FunAssign(tree[0][0], pre_order_linearize(tree[0][1], indent + "    "), "python", indent)]
    elif tree[-1] == "!tloop":
        return [Loop(tree[0][0], pre_order_linearize(tree[0][1], indent + "    "), "python", indent)]
    elif tree[-1] == "!tconditional":
        return [Conditional(tree[0][0], pre_order_linearize(tree[0][1], indent + "    "), pre_order_linearize(tree[0][2], indent + "    "), "python", indent)]
    elif tree[-1] == "!tcodeBlock":
        children = []
        for child in tree[0]:
            children += pre_order_linearize(child, indent)
        return children
    else:
        print(tree)
        exit()

### PARSING ###

'''
Reads a test language file into a string with no newlines
'''
def get_langstring(filename):
    with open(filename, "r") as f:
        filedata = f.readlines()

    langstring = ""
    for line in filedata:
        if line.strip() != "":
            langstring += line.strip() + " "

    langstring = langstring.strip()
    return langstring

'''
Removes comments from langstring
'''
def remove_comments(langstring):
    starti = 0
    endi = 0
    i = 0
    while i < len(langstring):
        select = langstring[i:i+2]
        if select == "#-":
            starti = i
        elif select == "-#":
            endi = i
            langstring = langstring[:starti] + " " + langstring[endi+2:]
            i -= endi - starti + 2
        i += 1
    return langstring

'''
Puts spaces between all tokens and converts the result to a list
'''
def preprocess(langstring, tokens):
    langstring = remove_comments(langstring)

    # classifies tokens by length
    classes = {}
    for token in tokens:
        l = len(token)
        if l in classes:
            classes[l].append(token)
        else:
            classes[l] = [token]
    
    # goes through the langstring and makes sure tokens aren't clumped
    iter = sorted(classes.keys())
    iter.reverse()
    for i in iter:
        string = False
        tok = False
        j = 0
        while j <= len(langstring) - i:
            select = langstring[j:j+i]
            if select[0] == "\"" and not string:
                string = True
            elif select[0] == "\"" and string:
                string = False

            if select[0] == "%" and not string and not tok:
                tok = True
            elif select[0] == "%" and not string and tok:
                tok = False

            active = not (string or tok)
            if select in classes[i] and active:
                prefix = langstring[:j]
                suffix = langstring[j+i:]
                langstring = prefix + " %" + select + "% " + suffix
                j += i+3
            j += 1
    
    langstring = langstring.replace("%", "")
    return langstring.split()

'''
Prepares a string of the test language for tokenization by marking literals and text tokens
'''
def tokenize1(langstring, tokens):
    string = False
    for i in range(len(langstring)):
        word = langstring[i]
        if word == "":
            continue
        
        # marks beginning and ending of strings
        if word[0] == "\"" and word[-1] == "\"":
            langstring[i] = "strStartstrEnd" + word
        elif word[0] == "\"":
            string = True
            langstring[i] = "strStart" + word
        elif word[-1] == "\"":
            string = False
            langstring[i] = "strEnd" + word
        elif string:
            continue
        
        # tokenizes text tokens
        elif word in tokens:
            langstring[i] = "!t" + word

        # marks literals
        elif word.isnumeric():
            langstring[i] = "isnum" + word
        
        elif word == "true" or word == "false":
            langstring[i] = "isbool" + word
        
        else:
            langstring[i] = "isvar" + word

    return langstring

'''
Takes a preprocessed langstring and tokenizes it in preparation for parsing
Text terminals are nested in loops in parsing, this sets up the bases
'''
def tokenize2(partially_tokenized, tokens):
    # markers for tokenization
    v = "isvar"
    n = "isnum"
    b = "isbool"
    ss = "strStart"
    se = "strEnd"
    sb = "strStartstrEnd"

    tokenized = []
    readstring = False
    for pt in partially_tokenized:
        if pt == "":
            continue
        
        # tokenizes strings
        if len(pt) > len(se) and pt[:len(se)] == se:
            readstring = False
            tokenized[-1][0][0] += (" " + pt[len(se):].rstrip("\""))
        elif readstring:
            tokenized[-1][0][0] += (" " + pt)
        elif len(pt) > len(sb) and pt[:len(sb)] == sb:
            tokenized.append([[pt[len(sb):].strip("\"")], "!tstring"])
        elif len(pt) > len(ss) and pt[:len(ss)] == ss:
            readstring = True
            tokenized.append([[pt[len(ss):].lstrip("\"")], "!tstring"])
        
        # tokenizes literals other than strings
        elif len(pt) > len(v) and pt[:len(v)] == v:
            tokenized.append([[pt[len(v):]], "!tvar"])
        elif len(pt) > len(n) and pt[:len(n)] == n:
            tokenized.append([[pt[len(n):]], "!tnum"])
        elif len(pt) > len(b) and pt[:len(b)] == b:
            tokenized.append([[pt[len(b):]], "!tboolVal"])
        
        else:
            if pt.lstrip("!t") in tokens:
                tokenized.append([pt])
            else:
                print("Syntax Error: NOT A TOKEN AT: " + pt)
                exit()

    return tokenized

'''
Prints a vertical parse tree for debugging
'''
def print_parse_tree(l, space=""):
    if len(l) == 1:
        print(space + l[0].lstrip("!t"))
        return
    elif type(l) == str:
        print(space + l.lstrip("!t"))
        return
    
    print(space + "<" + l[-1].lstrip("!t") + ">")    
    for e in l[0]:
        print_parse_tree(e, space + "  ")

'''
Prints given level of tree for debugging
'''
def print_level(l, depth):
    if depth == 0:
        print(l[-1], end=" ")
    else:
        for sl in l[0]:
            print_level(sl, depth - 1)

'''
Recurses through parens to concatenate strings
'''
def concat_strings(strings, offset):
    if strings[1][-1] == "!tstring":
        return [[strings[1][0][-1] + strings[3][0][-1]], "!tstring"]
    try:
        retval = [[concat_strings(strings[1:], offset-4)[0][-1] + strings[7+offset][0][-1]], "!tstring"]
    except IndexError:
        print("Syntax Error: ILLEGAL PARENTHESES")
        exit()
    return retval

'''
Isolates string that needs to be concatenated
'''
def get_strings(tokenized_tail):
    open_p = 0
    l = 0
    started = False
    for token in tokenized_tail:
        if token[-1] == "!t(":
            open_p += 1
            started = True
        elif token[-1] == "!t)":
            open_p -=1
        elif token[-1] == "!t+" or token[-1] == "!tstring":
            pass
        else:
            break
        l += 1
        if started and open_p == 0:
            return tokenized_tail[:l], l
    return "", 0

'''
Consolidates all strings that need to be concatenated
'''
def strings(tokenized, _):
    i = 0
    while i < len(tokenized):
        if tokenized[i][-1] == "!t(":
            potential, l = get_strings(tokenized[i:])
            if potential != "":
                insert = concat_strings(potential, l-9)
                tokenized = tokenized[:i] + [insert] + tokenized[i+l:]
        i += 1
    return tokenized

'''
Further tokenizes ops
'''
def ops(tokenized, grammar):
    opnames = {
        ("!t&",) : "!tand",
        ("!t|",) : "!tor",
        ("!txor",) : "!txor",

        ("!t<",) : "!tless",
        ("!t<=",) : "!tlessEq",
        ("!t=",) : "!teq",
        ("!t~=",) : "!tnotEq",
        ("!t>=",) : "!tgrtrEq",
        ("!t>",) : "!tgrtr",

        ("!t+",) : "!tadd",
        ("!t-",) : "!tsub",
        ("!t*",) : "!tmult",
        ("!t^",) : "!tpow",
        ("!t/",) : "!tfdiv",
        ("!tdiv",) : "!tidiv",
        ("!tmod",) : "!tmod",
    }
    i = 0
    for i in range(len(tokenized)):
        key = tuple(tokenized[i])
        try:
            if key in grammar:
                tokenized[i] = [[opnames[key]], grammar[key]]
        except TypeError:
            pass
        i += 1
    return tokenized

'''
Reduces all tokens given from expressions of length length
'''
def reduce(tokenized, grammar, expr, length, indices, debug=False):
    i = 0
    while i <= len(tokenized) - length:
        cur = []
        for j in range(length):
            cur.append(tokenized[i+j][-1])
        if debug:
            print(cur)
            inp = input()
            if inp.strip() == "nodeb":
                debug = False
        key = tuple(cur)
        if key in grammar and grammar[key] == expr:
            sliced = []
            for ind in indices:
                tok = tokenized[i+ind]
                if tok != ["!t;"]:
                    sliced.append(tok)
            tokenized = tokenized[:i] + [[sliced, expr]] + tokenized[i+length:]
            i -= 1
        i += 1
    return tokenized

'''
Checks to see if the given token is currently in the top level of the tree
'''
def check_for(tokens, tokenized, joined=False):
    l = len(tokens)
    for i in range(len(tokenized) - (l if joined else 0)):
        if joined:
            check = []
            for j in range(l):
                check.append(tokenized[i+j][-1])
            if check == tokens:
                return True
        else:
            check = tokenized[i][-1]
            if check in tokens:
                return True
    return False

'''
Reduces all reducible non-codeBlock literal items
'''
def literals(tokenized, grammar, entrytime=time.time()):
    step_one = strings(tokenized, grammar)
    step_two = ops(step_one, grammar)
    step_three = step_two
    while check_for(["!tarithOp"], step_three):
        if (time.time() - entrytime) >= TIMEOUT:
            print("Syntax Error: INCORRECT SYNTAX IN ARITHMETIC EXPRESSION")
            exit()
        step_three = reduce(step_three, grammar, "!tnum", 5, [1,2,3])
    step_four = step_three
    while check_for(["!tcmpOp", "!tlogicOp"], step_four):
        if (time.time() - entrytime) >= TIMEOUT:
            print("Syntax Error: INCORRECT SYNTAX IN LOGICAL EXPRESSION")
            exit()
        step_four = reduce(step_four, grammar, "!tboolVal", 5, [1,2,3])
    return step_four

'''
Reduces all non-recursive code blocks
'''
def simple_cbs(tokenized, grammar, entrytime=time.time()):
    step_one = reduce(tokenized, grammar, "!tfunCall", 2, [0,1])
    step_two = reduce(step_one, grammar, "!tvarAssign", 3, [0,2])
    step_three = step_two
    while check_for(["!tvarAssign", "!tfunCall"], step_three) or check_for(["!tcodeBlock", "!tcodeBlock"], step_three, True):
        if (time.time() - entrytime) >= TIMEOUT:
            print("Syntax Error: MISSING SEMICOLON")
            exit()
        step_three = reduce(step_three, grammar, "!tcodeBlock", 2, [0,1])
    return step_three

'''
Gets the base level of a nested list
'''
def base(l):
    if type(l[0]) == str:
        return l[0].lstrip("!t")
    return base(l[0])

'''
Gets the string of the non-codeblock that is causing the error
'''
def get_bad_string(tokenized):
    bad_string = ""
    start = False
    prev = True
    need_and = False
    for token in tokenized:
        if token[-1] == "!tcodeBlock":
            if start and prev:
                need_and = True
            prev = False
        else:
            if need_and:
                bad_string += ", "
                need_and = False
            bad_string += base(token)
            prev = True
            start = True
    return bad_string

'''
Recurses into parentheses to make code blocks fit into parent code blocks
'''
def nested_cbs(tokenized, grammar, entrytime=time.time()):
    global TIMEOUT
    step_four = tokenized
    while len(step_four) > 1:
        if (time.time() - entrytime) >= TIMEOUT:
            print("Syntax Error: INCORRECT SYNTAX AT: " + get_bad_string(step_four))
            exit()
        step_one = reduce(step_four, grammar, "!tfunAssign", 5, [0,3])
        step_two = reduce(step_one, grammar, "!tconditional", 9, [0,3,7])
        step_three = reduce(step_two, grammar, "!tloop", 5, [0,3])
        step_four = reduce(step_three, grammar, "!tcodeBlock", 2, [0,1])
    return step_four

'''
Creates a parse tree using the provided grammar
'''
def parse(tokenized, grammar):
    step_one = literals(tokenized, grammar)
    step_two = simple_cbs(step_one, grammar)
    step_three = nested_cbs(step_two, grammar)
    return step_three[0]

if __name__ == "__main__":
    main()