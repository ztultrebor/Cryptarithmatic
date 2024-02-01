# import string     # not clear if this is necessary
import re, itertools



#=======================
# functions


def solve(expression):
    """String -> [ListOf String]
       Given a formula like 'ODD + ODD == EVEN', fill in digits to solve it. 
       Returns valid solutions"""
    f, variables = compile_formula(expression)
    solutions = []
    for args in itertools.permutations([0,1,2,3,4,5,6,7,8,9], len(variables)):
        if f(*args):
            table = str.maketrans(variables, ''.join(map(str, args)))
            substexpression = expression.translate(table)
            if not(re.search(r'\b0[0-9]', substexpression)):
                solutions.append(substexpression)
    return solutions
        

def compile_formula(expression):
    """String -> [*N -> Bool] String
       takes an expression, extracts its variables and produces a
       lambda function, as well as a string that corresponds to 
       variables extracted from the the expression . This function 
       need be evaluated only once, thereby saving great amount of compute"""
    variables = ''.join(set(re.findall(r'[A-Z]', expression)))
    params = ','.join(variables)
    words = (re.split(r'([A-Z]+)', expression))
    body = ''.join([compile_word(w) for w in words])
    return eval('lambda %s: %s' % (params, body)), variables


def compile_word(word):
    """String -> String
       Compile a word of uppercase letters as numeric digits.
       E.g., compile_word('YOU') => '(1*U+10*O+100*Y)'
       Non-uppercase words unchanged: compile_word('+') => '+'"""
    if re.match(r'[A-Z]+', word):
        return '(' + '+'.join((str(10**(len(word)-p-1))) + '*' + l 
                              for (p,l) in enumerate(word)) + ')'
    else:
        return word


def display(solutions):
    """[ListOf String] -> None
       Prints out each valid solution on a new line"""
    print("")
    for s in solutions:
        print(s)
    print("")



#=======================
# actions!

display(solve('ODD + ODD == EVEN'))
display(solve('ATOM**(1/2) == A + TO + M'))
display(solve('ATOM**(1/2) == AT + O + M'))
display(solve('ATOM**(1/2) == AT + OM'))
display(solve('A**N + B**N == C**N and N > 1'))
display(solve('A**N + IB**N == IC**N and N > 1'))
display(solve('A**N + B**N + C**N == D**N and N > 1'))
display(solve('A**N + IB**N == I**N + ID**N == IXDA and N > 1'))