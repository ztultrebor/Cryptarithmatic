# import string     # not clear if this is necessary
import re, itertools

def solve(formula):
    """Given a formula like 'ODD + ODD == EVEN', fill in digits to solve it.
    Input formula is a string; output is a digit-filled-in string or None."""
    for f in next(fill_in(formula)):
        if valid(f):
            print(f)
    return None
        

def fill_in(formula):
    """Generate all possible fillings-in of letters in formula with digits."""
    variables = ''.join(re.findall(r'[A-Z]', formula))
    yield (formula.translate(
            str.maketrans(variables, ''.join(perm)))
                for perm in 
                    itertools.permutations('0123456789', len(variables)))


def valid(expression):
    """ String -> Maybe String
    returns the expression with digits substituted for variables in the case that 
    it forms a true 
    and valid mathematical expression"""
    try:
         if not(re.search(r'\b0[0-9]', expression)) and eval(expression):
             return expression
    except ArithmeticError:
        return False
        


table = str.maketrans('ABCD', '1230')
f = 'ODD + ODD == EVEN'

#print(solve(f))
print(solve(f))
print(solve('A + B == C'))