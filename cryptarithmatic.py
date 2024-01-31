# import string     # not clear if this is necessary
import re


def valid(expression, trans_table):
    """ String {N:N} -> Maybe String
    returns the expression with digits substituted for variables in the case that 
    it forms a true 
    and valid mathematical expression"""
    subsexpression = expression.translate(trans_table)
    try:
         if not(re.search(r'\b0[0-9]', subsexpression)) and eval(subsexpression):
             return subsexpression
    except ArithmeticError:
        return False
        


table = str.maketrans('ABCD', '1230')
f = 'A + B - C == D'
print(table)
print(valid(f))