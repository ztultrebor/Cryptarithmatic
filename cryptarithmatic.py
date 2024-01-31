# import string     # not clear if this is necessary


def valid(expression):
    subsexpession = expression.translate(table)
    try:
         if eval(subsexpession):
             return subsexpession
    except:
        ArithmeticError
        


table = str.maketrans('ABC', '123')
f = 'A + B == C'
print(valid(f))